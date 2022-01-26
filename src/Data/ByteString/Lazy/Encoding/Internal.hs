{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ByteString.Lazy.Encoding.Internal
  ( encode
  , encodeWith
  , decode
  , decodeWith
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Foreign
import qualified Foreign.Concurrent as Conc
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Ptr (nullPtr)
import qualified GHC.IO.Encoding as Enc
import GHC.IO.Buffer
import System.IO.Unsafe

-- | Encode a lazy 'TL.Text' into a lazy 'BL.ByteString' using a given 'Enc.TextEncoding'.
encode :: Enc.TextEncoding -> TL.Text -> BL.ByteString
encode enc = encodeWith enc 1024 1024

encodeWith :: Enc.TextEncoding -> Int -> Int -> TL.Text -> BL.ByteString
encodeWith enc inBufSize outBufSize = encodeStringWith enc inBufSize outBufSize . TL.unpack

encodeStringWith :: Enc.TextEncoding -> Int -> Int -> String -> BL.ByteString
encodeStringWith Enc.TextEncoding{ .. } inBufSize outBufSize s = BL.fromChunks $ unsafePerformIO $ do
  Enc.BufferCodec{ .. } <- mkTextEncoder
  fp <- Conc.newForeignPtr nullPtr close

  let fillInBuf :: String -> CharBuffer -> IO (String, CharBuffer)
      fillInBuf s buf
        | isEmptyBuffer buf = go s buf{ bufL=0, bufR=0 }
        | otherwise = go s buf
        where
          go :: String -> CharBuffer -> IO (String, CharBuffer)
          go [] buf = return ([], buf)
          go s@(c : cs) buf@Buffer{ bufR = r, bufRaw = iraw}
            | isFullCharBuffer buf = return (s, buf)
            | otherwise = do
                r' <- writeCharBuf iraw r c
                go cs buf{ bufR = r' }

      flushOutBuf :: Buffer Word8 -> IO ([B.ByteString], Buffer Word8)
      flushOutBuf buf
        | isEmptyBuffer buf = return ([], buf{ bufL=0, bufR=0 })
        | otherwise = do
            withBuffer buf $ \p -> do
              b <- B.packCStringLen (castPtr p, bufferElems buf)
              return ([b], buf{ bufL=0, bufR=0 })

      loop :: String -> CharBuffer -> Buffer Word8 -> IO [B.ByteString]
      loop s inBuf outBuf = do
        (s', inBuf1) <- fillInBuf s inBuf
        if isEmptyBuffer inBuf1 then do
          assert (null s') $ return ()
          (m, _outBuf') <- flushOutBuf outBuf
          touchForeignPtr fp
          return m
        else do
          (ret, inBuf2, outBuf2) <- encode inBuf1 outBuf
          case ret of
            Enc.InputUnderflow -> do
              if isFullCharBuffer inBuf2 && not (isEmptyBuffer inBuf2) then do
                withRawBuffer (bufRaw inBuf2) $ \p -> do
                  moveArray p (p `plusPtr` bufL inBuf2) (bufferElems inBuf2)
                loop s' inBuf2{ bufL = 0, bufR = bufferElems inBuf2 } outBuf2
              else do
                loop s' inBuf2 outBuf2
            Enc.OutputUnderflow -> do
              (b, outBuf3) <- flushOutBuf outBuf2
              bs <- unsafeInterleaveIO $ loop s' inBuf2 outBuf3
              return $ b ++ bs
            Enc.InvalidSequence -> do
              -- recover assumes that to buffer has at least one element of free space.
              if isFullBuffer outBuf2 then do
                (b, outBuf3) <- flushOutBuf outBuf2
                (inBuf4, outBuf4) <- recover inBuf2 outBuf3
                bs <- unsafeInterleaveIO $ loop s' inBuf4 outBuf4
                return $ b ++ bs
              else do
                (inBuf3, outBuf3) <- recover inBuf2 outBuf2
                loop s' inBuf3 outBuf3

  inBuf <- newCharBuffer inBufSize ReadBuffer
  outBuf <- newByteBuffer outBufSize WriteBuffer
  loop s inBuf outBuf


-- | Decode a lazy 'BL.ByteString' to a lazy 'TL.Text' using a given 'Enc.TextEncoding'.
decode :: Enc.TextEncoding -> BL.ByteString -> TL.Text
decode enc b = decodeWith enc 1024 1024 b

decodeWith :: Enc.TextEncoding -> Int -> Int -> BL.ByteString -> TL.Text
decodeWith Enc.TextEncoding{ .. } inBufSize outBufSize b = TL.fromChunks $ unsafePerformIO $ do
  Enc.BufferCodec{ .. } <- mkTextDecoder
  fp <- Conc.newForeignPtr nullPtr close

  let fillInBuf :: [B.ByteString] -> Buffer Word8 -> IO ([B.ByteString], Buffer Word8)
      fillInBuf bs buf
        | isEmptyBuffer buf = go bs buf{ bufL=0, bufR=0 }
        | otherwise = go bs buf
        where
          go :: [B.ByteString] -> Buffer Word8 -> IO ([B.ByteString], Buffer Word8)
          go [] buf = return ([], buf)
          go bbs@(b : bs) buf
            | isFullBuffer buf = return (bbs, buf)
            | B.null b = go bs buf
            | otherwise = do
                B.unsafeUseAsCString b $ \p -> do
                  withBuffer buf $ \q -> do
                    if B.length b <= bufferAvailable buf then do
                      moveBytes (q `plusPtr` bufR buf) p (B.length b)
                      go bs buf{ bufR = bufR buf + B.length b }
                    else do
                      moveBytes (q `plusPtr` bufR buf) p (bufferAvailable buf)
                      go (B.drop (bufferAvailable buf) b : bs) buf{ bufR = bufR buf + bufferAvailable buf }

#if MIN_VERSION_text(2,0,0)
      flushOutBuf :: CharBuffer -> ForeignPtr Word8 -> IO ([T.Text], CharBuffer)
      flushOutBuf buf workspace
        | isEmptyBuffer buf = return ([], buf{ bufL=0, bufR=0 })
        | otherwise =
            withForeignPtr workspace $ \workspace' ->
            withBuffer buf $ \p -> do
              let f !i !j
                    | bufR buf <= i = return j
                    | otherwise = do
                        (c, i') <- readCharBufPtr (castPtr p) i
                        j' <- writeUTF8 workspace' j c
                        f i' j'
              n <- f (bufL buf) 0
              t <- T.fromPtr workspace' (fromIntegral n)
              return ([t], buf{ bufL=0, bufR=0 })
#else
      flushOutBuf :: CharBuffer -> ForeignPtr Word16 -> IO ([T.Text], CharBuffer)
      flushOutBuf buf workspace
        | isEmptyBuffer buf = return ([], buf{ bufL=0, bufR=0 })
        | charSize==2 = do
            withBuffer buf $ \p -> do
              let p' :: Ptr Word16
                  p' = castPtr p
              t <- T.fromPtr (p' `plusPtr` bufL buf) (fromIntegral (bufferElems buf))
              return ([t], buf{ bufL=0, bufR=0 })
        | otherwise =
            withForeignPtr workspace $ \workspace' ->
            withBuffer buf $ \p -> do
              let p' :: Ptr Char
                  p' = castPtr p
                  f !i !j
                    | bufR buf <= i = return j
                    | otherwise = do
                        c <- liftM fromEnum $ peekElemOff p' i
                        if c < 0x10000 then do
                          pokeElemOff workspace' j (fromIntegral c)
                          f (i+1) (j+1)
                        else do
                          let c' = c - 0x10000
                          pokeElemOff workspace' j (fromIntegral (c' `div` 0x400 + 0xd800))
                          pokeElemOff workspace' (j+1) (fromIntegral (c' `mod` 0x400 + 0xdc00))
                          f (i+1) (j+2)
              n <- f (bufL buf) 0
              t <- T.fromPtr workspace' (fromIntegral n)
              return ([t], buf{ bufL=0, bufR=0 })
#endif

#if MIN_VERSION_text(2,0,0)
      loop :: [B.ByteString] -> Buffer Word8 -> CharBuffer -> ForeignPtr Word8 -> IO [T.Text]
#else
      loop :: [B.ByteString] -> Buffer Word8 -> CharBuffer -> ForeignPtr Word16 -> IO [T.Text]
#endif
      loop bs inBuf outBuf workspace = do
        (bs', inBuf1) <- fillInBuf bs inBuf
        if isEmptyBuffer inBuf1 then do
          assert (null bs') $ return ()
          (m, _outBuf') <- flushOutBuf outBuf workspace
          touchForeignPtr fp
          return m
        else do
          (ret, inBuf2, outBuf2) <- encode inBuf1 outBuf
          case ret of
            Enc.InputUnderflow -> do
              if isFullBuffer inBuf2 && not (isEmptyBuffer inBuf2) then do
                inBuf3 <- slideContents inBuf2
                loop bs' inBuf3 outBuf2 workspace
              else do
                loop bs' inBuf2 outBuf2 workspace
            Enc.OutputUnderflow -> do
              (t, outBuf3) <- flushOutBuf outBuf2 workspace
              ts <- unsafeInterleaveIO $ loop bs' inBuf2 outBuf3 workspace
              return $ t ++ ts
            Enc.InvalidSequence -> do
              -- recover assumes that to buffer has at least one element of free space.
              if isFullCharBuffer outBuf2 then do
                (t, outBuf3) <- flushOutBuf outBuf2 workspace
                (inBuf4, outBuf4) <- recover inBuf2 outBuf3
                ts <- unsafeInterleaveIO $ loop bs' inBuf4 outBuf4 workspace
                return $ t ++ ts
              else do
                (inBuf3, outBuf3) <- recover inBuf2 outBuf2
                loop bs' inBuf3 outBuf3 workspace

  inBuf <- newByteBuffer inBufSize ReadBuffer
  outBuf <- newCharBuffer outBufSize WriteBuffer
#if MIN_VERSION_text(2,0,0)
  workspace <- mallocForeignPtrArray (outBufSize * 4)
#else
  workspace <- if charSize == 2 then newForeignPtr_ nullPtr else mallocForeignPtrArray (outBufSize * 2)
#endif
  loop (BL.toChunks b) inBuf outBuf workspace


#if MIN_VERSION_text(2,0,0)

writeUTF8 :: Ptr Word8 -> Int -> Char -> IO Int
writeUTF8 p i c = do
  let x = ord c
  if x <= 0x7F then do
    pokeElemOff p i (fromIntegral x)
    return $! i+1
  else if x <= 0x07FF then do
    let (c1,c2) = ord2 c
    pokeElemOff p i c1
    pokeElemOff p (i+1) c2
    return $! i+2
  else if x <= 0xFFFF then do
    let (c1,c2,c3) = ord3 c
    pokeElemOff p i c1
    pokeElemOff p (i+1) c2
    pokeElemOff p (i+2) c3
    return $! i+3
  else do
    let (c1,c2,c3,c4) = ord4 c
    pokeElemOff p i c1
    pokeElemOff p (i+1) c2
    pokeElemOff p (i+2) c3
    pokeElemOff p (i+3) c4
    return $! i+4

-- -----------------------------------------------------------------------------
-- UTF-8 primitives, lifted from Data.Text.Fusion.Utf8

ord2   :: Char -> (Word8,Word8)
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (x1,x2)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
      x2 = fromIntegral $ (n .&. 0x3F)   + 0x80

ord3   :: Char -> (Word8,Word8,Word8)
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (x1,x2,x3)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
      x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = fromIntegral $ (n .&. 0x3F) + 0x80

ord4   :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = assert (n >= 0x10000) (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
      x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = fromIntegral $ (n .&. 0x3F) + 0x80

#endif
