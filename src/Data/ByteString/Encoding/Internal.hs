{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ByteString.Encoding.Internal
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
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Foreign
import qualified GHC.IO.Encoding as Enc
import GHC.IO.Buffer
import System.IO.Unsafe


encode :: Enc.TextEncoding -> TL.Text -> BL.ByteString
encode enc = encodeWith enc 1024 1024

encodeWith :: Enc.TextEncoding -> Int -> Int -> TL.Text -> BL.ByteString
encodeWith enc inBufSize outBufSize = encodeStringWith enc inBufSize outBufSize . TL.unpack

encodeStringWith :: Enc.TextEncoding -> Int -> Int -> String -> BL.ByteString
encodeStringWith Enc.TextEncoding{ .. } inBufSize outBufSize s = BL.fromChunks $ unsafePerformIO $ do
  Enc.BufferCodec{ .. } <- mkTextEncoder

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
          close
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


decode :: Enc.TextEncoding -> BL.ByteString -> TL.Text
decode enc b = decodeWith enc 1024 1024 b

decodeWith :: Enc.TextEncoding -> Int -> Int -> BL.ByteString -> TL.Text
decodeWith Enc.TextEncoding{ .. } inBufSize outBufSize b = TL.fromChunks $ unsafePerformIO $ do
  Enc.BufferCodec{ .. } <- mkTextDecoder

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

      flushOutBuf :: CharBuffer -> Ptr Word16 -> IO ([T.Text], CharBuffer)
      flushOutBuf buf workspace
        | isEmptyBuffer buf = return ([], buf{ bufL=0, bufR=0 })
        | charSize==2 = do
            withBuffer buf $ \p -> do
              let p' :: Ptr Word16
                  p' = castPtr p
              t <- T.fromPtr (p' `plusPtr` bufL buf) (fromIntegral (bufferElems buf))
              return ([t], buf{ bufL=0, bufR=0 })
        | otherwise = do
            withBuffer buf $ \p -> do
              let p' :: Ptr Char
                  p' = castPtr p
                  f !i !j
                    | bufR buf <= i = return j
                    | otherwise = do
                        c <- liftM fromEnum $ peekElemOff p' i
                        if c < 0x10000 then do
                          pokeElemOff workspace j (fromIntegral c)
                          f (i+1) (j+1)
                        else do
                          let c' = c - 0x10000
                          pokeElemOff workspace j (fromIntegral (c' `div` 0x400 + 0xd800))
                          pokeElemOff workspace (j+1) (fromIntegral (c' `mod` 0x400 + 0xdc00))
                          f (i+1) (j+2)
              n <- f (bufL buf) 0
              t <- T.fromPtr workspace (fromIntegral n)
              return ([t], buf{ bufL=0, bufR=0 })

      loop :: [B.ByteString] -> Buffer Word8 -> CharBuffer -> Ptr Word16 -> IO [T.Text]
      loop bs inBuf outBuf workspace = do
        (bs', inBuf1) <- fillInBuf bs inBuf
        if isEmptyBuffer inBuf1 then do
          assert (null bs') $ return ()
          (m, _outBuf') <- flushOutBuf outBuf workspace
          close
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
  if charSize == 2 then do
    loop (BL.toChunks b) inBuf outBuf nullPtr
  else do
    allocaArray (outBufSize * 2) $ \workspace ->
      loop (BL.toChunks b) inBuf outBuf workspace
