{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Encoding as Enc
import qualified Data.ByteString.Encoding.Internal as Enc
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Test.Tasty.QuickCheck
import Test.Tasty.TH

checkEncode :: Enc.TextEncoding -> (TL.Text -> BL.ByteString) -> Property
checkEncode enc f =
  forAll arbitrary $ \(s::String) ->
  forAll (choose (2,16)) $ \inBufSize ->
  forAll (choose (4,32)) $ \outBufSize ->
    let s' = TL.pack s
    in Enc.encodeWith enc inBufSize outBufSize s' == f s'

checkRoundTrip :: Enc.TextEncoding -> Property
checkRoundTrip enc = 
  forAll arbitrary $ \(s::String) ->
  forAll (choose (2,16)) $ \inBufSizeE ->
  forAll (choose (4,32)) $ \outBufSizeE ->
  forAll (choose (4,32)) $ \inBufSizeD ->
  forAll (choose (2,16)) $ \outBufSizeD ->
    let s' = TL.pack s
    in (Enc.decodeWith enc inBufSizeD outBufSizeD $ Enc.encodeWith enc inBufSizeE outBufSizeE $ s') == s'

prop_encode_utf8 :: Property
prop_encode_utf8 = checkEncode Enc.utf8 TL.encodeUtf8

prop_encode_utf16le :: Property
prop_encode_utf16le = checkEncode Enc.utf16le TL.encodeUtf16LE

prop_encode_utf16be :: Property
prop_encode_utf16be = checkEncode Enc.utf16be TL.encodeUtf16BE

prop_encode_utf32le :: Property
prop_encode_utf32le = checkEncode Enc.utf32le TL.encodeUtf32LE

prop_encode_utf32be :: Property
prop_encode_utf32be = checkEncode Enc.utf32be TL.encodeUtf32BE

prop_encode_decode_utf8 :: Property
prop_encode_decode_utf8 = checkRoundTrip Enc.utf8

prop_encode_decode_utf16le :: Property
prop_encode_decode_utf16le = checkRoundTrip Enc.utf16le

prop_encode_decode_utf16be :: Property
prop_encode_decode_utf16be = checkRoundTrip Enc.utf16be

prop_encode_decode_utf32le :: Property
prop_encode_decode_utf32le = checkRoundTrip Enc.utf32le

prop_encode_decode_utf32be :: Property
prop_encode_decode_utf32be = checkRoundTrip Enc.utf32be

-- ---------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
