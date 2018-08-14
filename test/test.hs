{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Encoding.Internal as Enc
import qualified Data.Text.Lazy as TL
import System.IO (utf8)

import Test.Tasty.QuickCheck
import Test.Tasty.TH

prop_encode_utf8 :: Property
prop_encode_utf8 =
  forAll arbitrary $ \(s::String) ->
  forAll (choose (16,64)) $ \a ->
  forAll (choose (16,64)) $ \b ->
    let s' = TL.pack s
    in Enc.encodeWith utf8 a b s' == Builder.toLazyByteString (Builder.stringUtf8 s)

prop_encode_decode_utf8 :: Property
prop_encode_decode_utf8 =
  forAll arbitrary $ \(s::String) ->
  forAll (choose (16,64)) $ \a ->
  forAll (choose (16,64)) $ \b ->
  forAll (choose (16,64)) $ \c ->
  forAll (choose (16,64)) $ \d ->
    let s' = TL.pack s
    in (Enc.decodeWith utf8 a b $ Enc.encodeWith utf8 c d $ s') == s'

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
