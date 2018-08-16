{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Iconv (iconvTestGroup) where

import qualified Data.ByteString.Lazy.Encoding as Enc
import qualified Data.ByteString.Lazy.Encoding.Internal as Enc
import qualified Data.ByteString.Lazy as BL

import qualified Test.QuickCheck.Monadic as QM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{-
XXX: GHC.IO.Encoding.BufferCodec does not provide an API to reset
state at the end of the string.
-}

prop_encode_decode_iso2022jp :: Property
prop_encode_decode_iso2022jp = QM.monadicIO $ do
  enc <- QM.run $ Enc.mkTextEncoding "iso-2022-jp"
  inBufSizeE  <- QM.pick $ choose (2,16)
  outBufSizeE <- QM.pick $ choose (5,32)
  inBufSizeD  <- QM.pick $ choose (4,32)
  outBufSizeD <- QM.pick $ choose (2,16)
  let s = "\12371\12435\12395\12385\12399ABC\19990\30028\n" -- "こんにちはABC世界\n"
  QM.assert $ Enc.decodeWith enc inBufSizeD outBufSizeD (Enc.encodeWith enc inBufSizeE outBufSizeE s) == s

case_decode_iso2022jp :: Assertion
case_decode_iso2022jp = do
  enc <- Enc.mkTextEncoding "iso-2022-jp"
  Enc.decode enc b1 @?= s
  Enc.decode enc b2 @?= s
  where
    s = "\12371\12435\12395\12385\12399\19990\30028\n" -- "こんにちは世界\n"
    b1 = BL.pack [0x1b,0x24,0x40,0x24,0x33,0x24,0x73,0x24,0x4b,0x24,0x41,0x24,0x4f,0x40,0x24,0x33,0x26,0x1b,0x28,0x42,0x0a]
    b2 = BL.pack [0x1b,0x24,0x42,0x24,0x33,0x24,0x73,0x24,0x4b,0x24,0x41,0x24,0x4f,0x40,0x24,0x33,0x26,0x1b,0x28,0x42,0x0a]

prop_encode_decode_shiftjis :: Property
prop_encode_decode_shiftjis = QM.monadicIO $ do
  enc <- QM.run $ Enc.mkTextEncoding "shift_jis"
  inBufSizeE  <- QM.pick $ choose (2,16)
  outBufSizeE <- QM.pick $ choose (2,16)
  inBufSizeD  <- QM.pick $ choose (2,16)
  outBufSizeD <- QM.pick $ choose (2,16)
  let s = "\12371\12435\12395\12385\12399ABC\19990\30028\65402\65437\65414\65409\65436\n" -- "こんにちはABC世界ｺﾝﾆﾁﾜ\n"
  QM.assert $ Enc.decodeWith enc inBufSizeD outBufSizeD (Enc.encodeWith enc inBufSizeE outBufSizeE s) == s

prop_encode_decode_eucjp :: Property
prop_encode_decode_eucjp = QM.monadicIO $ do
  enc <- QM.run $ Enc.mkTextEncoding "euc-jp"
  inBufSizeE  <- QM.pick $ choose (2,16)
  outBufSizeE <- QM.pick $ choose (3,16)
  inBufSizeD  <- QM.pick $ choose (3,16)
  outBufSizeD <- QM.pick $ choose (2,16)
  let s = "\12371\12435\12395\12385\12399ABC\19990\30028\65402\65437\65414\65409\65436\39439\39502\n" -- "こんにちはABC世界ｺﾝﾆﾁﾜ騏驎\n"
  QM.assert $ Enc.decodeWith enc inBufSizeD outBufSizeD (Enc.encodeWith enc inBufSizeE outBufSizeE s) == s

iconvTestGroup :: TestTree
iconvTestGroup = 
  testGroup "Iconv"
  [ testProperty "encode decode iso2022jp" prop_encode_decode_iso2022jp
  , testCase "decode iso2022jp" case_decode_iso2022jp
  , testProperty "encode decode shiftjis" prop_encode_decode_shiftjis
  , testProperty "encode decode eucjp" prop_encode_decode_eucjp
  ]
