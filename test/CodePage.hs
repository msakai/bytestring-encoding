{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module CodePage (codepageTestGroup) where

import qualified Data.ByteString.Lazy.Encoding as Enc
import qualified Data.ByteString.Lazy.Encoding.Internal as Enc
import qualified Data.ByteString.Lazy as BL

import qualified Test.QuickCheck.Monadic as QM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

prop_encode_decode_cp932 :: Property
prop_encode_decode_cp932 = QM.monadicIO $ do
  enc <- QM.run $ Enc.mkTextEncoding "CP932"
  inBufSizeE  <- QM.pick $ choose (2,16)
  outBufSizeE <- QM.pick $ choose (2,16)
  inBufSizeD  <- QM.pick $ choose (2,16)
  outBufSizeD <- QM.pick $ choose (2,16)
  let s = "\12371\12435\12395\12385\12399ABC\19990\30028\65402\65437\65414\65409\65436\n" -- "こんにちはABC世界ｺﾝﾆﾁﾜ\n"
  QM.assert $ Enc.decodeWith enc inBufSizeD outBufSizeD (Enc.encodeWith enc inBufSizeE outBufSizeE s) == s

codepageTestGroup :: TestTree
codepageTestGroup = 
  testGroup "CodePage"
  [ testProperty "encode decode cp932" prop_encode_decode_cp932
  ]
