{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified System.Info
import Test.Tasty

import Base
import CodePage
import Iconv

main :: IO ()
main = defaultMain $ testGroup "bytestring-encoding" $
  [ baseTestGroup, codepageTestGroup ] ++
  [ iconvTestGroup | System.Info.os /= "mingw32" ]

