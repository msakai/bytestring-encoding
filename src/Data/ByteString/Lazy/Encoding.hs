-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Lazy.Encoding
-- Copyright   :  (c) Masahiro Sakai 2018
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- 'BL.ByteString' ↔ 'TL.Text' converter based on "GHC.IO.Encoding".
--
-----------------------------------------------------------------------------
module Data.ByteString.Lazy.Encoding
  (
  -- * Converter functions
    encode
  , decode

  -- * Text encodings (re-export from "System.IO")
  , TextEncoding
  , latin1
  , utf8
  , utf8_bom
  , utf16
  , utf16le
  , utf16be
  , utf32
  , utf32le
  , utf32be
  , localeEncoding
  , char8
  , mkTextEncoding
  ) where

import Data.ByteString.Lazy.Encoding.Internal
import System.IO
