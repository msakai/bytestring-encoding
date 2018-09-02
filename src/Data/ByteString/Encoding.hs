-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Encoding
-- Copyright   :  (c) Masahiro Sakai 2018
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- 'B.ByteString' ↔ 'T.Text' converter based on "GHC.IO.Encoding".
--
-----------------------------------------------------------------------------
module Data.ByteString.Encoding
  (
  -- * Converter functions
    encode
  , decode

  -- * Text encodings (re-exported from "System.IO")
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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Encoding as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.IO

-- | Encode a strict 'T.Text' into strict 'B.ByteString' using a given 'Enc.TextEncoding'.
encode :: TextEncoding -> T.Text -> B.ByteString
encode enc = BL.toStrict . L.encode enc . TL.fromStrict

-- | Decode a strict 'B.ByteString' to a strit 'T.Text' using a given 'Enc.TextEncoding'.
decode :: TextEncoding -> B.ByteString -> T.Text
decode enc = TL.toStrict . L.decode enc . BL.fromStrict

