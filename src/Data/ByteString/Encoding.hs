module Data.ByteString.Encoding
  (
  -- * Converter functions
    encode
  , decode

  -- * Unicode encodings
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

import Data.ByteString.Encoding.Internal
import System.IO
