module Data.ByteString.Lazy.Encoding
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

import Data.ByteString.Lazy.Encoding.Internal
import System.IO
