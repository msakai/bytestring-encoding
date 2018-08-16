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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Encoding as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.IO

encode :: TextEncoding -> T.Text -> B.ByteString
encode enc = BL.toStrict . L.encode enc . TL.fromStrict

decode :: TextEncoding -> B.ByteString -> T.Text
decode enc = TL.toStrict . L.decode enc . BL.fromStrict

