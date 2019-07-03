{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Control.Exception          (Exception, SomeException, displayException)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LBS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last), getLast)

import           Data.Text.Encoding         (encodeUtf8)

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))
import           Waargonaut.Decode.Runners  (pureDecodeFromByteString)
import           Waargonaut.Decode.Types    (CursorHistory)

import qualified Waargonaut.Encode          as E

import           Level06.AppM               (AppM (..), liftEither, liftIO)
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf),
                                             partialConfDecoder,
                                             partialConfEncoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = 
  LBS.toStrict . (E.simplePureEncodeByteString partialConfEncoder) <$> parseJSONConfigFile fp

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp =
  tryReadFile fp >>= tryDecodeContent

tryReadFile :: FilePath -> AppM ConfigError LBS.ByteString
tryReadFile = liftIO . LBS.readFile

tryDecodeContent :: LBS.ByteString -> AppM ConfigError PartialConf
tryDecodeContent = 
  liftEither . first (BadConfFile . fst) . mapByteStringToPartialConf

mapByteStringToPartialConf :: LBS.ByteString -> Either (DecodeError, CursorHistory) PartialConf
mapByteStringToPartialConf = 
  pureDecodeFromByteString AB.parseOnly partialConfDecoder . LBS.toStrict

-- Go to 'src/Level06/Conf.hs' next.
