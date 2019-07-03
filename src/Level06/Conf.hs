{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Monoid              ((<>), Last, getLast)

import           Level06.AppM             (AppM, liftIO, liftEither)
import           Level06.Types            (Conf (..), ConfigError (..),
                                           DBFilePath (DBFilePath), PartialConf (PartialConf),
                                           Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf (pure $ Port 4000) (pure $ DBFilePath "default")

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf lPort lDbFilePath) = 
  case (getLast lPort) of
    Nothing -> Left PortInvalid
    Just port -> case (getLast lDbFilePath) of
      Nothing -> Left DBFilePathInvalid
      Just dbFilePath -> Right $ Conf port dbFilePath

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp =
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  liftIO commandLineParser
    >>= (\pc -> parseJSONConfigFile fp >>= (\pc' -> pure $ defaultConf <> pc' <> pc))
    >>= liftEither . makeConfig
