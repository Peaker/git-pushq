{-# OPTIONS -Wall #-}
-- | Options parsing
module Opts
    ( Opts(..), parseOpts
    ) where

import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (NominalDiffTime)
import qualified Git.CLI as Git
import           Options.Applicative

data Opts = Opts
    { optsPollInterval :: NominalDiffTime
    , optsRepoPath :: Git.RepoPath
    } deriving (Show)

defaultPollInterval :: NominalDiffTime
defaultPollInterval = 20

desc :: String
desc =
    intercalate "\n"
    [ "Run a git push-q"
    , ""
    , "<TODO>"
    ]

opt :: Read a => Mod OptionFields a -> Parser (Maybe a)
opt = optional . option auto

strOpt :: Mod OptionFields String -> Parser (Maybe String)
strOpt = optional . strOption

fromDouble :: Fractional a => Double -> a
fromDouble = realToFrac

parser :: Parser Opts
parser =
    Opts
    <$> (maybe defaultPollInterval fromDouble
         <$> opt (long "interval" <> metavar "interval" <> help "poll interval in (fractional) seconds"))
    <*> (fromMaybe "." <$> strOpt (long "path" <> metavar "repopath" <> help "The path to the git repo"))

parseOpts :: IO Opts
parseOpts =
    execParser $
    info (helper <*> parser)
    (fullDesc <> progDesc desc <> header "buildsome - build an awesome project")
