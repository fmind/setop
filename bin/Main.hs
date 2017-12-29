{-# LANGUAGE RecordWildCards #-}

module Main where

-- import           Setop
import           Protolude

-- import qualified System.IO as SIO
import qualified Options.Applicative as OPT

data Options = Options
  { left  :: Text
  , setop :: Text
  , right :: Text }

desc :: Text
desc = "Perform a set operation between two files."

inputHelp :: Text
inputHelp = "either a file path or the 'STDIN' literal."

setopHelp :: Text
setopHelp = "one letter: U (union), D (diff), J (disj), I (inter)."

options :: OPT.Parser Options
options = Options
  <$> OPT.strArgument (OPT.metavar "LEFT" <> OPT.help (toS inputHelp))
  <*> OPT.strArgument (OPT.metavar "SETOP" <> OPT.help (toS setopHelp))
  <*> OPT.strArgument (OPT.metavar "RIGHT" <> OPT.help (toS inputHelp))

parser :: OPT.ParserInfo Options
parser = OPT.info
  (options <**> OPT.helper)
  (OPT.fullDesc <> OPT.progDesc (toS desc))

main :: IO ()
main = do
  Options{..} <- OPT.execParser parser
  when (left == "STDIN" && right == "STDIN")
    (throwIO $ AssertionFailed "LEFT and RIGHT arguments cannot both refer to STDIN")
  print left
  print right
  print setop
