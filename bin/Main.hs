{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude

import qualified Setop               as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Options.Applicative as OPT

data Options = Options
  { left  :: FilePath
  , setop :: Text
  , right :: FilePath }

stdinKey :: FilePath
stdinKey = "STDIN" -- special file path for input

progDesc :: Text
progDesc = "Perform a set operation on two files."

inputHelp :: Text
inputHelp = "either a file or the 'STDIN' literal."

setopHelp :: Text
setopHelp = "U / Union, D / Diff, J / Disj, I / Inter."

options :: OPT.Parser Options
options = Options
  <$> OPT.strArgument (OPT.metavar "LEFT" <> OPT.help (toS inputHelp))
  <*> OPT.strArgument (OPT.metavar "SETOP" <> OPT.help (toS setopHelp))
  <*> OPT.strArgument (OPT.metavar "RIGHT" <> OPT.help (toS inputHelp))

parser :: OPT.ParserInfo Options
parser = OPT.info
  (options <**> OPT.helper)
  (OPT.fullDesc <> OPT.progDesc (toS progDesc))

main :: IO ()
main = do
  Options{..} <- OPT.execParser parser
  when (left == stdinKey && right == stdinKey)
    (throwIO $ AssertionFailed "LEFT and RIGHT arguments cannot both be STDIN")
  operation <- case toSetOp setop of
    Left err -> throwIO err
    Right op -> return op
  lContent <- contentOf left
  rContent <- contentOf right
  let lSet = fromText lContent
      rSet = fromText rContent
      oSet = operation lSet rSet
  TIO.putStr $ toText oSet

-- |Convert set to text
toText :: Set Text -> Text
toText = T.unlines . S.toList

-- |Convert text to set op
toSetOp :: Text -> Either PatternMatchFail (Set Text -> Set Text -> Set Text)
toSetOp "U"     = return S.union
toSetOp "Union" = return S.union
toSetOp "D"     = return S.difference
toSetOp "Diff"  = return S.difference
toSetOp "J"     = return S.disjunction
toSetOp "Disj"  = return S.disjunction
toSetOp "I"     = return S.intersection
toSetOp "Inter" = return S.intersection
toSetOp setop   = Left $ PatternMatchFail ("invalid set operation: "  ++ toS setop)

-- |Convert lines to a set
fromText :: Text -> Set Text
fromText = S.fromList . filter (not . T.null) . T.lines

-- |Return content of file
contentOf :: FilePath -> IO Text
contentOf fp = if fp /= stdinKey
               then TIO.readFile fp -- file
               else TIO.getContents -- stdin
