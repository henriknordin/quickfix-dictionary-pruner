module Main where

import qualified Data.ByteString     as BS
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Lib                 (prune)

newtype Option = Option String

run :: Option -> IO ()
run (Option filename) = do
  input <- BS.readFile filename
  let result = prune input
  mapM_ print result
  return ()

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs { prefShowHelpOnError = True }

main :: IO ()
main = run =<< customExecParser parserPrefs opts
  where
    parser :: Parser Option
    parser = Option <$> strOption (long "file" <>
                                   short 'f' <>
                                   help "The file to examine" <>
                                   metavar "FILENAME")
    opts = info (  parser <**> helper)
      (  fullDesc
      <> header "Lists all unused field definitions in a QuickFix data dictionary"
      )
