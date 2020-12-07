module Main where

import           Assign
import           Data.Text
import           System.Environment
import           System.Process
import           Options.Applicative

data Options = Options { inputFile  :: FilePath
                       , rulesFile :: Maybe FilePath
                       , outputFile :: FilePath
                       , delimiter :: Char
                       }


assignKeysOptionsParser :: ParserInfo Options
assignKeysOptionsParser =
  info (optionsParser <**> helper)
  ( progDesc "")

optionsParser :: Parser Options
optionsParser = Options
  <$> inputFileOption
  <*> optional rulesFileOption
  <*> outputFileOption
  <*> delimiterOption

inputFileOption :: Parser FilePath
inputFileOption =
  strOption (long "input-file"
               <> short 'i'
               <> metavar "FILE"
               <> help ( "Input file, expect a semicolon-separated file with the following structure, eg. so-called 'raw' entries\n" <>
                         "as emitted by an accountant:\n\n" <>
                         "Compte;Journal;Date;Piece;Libelle;RefLibelle;Reference;Debit;Credit;Solde\n"  <>
                         "10100000;BQE;13/05/2020;5;PANKZSOFT;Capital;VIRT;0;4000;-4000\n" <>
                         "27100000;BQE;04/06/2020;16;SOUSCRIP PS;Titres immobilisés (droit de propriété);;15.2;0;15.2"
                       )
              )


outputFileOption :: Parser FilePath
outputFileOption =
  strOption (long "output-file"
               <> short 'o'
               <> metavar "FILE"
                <> help "File to output entries with assignment to. The content of the file will be erased"
              )

rulesFileOption :: Parser FilePath
rulesFileOption =
  strOption (long "rules-file"
               <> short 'r'
               <> metavar "FILE"
               <> help "File containing assignment rules of the form '<regexp>  -> <name>'"
              )

delimiterOption :: Parser Char
delimiterOption =
  option (maybeReader readSingleChar) (short 'd' <> metavar "CHAR" <> value '\t'  <> help "field delimiter to use for input file (default: '\t')")
  where
    readSingleChar (c:_) = Just c
    readSingleChar [] = Nothing

main :: IO ()
main = do
  Options{inputFile,rulesFile,outputFile,delimiter} <- execParser assignKeysOptionsParser
  assignKeys inputFile rulesFile outputFile delimiter
