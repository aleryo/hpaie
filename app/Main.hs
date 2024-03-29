module Main where

import Data.Text
import Options.Applicative
import Run
import System.Environment
import System.Process

data Options = Options
    { inputFile :: FilePath
    , outputFile :: FilePath
    , delimiter :: Char
    }

hpaieOptionsParser :: ParserInfo Options
hpaieOptionsParser =
    info
        (optionsParser <**> helper)
        (progDesc "Compute analytical accounting from raw ledger")

optionsParser :: Parser Options
optionsParser =
    Options
        <$> inputFileOption
        <*> outputFileOption
        <*> delimiterOption

delimiterOption :: Parser Char
delimiterOption =
    option (maybeReader readSingleChar) (short 'd' <> metavar "CHAR" <> value '\t' <> help "field delimiter to use for input file (default: '\t')")
  where
    readSingleChar (c : _) = Just c
    readSingleChar [] = Nothing

inputFileOption :: Parser FilePath
inputFileOption =
    strOption
        ( long "input-file"
            <> short 'i'
            <> metavar "FILE"
            <> help
                ( "Input file, expect a tab-separated file with the following structure:\n"
                    <> "Date\tcompte\tlibelle\tsens\tmontant\tkeys\n"
                    <> "13/02/2018\t10100000:Capital\tCLOTURE COMPTE CAPITAL\tC\t4000,00\tALL\n"
                    <> "30/11/2017\t40110000:Fournisseurs\tMois Novembre 2017\tC\t2743,34\tALL\n"
                )
        )

outputFileOption :: Parser FilePath
outputFileOption =
    strOption
        ( long "output-file"
            <> short 'o'
            <> metavar "FILE"
            <> help "File to output generated ledger entries to. The content of the file will be erased"
        )

main :: IO ()
main = do
    Options{inputFile, outputFile, delimiter} <- execParser hpaieOptionsParser
    comptaAnalytique inputFile outputFile delimiter
    callProcess "hledger" ["-f", outputFile, "bal"]
