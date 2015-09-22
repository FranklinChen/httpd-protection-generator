module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist)

import Control.Monad.Reader

import Data.Yaml (decodeFileEither)

import Pipes
import qualified Pipes.Prelude as P

import GenerateConf (AppConfig(..), directivesOutput)

main :: IO ()
main = getArgs >>= runOnArgs

-- | Run and output any errors on 'stderr'.
runOnArgs :: [String] -> IO ()
runOnArgs [inputFile, outputFile] = do
  parseResult <- decodeFileEither inputFile
  case parseResult of
    Left e ->
      hPutStrLn stderr $ "YAML parse error: " ++ show e
    Right theDoc ->
      let config = AppConfig theDoc doesDirectoryExist
      in runEffect (runReaderT directivesOutput config >->
                    P.toHandle stderr) >>=
         writeFile outputFile
runOnArgs _ = do
  progName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ progName ++ " configFile.yaml output.conf"
