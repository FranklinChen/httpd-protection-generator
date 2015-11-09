module GenerateConfSpec (main, spec) where

import Test.Hspec

import Control.Monad.Reader

import Data.Either (isRight)
import Data.Yaml (decodeFileEither)

import Pipes
import qualified Pipes.Prelude as P

import GenerateConf (AppConfig(..), directivesOutput)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "GenerateConf" $ do
    testFiles "sample"
    testFiles "sample-users"

testFiles :: String -> SpecWith ()
testFiles baseName =
    it ("handles file " ++ baseName) $ do
      parseResult <- decodeFileEither $ yamlFile baseName
      parseResult `shouldSatisfy` isRight

      let Right theDoc = parseResult

      let mockGoodDir _ = return False
      let config = AppConfig theDoc mockGoodDir

      (warnings, result) <- P.toListM' $ runReaderT directivesOutput config
      expectedWarnings <- readFile $ errFile baseName
      warnings `shouldBe` lines expectedWarnings

      expected <- readFile $ confFile baseName
      result `shouldBe` expected

yamlFile :: String -> FilePath
yamlFile baseName = baseName ++ ".yaml"

confFile :: String -> FilePath
confFile baseName = baseName ++ ".conf"

errFile :: String -> FilePath
errFile baseName = baseName ++ ".err"
