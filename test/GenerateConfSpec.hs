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
  describe "GenerateConf" $
    it "parses YAML" $ do
      parseResult <- decodeFileEither "sample.yaml"
      parseResult `shouldSatisfy` isRight

      let Right theDoc = parseResult

      let mockGoodDir _ = return False
      let config = AppConfig theDoc mockGoodDir

      (warnings, result) <- toListM' $ runReaderT directivesOutput config
      expectedWarnings <- readFile "sample.err"
      warnings `shouldBe` lines expectedWarnings

      expected <- readFile "sample.conf"
      result `shouldBe` expected

-- | My utility, based on 'Pipes.Prelude.toListM'
toListM' :: Monad m => Producer a m r -> m ([a], r)
toListM' = P.fold' step begin done where
  step x a = x . (a:)
  begin = id
  done x = x []
{-# INLINABLE toListM' #-}
