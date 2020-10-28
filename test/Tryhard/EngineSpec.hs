module Tryhard.EngineSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Fixture

import           Data.Functor.Identity          ( Identity(runIdentity) )

import           Tryhard.Engine
import           Tryhard.Stats.Mode
import           Tryhard.Types

spec :: Spec
spec = do
  describe "#recomend" $ do
    it "antimage is good agains bane, then" $ do
      recomend' antiMage `shouldBe` [bane, axe]

recomend' :: Hero -> [Hero]
recomend' h = runIdentity $ do
  mups <- matchups `for` h
  let stats   = numberOfMatches <$> mups
  let results = recomend stats
  pure $ resultHero <$> results
