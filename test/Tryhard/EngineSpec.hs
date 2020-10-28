module Tryhard.EngineSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Fixture

import           Data.Functor.Identity          ( Identity(runIdentity) )

import           Tryhard.Engine
import           Tryhard.Types
import           Tryhard.Stats

spec :: Spec
spec = do
  describe "#recomend" $ do
    it "antimage is good agains axe and bane" $ do
      matchupHero <$> recomend' matchups antiMage `shouldBe` [axe, bane]

recomend' :: ConstMathcupMap -> Hero -> [Matchup]
recomend' m h = runIdentity $ recomend m h
