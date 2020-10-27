module Tryhard.EngineSpec
  ( spec
  )
where

import           Test.Hspec

import           Tryhard.Engine
import           Tryhard.Types
import           Tryhard.Fixture
import           Tryhard.OpenDota

import           Data.Functor.Identity          ( Identity(runIdentity) )

spec :: Spec
spec = do
  describe "#recomend" $ do
    it "antimage is good agains axe and bane" $ do
      matchupHero <$> recomend' matchups antiMage `shouldBe` [axe, bane]

recomend' :: ConstMathcupMap -> Hero -> [Matchup]
recomend' m h = runIdentity $ recomend m h
