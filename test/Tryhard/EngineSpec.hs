module Tryhard.EngineSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Char

import           Tryhard.Engine
import           Tryhard.Types
import           Tryhard.Fixture
import           Tryhard.OpenDota

import           Data.Functor.Identity          ( Identity(runIdentity) )

yourTeam :: [Hero]
yourTeam = [antiMage]

spec :: Spec
spec = do
  describe "#recomend" $ do
    it "antimage is good agains axe and bane" $ do
      recomend' db matchups antiMage `shouldBe` [axe, bane]



recomend' :: [Hero] -> ConstMathcupMap -> Hero -> [Hero]
recomend' hs m h = runIdentity $ recomend hs m h
