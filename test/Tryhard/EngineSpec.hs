module Tryhard.EngineSpec
  ( spec
  )
where

import           Test.Hspec
import           Data.Maybe                     ( catMaybes )

import           Tryhard.Engine
import           Tryhard.Types
import           Tryhard.Fixture
import           Tryhard.OpenDota

import           Data.Functor.Identity          ( Identity(runIdentity) )

spec :: Spec
spec = do
  describe "#recomend" $ do
    it "antimage is good agains axe and bane" $ do
      recomend' db matchups antiMage `shouldBe` [axe, bane]

recomend' :: [Hero] -> ConstMathcupMap -> Hero -> [Hero]
recomend' heroes m h =
  catMaybes $ bindHeros heroes <$> (runIdentity $ recomend m h)
