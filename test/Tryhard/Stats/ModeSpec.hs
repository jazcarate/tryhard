module Tryhard.Stats.ModeSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.Fixture

import           Tryhard.Types
import           Tryhard.Stats.Mode

spec :: Spec
spec = do
  describe "#WinPercentage" $ do
    it "A WinPercentage is equal to itself" $ do
      WinPercentage vsAxe == WinPercentage vsAxe `shouldBe` True
    it "has won more vs Bane than to Axe" $ do
      WinPercentage vsAxe `shouldSatisfy` (<) (WinPercentage vsBane)
    prop "sorts by the number of matches" $ \(n1, n2) ->
      let m1 = WinPercentage (matchup bane 100 n1)
          m2 = WinPercentage (matchup axe 100 n2)
      in  (n1 /= 0 && n2 /= 0) ==> n1 `compare` n2 == m1 `compare` m2
    it "extracts the hero" $ do
      getHero (WinPercentage vsAxe) `shouldBe` axe
    it "can be displayed" $ do
      show (WinPercentage vsAxe) `shouldBe` "50.00%"
    describe "0 games played" $ do
      let zeroMatchesPlayed = WinPercentage (matchup axe 0 100)
      it "shows a null" $ do
        show zeroMatchesPlayed `shouldBe` "-%"
      prop "is less than any other (played) matchup" $ \n ->
        n /= 0 ==> zeroMatchesPlayed `shouldSatisfy` (<)
          (WinPercentage (matchup axe n 100))

  describe "#NumberOfMatches" $ do
    it "A NumberOfMatches is equal to itself" $ do
      NumberOfMatches vsAxe == NumberOfMatches vsAxe `shouldBe` True
    it "has more times played against axe than bane" $ do
      NumberOfMatches vsAxe `shouldSatisfy` (>) (NumberOfMatches vsBane)
    prop "sorts by the number of matches" $ \(n1, n2) ->
      let m1 = NumberOfMatches (matchup bane n1 100)
          m2 = NumberOfMatches (matchup axe n2 100)
      in  n1 `compare` n2 == m1 `compare` m2
    it "extracts the hero" $ do
      getHero (NumberOfMatches vsAxe) `shouldBe` axe
    it "can be displayed" $ do
      show (NumberOfMatches vsAxe) `shouldBe` "10"