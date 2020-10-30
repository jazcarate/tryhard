module Tryhard.Stats.ModeSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.Fixture

import           Tryhard.Stats.Mode

spec :: Spec
spec = do
  describe "Compare" $ do
    describe "#WinPercentage" $ do
      it "a percentage is equal to itself" $ do
        WinPercentage (percent 50) == WinPercentage (percent 50) `shouldBe` True
      it "can be compared" $ do
        (WinPercentage (percent 50))
          <          (WinPercentage (percent 100))
          `shouldBe` True
      prop "compares by the wins when played are the same" $ \(n1, n2) ->
        let m1 = WinPercentage (percent n1)
            m2 = WinPercentage (percent n2)
        in  (n1 /= 0 && n2 /= 0) ==> n1 `compare` n2 == m1 `compare` m2
      it "can be displayed" $ do
        show (WinPercentage (percent 50)) `shouldBe` "50.00%"
      describe "0 games played" $ do
        let zeroMatchesPlayed = WinPercentage notPlayed
        it "shows a null" $ do
          show zeroMatchesPlayed `shouldBe` "-%"
        prop "is less than any other (played) matchup" $ \n ->
          (n /= 0)
            ==> (          (zeroMatchesPlayed < (WinPercentage (matchup n 100)))
                `shouldBe` True
                )

    describe "#NumberOfMatches" $ do
      it "a numberOfMatches is equal to itself" $ do
        NumberOfMatches 50 == NumberOfMatches 50 `shouldBe` True
      it "can be compared" $ do
        (NumberOfMatches 10) < (NumberOfMatches 100) `shouldBe` True
      prop "sorts by the number of matches" $ \(n1, n2) ->
        let m1 = NumberOfMatches n1
            m2 = NumberOfMatches n2
        in  n1 `compare` n2 == m1 `compare` m2
      it "can be displayed" $ do
        show (NumberOfMatches 10) `shouldBe` "10 matches"

    describe "#NumberOfLegs" $ do
      it "a numberOfLegs is equal to itself" $ do
        numberOfLegs axe == numberOfLegs axe `shouldBe` True
      it "axe has less legs than bane" $ do
        (numberOfLegs axe) < (numberOfLegs bane) `shouldBe` True
      prop "sorts by the number of legs" $ \(legs1, legs2) ->
        let m1 = numberOfLegs (hero "foo" 1 legs1)
            m2 = numberOfLegs (hero "bar" 2 legs2)
        in  legs1 `compare` legs2 == m1 `compare` m2
      it "can be displayed" $ do
        show (numberOfLegs axe) `shouldBe` "2 legs"

  describe "Merging" $ do
    describe "#Max" $ do
      describe ".NumberOfMatches" $ do
        prop "collapses two NumberOfMatches, keeping the bigger one"
          $ \(played1, played2) ->
              let m1 = Max $ (NumberOfMatches played1)
                  m2 = Max $ (NumberOfMatches played2)
              in  case compare played1 played2 of
                    EQ -> m1 <> m2 `shouldBe` m1
                    LT -> m1 <> m2 `shouldBe` m2
                    GT -> m1 <> m2 `shouldBe` m1
      describe ".WinPercentage" $ do
        prop
            "collapses two WinPercentage, keeping the better one (when all have been played"
          $ \(win1, played1, win2, played2) ->
              let m1 = Max $ (WinPercentage (matchup played1 win1))
                  m2 = Max $ (WinPercentage (matchup played2 win2))
                  ratio :: Int -> Int -> Float
                  ratio w p = (realToFrac w / realToFrac p)
              in  (played1 /= 0 && played2 /= 0)
                    ==> case
                          compare (win1 `ratio` played1) (win2 `ratio` played2)
                        of
                          EQ -> m1 <> m2 `shouldBe` m1
                          LT -> m1 <> m2 `shouldBe` m2
                          GT -> m1 <> m2 `shouldBe` m1
        it "chooses the played one over the not played one" $ do
          (Max $ WinPercentage (notPlayed))
            <>         (Max $ WinPercentage (percent 10))
            `shouldBe` (Max $ WinPercentage (percent 10))

    describe "#Sum" $ do
      describe ".NumberOfMatches" $ do
        prop "collapses two NumberOfMatches, adding the number of matches"
          $ \(played1, played2) ->
              let m1 = Sum $ (NumberOfMatches played1)
                  m2 = Sum $ (NumberOfMatches played2)
              in  m1
                    <>         m2
                    `shouldBe` (Sum $ NumberOfMatches (played1 + played2))
      describe ".WinPercentage" $ do
        it "can colapse with no matches, and simply ignores it" $ do
          getMax
              (  Max (WinPercentage notPlayed)
              <> Max (WinPercentage (percent 100))
              )
            `shouldBe` (WinPercentage (percent 100))
