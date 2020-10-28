module Tryhard.ConfigSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           System.Directory               ( getAppUserDataDirectory )

import           Tryhard.Config

spec :: Spec
spec = do
  describe "#transformPath" $ do
    describe "absolute path" $ do
      it "keeps the paths if it is absolute" $ do
        path <- transformPath "/home/me/tryhard/"
        path `shouldBe` "/home/me/tryhard/"
      it "makes the path a folder" $ do
        path <- transformPath "/home/me/tryhard"
        path `shouldBe` "/home/me/tryhard/"
    describe "relative path" $ do
      it "uses the user data directory" $ do
        path        <- transformPath "tryhard/"
        userDataDir <- getAppUserDataDirectory "tryhard/"
        path `shouldBe` userDataDir
    describe "edge cases" $ do
      describe "\0" $ do
        it "transforms to '_'" $ do
          path <- transformPath "/try\0hard"
          path `shouldBe` "/try_hard/"
      describe "empty string" $ do
        it "transforms to '_'" $ do
          path        <- transformPath mempty
          userDataDir <- getAppUserDataDirectory "_/"
          path `shouldBe` userDataDir
    it "always succedes" $ monadicIO $ do
      path <- pick arbitrary
      _    <- run $ transformPath path
      pure ()
