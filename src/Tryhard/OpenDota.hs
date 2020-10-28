module Tryhard.OpenDota where

import           System.FilePath                ( (</>) )
import           Data.Maybe                     ( catMaybes )

import           Tryhard.Config
import           Tryhard.Types
import qualified Tryhard.OpenDota.Internal     as I
import           Tryhard.OpenDota.HeroDB


heroesETagFile :: FilePath -> FilePath
heroesETagFile home = home </> "heroes.etag"

heroesJSONFile :: FilePath -> FilePath
heroesJSONFile home = home </> "heroes.json"

getHeroes :: AppConfig -> IO HeroDB
getHeroes appConfig = do
  url  <- prepareUrl (appConfigHeroJsonURL appConfig)
  home <- toPath $ appConfigHome appConfig
  let etagFilePath   = heroesETagFile home
  let heroesFilePath = heroesJSONFile home
  heroes <- I.getHeroes etagFilePath heroesFilePath url
  pure $ toDB heroes

toDB :: [I.HeroResponse] -> HeroDB
toDB responses = fromList $ entry <$> responses
 where
  entry resposne =
    ( heroId
    , Hero { heroID   = heroId
           , heroName = I.heroResponseName resposne
           , heroLegs = I.heroResponseLegs resposne
           }
    )
    where heroId = toheroID $ I.heroResponseID resposne

toheroID :: I.HeroIDResponse -> HeroID
toheroID = HeroID . I.unHeroID

getHeroMatchup :: AppConfig -> HeroDB -> Hero -> IO [Matchup]
getHeroMatchup appConfig heroeDB hero = do
  url         <- prepareUrl (appConfigOpenDotaApi appConfig)
  rawResponse <- I.getHeroMatchup (unHero heroId) url
  pure $ catMaybes $ toMatchup <$> rawResponse -- Ignore matchups that we can't find hero for
 where
  heroId = heroID hero
  toMatchup :: I.HeroMatchupResponse -> Maybe Matchup
  toMatchup response = do
    hero' <-
      (heroeDB) `byHeroId` (toheroID $ I.heroMatchupResponseHeroID response)
    pure $ Matchup
      { matchupHero        = hero'
      , matchupGamesPlayed = I.heroMatchupGamesResponsePlayed response
      , matchupWins        = I.heroMatchupResponseWins response
      }
