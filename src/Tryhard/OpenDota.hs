module Tryhard.OpenDota where

import           System.FilePath                ( (</>) )
import           Data.Maybe                     ( catMaybes )

import           Tryhard.Config
import           Tryhard.Types
import qualified Tryhard.OpenDota.Internal     as I
import           Tryhard.OpenDota.HeroDB
import           Data.Functor.Compose


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

getHeroMatchup :: AppConfig -> HeroDB -> Hero -> IO (StatsResult Matchup)
getHeroMatchup appConfig heroeDB hero = do
  url         <- prepareUrl (appConfigOpenDotaApi appConfig)
  rawResponse <- I.getHeroMatchup (heroId) url
  let y =
        getCompose
          $   (byHeroId heroeDB)
          <$> (Compose $ toMatchupEntry <$> rawResponse)
  pure $ statsFromList $ catMaybes $ sequence <$> y -- Ignore matchups that we can't find hero for
 where
  heroId = unHero $ heroID hero
  toMatchupEntry :: I.HeroMatchupResponse -> (Matchup, HeroID)
  toMatchupEntry response =
    ( Matchup { matchupGamesPlayed = I.heroMatchupGamesResponsePlayed response
              , matchupWins        = I.heroMatchupResponseWins response
              }
    , toheroID $ I.heroMatchupResponseHeroID response
    )
