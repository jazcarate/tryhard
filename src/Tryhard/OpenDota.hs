module Tryhard.OpenDota where

import           System.FilePath                ( (</>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Functor.Compose

import           Tryhard.Config
import           Tryhard.Types
import qualified Tryhard.OpenDota.Internal     as I
import qualified Tryhard.OpenDota.HeroDB       as DB
import           Tryhard.Hero
import           Tryhard.Picks
import           Tryhard.Stats.Matchup
import           Tryhard.Stats.Combo
import           Tryhard.Stats.Result



heroesETagFile :: FilePath -> FilePath
heroesETagFile home = home </> "heroes.etag"

heroesJSONFile :: FilePath -> FilePath
heroesJSONFile home = home </> "heroes.json"

getHeroes :: AppConfig -> IO DB.HeroDB
getHeroes appConfig = do
  url  <- prepareUrl (appConfigHeroJsonURL appConfig)
  home <- toPath $ appConfigHome appConfig
  let etagFilePath   = heroesETagFile home
  let heroesFilePath = heroesJSONFile home
  heroes <- I.getHeroes etagFilePath heroesFilePath url
  pure $ toDB $ toHero <$> heroes

toHero :: I.HeroResponse -> Hero
toHero resposne = Hero { heroID   = toheroID $ I.heroResponseID resposne
                       , heroName = I.heroResponseName resposne
                       , heroLegs = I.heroResponseLegs resposne
                       }

toDB :: [Hero] -> DB.HeroDB
toDB heroes = DB.fromList $ entry <$> heroes
  where entry hero = (heroID hero, hero)

toheroID :: I.HeroIDResponse -> HeroID
toheroID = HeroID . I.unHeroID

getHeroMatchup :: AppConfig -> DB.HeroDB -> Hero -> IO (Result Matchup)
getHeroMatchup appConfig heroeDB hero = do
  url         <- prepareUrl (appConfigOpenDotaApi appConfig)
  rawResponse <- I.getHeroMatchup (heroId) url
  let y =
        getCompose
          $   (DB.byHeroId heroeDB)
          <$> (Compose $ toMatchupEntry <$> rawResponse)
  pure $ unLHS <$> (fromList $ catMaybes $ sequence <$> y) -- Ignore matchups that we can't find hero for
 where
  heroId = unHero $ heroID hero
  toMatchupEntry :: I.HeroMatchupResponse -> (LHS Matchup, HeroID) -- We know that the request gets us a single matchup per hero, so we wont need to <>
  toMatchupEntry response =
    ( LHS $ Matchup
      { matchupGamesPlayed = I.heroMatchupGamesResponsePlayed response
      , matchupWins        = I.heroMatchupResponseWins response
      }
    , toheroID $ I.heroMatchupResponseHeroID response
    )


getHeroCombo :: AppConfig -> DB.HeroDB -> Picks -> IO (Result Combo)
getHeroCombo appConfig heroeDB matchComp = do
  url         <- prepareUrl (appConfigOpenDotaApi appConfig)
  rawResponse <- I.getHerosCombo (unHero <$> heroID <$> teamA)
                                 (unHero <$> heroID <$> teamB)
                                 url
  let entries = mconcat $ (toCombo heroeDB) <$> rawResponse
  pure $ fromList entries
  where (teamA, teamB) = teams matchComp

-- Tuple like, but with the same `a` in both cases
data TwoOf a = TwoOf { unTwoOfA :: a, unTwoOfB :: a }

instance Functor TwoOf where
  fmap f (TwoOf a b) = TwoOf (f a) (f b)

tuple :: TwoOf a -> (a, a)
tuple (TwoOf a b) = (a, b)

toCombo :: DB.HeroDB -> I.HeroComboResponse -> [(Combo, Hero)]
toCombo db (I.HeroComboResponse { I.heroComboResponseTeamA = teamA, I.heroComboResponseTeamB = teamB })
  = maybe [] id go
 where
  toHero' :: I.HeroIDResponse -> Maybe Hero
  toHero' x = db `DB.byHeroId` (toheroID x)
  go :: Maybe [(Combo, Hero)]
  go = do
    let twoOf = TwoOf teamA teamB
    x <- superSequence $ getCompose $ toHero' <$> Compose twoOf
    let (myTeam, enemyTeam) = tuple x
    let entriesMyTeam       = (\h -> (with, h)) <$> myTeam
    let entriesEnemyTeam    = (\h -> (against, h)) <$> enemyTeam
    pure $ entriesMyTeam <> entriesEnemyTeam


superSequence :: TwoOf [Maybe Hero] -> Maybe (TwoOf [Hero])
superSequence (TwoOf a b) = do
  x <- sequence a
  y <- sequence b
  pure $ TwoOf x y
