module Tryhard.OpenDota where

import           System.FilePath                ( (</>) )
import           Data.Maybe                     ( catMaybes )
import           Data.Functor.Compose
import           Data.Bifunctor                 ( bimap )

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
  pure $ toDB $ toHero <$> heroes

toHero :: I.HeroResponse -> Hero
toHero resposne = Hero { heroID   = toheroID $ I.heroResponseID resposne
                       , heroName = I.heroResponseName resposne
                       , heroLegs = I.heroResponseLegs resposne
                       }

toDB :: [Hero] -> HeroDB
toDB heroes = fromList $ entry <$> heroes
  where entry hero = (heroID hero, hero)

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
  pure $ unLHS <$> (statsFromList $ catMaybes $ sequence <$> y) -- Ignore matchups that we can't find hero for
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

inComp :: Hero -> MatchComp -> Bool
inComp h mc = h `elem` toList mc

getHeroCombo :: AppConfig -> HeroDB -> MatchComp -> IO (StatsResult Combo)
getHeroCombo appConfig heroeDB matchComp = do
  url         <- prepareUrl (appConfigOpenDotaApi appConfig)
  rawResponse <- I.getHerosCombo (unHero <$> heroID <$> teamA)
                                 (unHero <$> heroID <$> teamB)
                                 url
  let allEntries = mconcat $ (toCombo heroeDB) <$> rawResponse
  let entries    = filter (\(_, h) -> not $ h `inComp` matchComp) allEntries
  pure $ statsFromList entries -- Ignore matchups that we can't find hero in any all the combo
  where (teamA, teamB) = toTuple matchComp

-- Tuple like, but with the same `a` in both cases
data TwoOf a = TwoOf { unTwoOfA :: a, unTwoOfB :: a }

instance Functor TwoOf where
  fmap f (TwoOf a b) = TwoOf (f a) (f b)

tuple :: TwoOf a -> (a, a)
tuple (TwoOf a b) = (a, b)

toCombo :: HeroDB -> I.HeroComboResponse -> [(Combo, Hero)]
toCombo db (I.HeroComboResponse { I.heroComboResponseTeamA = teamA, I.heroComboResponseTeamB = teamB })
  = maybe [] id go
 where
  toHero :: I.HeroIDResponse -> Maybe Hero
  toHero x = db `byHeroId` (toheroID x)
  go :: Maybe [(Combo, Hero)]
  go = do
    let twoOf = TwoOf teamA teamB
    x <- superSequence $ getCompose $ toHero <$> Compose twoOf
    let (teamA', teamB') = tuple x
    let teams            = (teamA', teamB')
    let (entriesMyTeam, entriesEnemyTeam) =
          bimap (\h -> ((,) withC) <$> h) (\h -> ((,) againstC) <$> h) $ teams
    pure $ entriesMyTeam <> entriesEnemyTeam


superSequence :: TwoOf [Maybe Hero] -> Maybe (TwoOf [Hero])
superSequence (TwoOf a b) = do
  x <- sequence a
  y <- sequence b
  pure $ TwoOf x y
