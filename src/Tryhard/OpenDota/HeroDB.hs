module Tryhard.OpenDota.HeroDB where

import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import qualified Text.Fuzzy                    as Fuzzy
import           Data.Maybe                     ( listToMaybe )

import           Tryhard.Hero

newtype HeroDB = HeroDB { unHeroDB :: HM.HashMap HeroID Hero }

byNameLike :: HeroDB -> Text -> Maybe Hero
byNameLike HeroDB { unHeroDB = haystack } needle = do
  let entries = HM.elems haystack
  let matches = Fuzzy.filter needle entries mempty mempty heroName False
  Fuzzy.original <$> listToMaybe matches

byHeroId :: HeroDB -> HeroID -> Maybe Hero
byHeroId HeroDB { unHeroDB = haystack } needle = do
  HM.lookup needle haystack


fromList :: [(HeroID, Hero)] -> HeroDB
fromList = HeroDB . HM.fromList

findAll :: HeroDB -> [Hero]
findAll = HM.elems . unHeroDB
