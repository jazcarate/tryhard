{-# LANGUAGE TemplateHaskell #-}

module Tryhard.TUI where


import           Lens.Micro                     ( (.~)
                                                , (%~)
                                                , (&)
                                                , (^.)
                                                )
import           Lens.Micro.TH
import qualified Graphics.Vty                  as V

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Core
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.List            as L
import qualified Brick.AttrMap                 as A

import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Focus                   as F
import           Brick.Util                     ( on )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Tx
import qualified Text.Fuzzy                    as Fuzzy
import           Data.Vector                    ( Vector
                                                , fromList
                                                )

import           Tryhard.Types
import           Tryhard.Stats
import           Tryhard.OpenDota.HeroDB        ( findAll
                                                , HeroDB
                                                )

import           Debug.Trace

heroNameOmni :: [Hero] -> Text -> [Hero]
heroNameOmni db query =
  Fuzzy.original <$> Fuzzy.filter query db mempty mempty heroName False

data Event = Event

data State = State
  { _dataSources ::  DataSources
  ,  _heroPopupState :: HeroPopupState
  }

data Name = HeroInput | HeroSelection deriving (Ord, Show, Eq)

data HeroPopupState = HeroPopupState
  { _showPopup :: Bool
  , _input :: E.Editor Text Name
  , _choices :: L.List Name Hero
  , _popupDB :: HeroDB
  }

makeLenses ''State
makeLenses ''HeroPopupState

undefinedW :: T.Widget n
undefinedW = str "Undefined"

popupAttr :: A.AttrName
popupAttr = "popup"

listFocusedAttr :: A.AttrName
listFocusedAttr = "focused"

highlightAttr :: A.AttrName
highlightAttr = "highlight"

-- | hero choosing popup
heroPopup :: HeroPopupState -> T.Widget Name
heroPopup st = case st ^. showPopup of
  False -> emptyWidget
  True ->
    C.centerLayer
      $ withDefAttr popupAttr
      $ withBorderStyle BS.defaultBorderStyle
      $ B.borderWithLabel (str "Choose a hero")
      $ vBox [hBox [padAll 2 $ vBox [editor, results]]]
 where
  editor =
    (   str "Hero: "
    <+> (hLimit 30 $ vLimit 5 $ E.renderEditor (txt . Tx.unwords)
                                               True
                                               (st ^. input)
        )
    )

  results =
    (L.renderList
      (\focused heroW -> if focused
        then withAttr listFocusedAttr $ txt "> " <+> (txt $ heroName heroW)
        else txt $ heroName heroW
      )
      True
      (st ^. choices)
    )


drawUI :: State -> [T.Widget Name]
drawUI st = [popup, context]
 where

  popup   = heroPopup (st ^. heroPopupState)
  context = vBox
    [vLimit 1 $ C.center $ str "Tryhard", selecction, vLimit 1 $ str "Status"]

  selecction = hBox
    [ panel "team comp"      (str "foo")
    , panel "strategy"       (str "foo")
    , panel "recomendations" (str "foo")
    ]
  panel name inner =
    withBorderStyle BS.defaultBorderStyle $ B.borderWithLabel (str name) $ vBox
      [hBox [inner, fill ' '], fill ' ']

handleHeroPopupEvent
  :: V.Event -> HeroPopupState -> T.EventM Name HeroPopupState
handleHeroPopupEvent ev st =
  T.handleEventLensed st choices L.handleListEvent ev
    >>= (\st' -> T.handleEventLensed st' input E.handleEditorEvent ev)
    >>= (\st' -> conditionalModifyHeroList st')


 where
  conditionalModifyHeroList :: HeroPopupState -> T.EventM Name HeroPopupState
  conditionalModifyHeroList st' =
    if E.getEditContents (st ^. input) == E.getEditContents (st' ^. input)
      then pure st'
      else modifyHeroList st'
  modifyHeroList :: HeroPopupState -> T.EventM Name HeroPopupState
  modifyHeroList st' = pure $ st' & choices .~ heroList
    (st ^. popupDB)
    (Tx.unwords $ E.getEditContents $ st ^. input)

heroList :: HeroDB -> Text -> L.List Name Hero
heroList db search = L.list HeroSelection (filterHeroes (findAll db) search) 1

highlightPrefix :: Char
highlightPrefix = '>'
highlightPostfix :: Char
highlightPostfix = '<'

filterHeroes :: [Hero] -> Text -> Vector Hero
filterHeroes db search = fromList $ Fuzzy.original <$> found
 where
  found :: [Fuzzy.Fuzzy Hero Text]
  found = Fuzzy.filter search
                       db
                       (Tx.singleton highlightPrefix)
                       (Tx.singleton highlightPostfix)
                       (heroName)
                       False

highlight :: Text -> T.Widget a
highlight t = if Tx.any (/= highlightPrefix) t
  then txt t
  else txt before <+> (withAttr highlightAttr $ txt middle) <+> highlight
    (Tx.tail after)
 where
  (before, mid  ) = Tx.breakOn (Tx.singleton highlightPrefix) t
  (middle, after) = Tx.breakOn (Tx.singleton highlightPostfix) (Tx.tail mid)

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent st (T.VtyEvent ev) = case st ^. heroPopupState . showPopup of
  True -> M.continue =<< case ev of
    V.EvKey V.KEsc [] -> pure $ st & heroPopupState %~ showPopup .~ False
    V.EvKey V.KEnter [] -> pure $ st & heroPopupState %~ showPopup .~ False
    _ -> T.handleEventLensed st heroPopupState handleHeroPopupEvent ev
  False -> case ev of
    V.EvKey V.KEsc   [] -> M.halt st
    V.EvKey V.KEnter [] -> M.continue
      ( st
      & (heroPopupState . showPopup .~ True)
      . (heroPopupState .~ initialPopupState (st ^. dataSources))
      )
    _ -> M.continue st
appEvent st _ = M.continue st

initialPopupState :: DataSources -> HeroPopupState
initialPopupState ds = HeroPopupState False
                                      (E.editor HeroInput (Just 1) "")
                                      (heroList db Tx.empty)
                                      db
  where db = dataSourceHeroDB ds

initialState :: DataSources -> State
initialState ds = State ds (initialPopupState ds)

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (E.editAttr       , V.white `on` V.blue)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (listFocusedAttr  , V.blue `on` V.white)
  , (popupAttr        , V.white `on` V.yellow)
  , (highlightAttr    , V.yellow `on` V.brightCyan)
  ]

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = M.neverShowCursor

theApp :: M.App State e Name
theApp = M.App { M.appDraw         = drawUI
               , M.appChooseCursor = appCursor
               , M.appHandleEvent  = appEvent
               , M.appStartEvent   = return
               , M.appAttrMap      = const theMap
               }

start :: DataSources -> IO State
start ds = M.defaultMain theApp (initialState ds)
