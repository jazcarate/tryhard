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

import qualified Brick.BChan                   as BChan
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
import qualified Control.Concurrent            as Co

import           Tryhard.Types
import           Tryhard.Stats
import           Tryhard.OpenDota.HeroDB        ( findAll
                                                , HeroDB
                                                )

import           Debug.Trace
import           Control.Monad                  ( void )
import           Data.Algebra.Free              ( collapse )
import           Tryhard.Stats.Mode             ( WinPercentage(WinPercentage)
                                                , Max(Max)
                                                )
import           Tryhard.Engine                 ( recomend )

heroNameOmni :: [Hero] -> Text -> [Hero]
heroNameOmni db query =
  Fuzzy.original <$> Fuzzy.filter query db mempty mempty heroName False

data Event = Event

data State = State
  { _dataSources ::  DataSources
  , _heroPopupState :: HeroPopupState
  , _teams :: TeamsState
  , _stats :: StatsState
  }

data Name = HeroInput | HeroSelection | MyTeamN | EnemyTeamN deriving (Ord, Show, Eq)
data StatsState = NotFetched | Loading | Loaded [Text]

data HeroPopupState = HeroPopupState
  { _showPopup :: Bool -- TODO move this to the State and use a sum-type
  , _input :: E.Editor Text Name
  , _choices :: L.List Name Hero
  , _popupDB :: HeroDB
  }

data TeamsState = TeamsState
  { _teamFocus :: F.FocusRing Name
  , _myTeam :: L.List Name Hero
  , _enemyTeam :: L.List Name Hero
  }

makeLenses ''State
makeLenses ''HeroPopupState
makeLenses ''TeamsState

popupAttr :: A.AttrName
popupAttr = "popup"

-- | hero choosing popup
heroPopup :: HeroPopupState -> T.Widget Name
heroPopup st = case st ^. showPopup of
  False -> emptyWidget
  True ->
    C.centerLayer
      $ withDefAttr popupAttr
      $ withBorderStyle BS.defaultBorderStyle
      $ B.borderWithLabel (str "Choose a hero")
      $ vBox [editor, results]
 where
  editor =
    (   str "Hero: "
    <+> (hLimit 30 $ vLimit 5 $ E.renderEditor (txt . Tx.unwords)
                                               True
                                               (st ^. input)
        )
    )

  results =
    L.renderList (\_ heroW -> txt $ heroName heroW) True (st ^. choices)

help :: T.Widget n
help = hBox $ toW <$> content
 where
  toW (graph, text) = txt graph <+> txt text
  content =
    [ ("^↑"  , "Select your team")
    , ("^↓"  , "Select enemy team")
    , ("⤶"   , "Add a hero")
    , ("DEL" , "Remove a hero")
    , ("^DEL", "Clear all heroes")
    ]

drawUI :: State -> [T.Widget Name]
drawUI st = [popup, context]
 where

  popup = heroPopup (st ^. heroPopupState)
  context =
    vBox [vLimit 1 $ C.center $ str "Tryhard", selecction, vLimit 1 $ help]

  selecction = hBox
    [ teamsUI (st ^. teams)
    , panel "strategy"       (str "foo")
    , panel "recomendations" recomendations
    ]
  recomendations = case st ^. stats of
    Loading    -> txt "Loading..."
    NotFetched -> emptyWidget
    Loaded s   -> vBox $ txt <$> s
  panel name inner =
    withBorderStyle BS.defaultBorderStyle $ B.borderWithLabel (str name) $ vBox
      [hBox [inner, fill ' '], fill ' ']

teamsUI :: TeamsState -> T.Widget Name
teamsUI st = vBox
  [ F.withFocusRing (st ^. teamFocus) (teamUI "Your team") (st ^. myTeam)
  , F.withFocusRing (st ^. teamFocus) (teamUI "Enemy team") (st ^. enemyTeam)
  ]

teamUI :: Text -> Bool -> L.List Name Hero -> T.Widget Name
teamUI label focus st =
  border . B.borderWithLabel (txt label) $ L.renderListWithIndex
    (\i _ heroW -> txt $ (Tx.pack $ show (i + 1)) <> ". " <> (heroName heroW))
    focus
    st
 where
  border = if focus
    then withBorderStyle BS.unicodeBold
    else withBorderStyle BS.defaultBorderStyle

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


filterHeroes :: [Hero] -> Text -> Vector Hero
filterHeroes db search = fromList $ Fuzzy.original <$> found
 where
  found :: [Fuzzy.Fuzzy Hero Text]
  found = Fuzzy.filter search db mempty mempty heroName False


selectedHero :: State -> Maybe Hero
selectedHero st =
  snd <$> L.listSelectedElement (st ^. heroPopupState . choices)

listAddAndFocus
  :: (L.Splittable t, Applicative t, Semigroup (t a), Foldable t)
  => a
  -> L.GenericList n t a
  -> L.GenericList n t a
listAddAndFocus e l = L.listMoveTo index $ L.listInsert index e l
  where index = 1 + (length $ L.listElements l)

listRemoveSeelected
  :: (L.Splittable t, Foldable t, Semigroup (t e))
  => L.GenericList n t e
  -> L.GenericList n t e
listRemoveSeelected l = case L.listSelectedElement l of
  Nothing     -> l
  Just (i, _) -> L.listRemove i l

data StatsEvent = NewStats [Text] deriving (Show)

appEvent
  :: Callback StatsEvent
  -> State
  -> T.BrickEvent Name StatsEvent
  -> T.EventM Name (T.Next State)
appEvent cb st (T.VtyEvent ev) = case st ^. heroPopupState . showPopup of
  True -> case ev of -- Popup
    V.EvKey V.KEsc [] -> M.continue $ st & heroPopupState %~ showPopup .~ False
    V.EvKey V.KEnter [] -> M.suspendAndResume $ do
      let st' =
            st
              & (case
                    ( F.focusGetCurrent (st ^. teams . teamFocus)
                    , selectedHero st
                    )
                  of -- TODO: Duplicated of the handleListEvent when the popup is closed 
                    (Just MyTeamN, Just h) ->
                      teams . myTeam %~ listAddAndFocus h
                    (Just EnemyTeamN, Just h) ->
                      teams . enemyTeam %~ listAddAndFocus h
                    _ -> id
                )
              & (heroPopupState %~ showPopup .~ False)
              & (stats .~ Loading)
      _ <- cb $ do
        let mStats = (collapse (Max . WinPercentage))
              <$> dataSourceMatchup (st' ^. dataSources)
        let myTeamComp =
              foldl (flip with) comp
                $   heroTC
                <$> (L.listElements (st' ^. teams . myTeam))
        let enemyTeamComp =
              foldl (flip against) comp
                $   heroTC
                <$> (L.listElements (st' ^. teams . enemyTeam))
        x <- runStats mStats (myTeamComp <> enemyTeamComp)
        let recomendatios = (Tx.pack . show) <$> recomend x
        pure (NewStats recomendatios)
      pure st'
    _ ->
      M.continue
        =<< T.handleEventLensed st heroPopupState handleHeroPopupEvent ev
  False -> case ev of --No popup
    V.EvKey V.KEsc   [] -> M.halt st
    V.EvKey V.KEnter [] -> M.continue
      ( st
      & (heroPopupState . showPopup .~ True)
      . (heroPopupState .~ initialPopupState (st ^. dataSources))
      )
    V.EvKey V.KDown [V.MShift] ->
      M.continue $ st & teams . teamFocus %~ F.focusSetCurrent EnemyTeamN
    V.EvKey V.KUp [V.MShift] ->
      M.continue $ st & teams . teamFocus %~ F.focusSetCurrent MyTeamN
    V.EvKey V.KDel [] ->
      M.continue $ case F.focusGetCurrent (st ^. teams . teamFocus) of -- TODO: sort of duplicated from popup closing
        Just MyTeamN    -> st & teams . myTeam %~ listRemoveSeelected
        Just EnemyTeamN -> st & teams . enemyTeam %~ listRemoveSeelected
        _               -> st
    V.EvKey V.KDel [V.MShift] ->
      M.continue
        $ st
        & (teams . myTeam %~ L.listClear)
        & (teams . enemyTeam %~ L.listClear)
    _ -> M.continue =<< case F.focusGetCurrent (st ^. teams . teamFocus) of
      Just MyTeamN ->
        T.handleEventLensed st (teams . myTeam) L.handleListEvent ev
      Just EnemyTeamN ->
        T.handleEventLensed st (teams . enemyTeam) L.handleListEvent ev
      _ -> pure st
appEvent _ st (T.AppEvent ev) = case ev of
  NewStats newStats -> M.continue $ st & stats .~ (Loaded newStats)
appEvent _ st _ = M.continue st

initialPopupState :: DataSources -> HeroPopupState
initialPopupState ds = HeroPopupState False
                                      (E.editor HeroInput (Just 1) "")
                                      (heroList db Tx.empty)
                                      db
  where db = dataSourceHeroDB ds

innitialTeamsState :: TeamsState
innitialTeamsState = TeamsState (F.focusRing [MyTeamN, EnemyTeamN])
                                (L.list MyTeamN mempty 1)
                                (L.list EnemyTeamN mempty 1)

innitialStatsState :: StatsState
innitialStatsState = NotFetched

initialState :: DataSources -> State
initialState ds =
  State ds (initialPopupState ds) (innitialTeamsState) innitialStatsState

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (E.editAttr               , V.white `on` V.blue)
  , (E.editFocusedAttr        , V.black `on` V.yellow)
  , (L.listSelectedFocusedAttr, V.blue `on` V.white)
  , (popupAttr                , V.white `on` V.yellow)
  ]

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = M.neverShowCursor

type Callback a = (IO a -> IO ())

theApp :: Callback StatsEvent -> M.App State StatsEvent Name
theApp cb = M.App { M.appDraw         = drawUI
                  , M.appChooseCursor = appCursor
                  , M.appHandleEvent  = appEvent cb
                  , M.appStartEvent   = return
                  , M.appAttrMap      = const theMap
                  }

callback :: BChan.BChan a -> IO a -> IO ()
callback chan a = void $ Co.forkIO $ a >>= BChan.writeBChan chan

start :: DataSources -> IO State
start ds = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan       <- BChan.newBChan 3
  M.customMain initialVty
               builder
               (Just chan)
               (theApp $ callback chan)
               (initialState ds)
