{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Tryhard.TUI where


import           Lens.Micro                     ( (.~)
                                                , (%~)
                                                , (&)
                                                , (^.)
                                                )
import           Lens.Micro.TH
import qualified Graphics.Vty                  as V
import qualified Graphics.Vty.Attributes       as VA

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
                                                , KeepHero(KeepHero)
                                                )
import           Tryhard.Engine                 ( resultHero
                                                , Result
                                                , recomend
                                                )

import qualified Data.Function                 as F

heroNameOmni :: [Hero] -> Text -> [Hero]
heroNameOmni db query =
  Fuzzy.original <$> Fuzzy.filter query db mempty mempty heroName False

data Event = Event

data State = State
  { _dataSources ::  DataSources
  , _heroPopupState :: HeroPopupState
  , _teams :: TeamsState
  , _stats :: L.List Name Result
  , _statsState :: StatsState
  , _panel :: F.FocusRing Name
  }

data Name = HeroInput | HeroSelection | MyTeamN | EnemyTeamN | BansN | TeamsPanel | RecomendationPanel deriving (Ord, Show, Eq)
data StatsState = NotFetched | Loading | Loaded

data HeroPopupState = HeroPopupState
  { _showPopup :: Bool -- TODO move this to the State and use a sum-type
  , _input :: E.Editor Text Name
  , _choices :: L.List Name Hero
  , _popupDB :: HeroDB
  }

data TeamsState = TeamsState -- TOdo one widget. Too many duplicated lines
  { _teamFocus :: F.FocusRing Name
  , _myTeam :: L.List Name Hero
  , _enemyTeam :: L.List Name Hero
  , _bans :: L.List Name Hero
  }

makeLenses ''State
makeLenses ''HeroPopupState
makeLenses ''TeamsState

instance (Named TeamsState Name) where
  getName _ = TeamsPanel

popupAttr :: A.AttrName
popupAttr = "popup"

bannedAttr :: A.AttrName
bannedAttr = "banned"

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
    [ ("^↑/^↓", "Move though teams and bans")
    , ("⤶"    , "Add a hero")
    , ("DEL"  , "Remove a hero")
    , ("^DEL" , "Clear all heroes")
    ]

drawUI :: State -> [T.Widget Name]
drawUI st = [popup, context]
 where
  popup = heroPopup (st ^. heroPopupState)
  context =
    vBox [vLimit 1 $ C.center $ str "Tryhard", selecction, vLimit 1 $ help]

  selecction = hBox
    [ F.withFocusRing (st ^. panel) teamsUI (st ^. teams)
    , F.withFocusRing (st ^. panel) recomendations (st ^. stats)
    , panel' "strategy" (str "foo")
    ]
  recomendations focus ls =
    (focusBorder focus)
      $ B.borderWithLabel (txt "Recomendation")
      $ case (st ^. statsState) of
          Loading    -> txt "Loading..." <+> fill ' '
          NotFetched -> txt "Add heroes to see the recomendations" <+> fill ' '
          Loaded     -> L.renderListWithIndex
            (\i _ recomened ->
              (if baned (resultHero recomened) then withAttr bannedAttr else id)
                $  txt
                $  Tx.pack
                $  show (i + 1)
                <> ". "
                <> (if baned (resultHero recomened) then "(banned) " else mempty
                   )
                <> (show recomened)
            )
            focus
            ls
  baned hero = hero `elem` L.listElements (st ^. teams . bans)
  panel' name inner =
    withBorderStyle BS.defaultBorderStyle $ B.borderWithLabel (str name) $ vBox
      [hBox [inner, fill ' '], fill ' ']

focusBorder :: Bool -> T.Widget n -> T.Widget n
focusBorder focus = if focus
  then withBorderStyle BS.unicodeBold
  else withBorderStyle BS.defaultBorderStyle

teamsUI :: Bool -> TeamsState -> T.Widget Name
teamsUI focus st = vBox
  [ F.withFocusRing (st ^. teamFocus) (teamUI "Your team" focus) (st ^. myTeam)
  , F.withFocusRing (st ^. teamFocus)
                    (teamUI "Enemy team" focus)
                    (st ^. enemyTeam)
  , F.withFocusRing (st ^. teamFocus) (teamUI "Bans" focus) (st ^. bans)
  ]

teamUI :: Text -> Bool -> Bool -> L.List Name Hero -> T.Widget Name
teamUI label outsideFocus focus st =
  (focusBorder focus) . B.borderWithLabel (txt label) $ L.renderListWithIndex
    (\i _ heroW -> txt $ (Tx.pack $ show (i + 1)) <> ". " <> (heroName heroW))
    (focus && outsideFocus)
    st

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


selectedHero :: L.List a Hero -> Maybe Hero
selectedHero st = snd <$> L.listSelectedElement st

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

data StatsEvent = NewStats [Result] deriving (Show)

shouldFetch :: State -> State -> Bool
shouldFetch a b = (cmp myTeam) a b || (cmp enemyTeam) a b
 where
  cmp getter = (/=) `F.on` (L.listElements . (\st -> st ^. teams . getter))

appEvent'
  :: Callback StatsEvent
  -> State
  -> T.BrickEvent Name StatsEvent
  -> T.EventM Name (T.Next State)
appEvent' cb st ev = do
  continuationSt <- appEvent st ev
  case continuationSt of
    Halt st' -> M.halt st'
    Next st' -> if not $ shouldFetch st st'
      then M.continue st'
      else M.suspendAndResume $ do
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
          let recomendatios = recomend x
          pure (NewStats recomendatios)
        pure $ st' & (statsState .~ Loading)



data Continuation a = Next a | Halt a

continue :: a -> T.EventM Name (Continuation a)
continue = pure . Next

halt :: a -> T.EventM Name (Continuation a)
halt = pure . Halt

appEvent
  :: State -> T.BrickEvent Name StatsEvent -> T.EventM Name (Continuation State)
appEvent st (T.VtyEvent ev) = case st ^. heroPopupState . showPopup of
  True -> case ev of -- Popup
    V.EvKey V.KEsc [] -> continue $ st & heroPopupState %~ showPopup .~ False
    V.EvKey V.KEnter [] ->
      continue
        $ st
        & (case
              ( F.focusGetCurrent (st ^. teams . teamFocus)
              , selectedHero (st ^. heroPopupState . choices)
              )
            of -- TODO: Duplicated of the handleListEvent when the popup is closed 
              (Just MyTeamN, Just h) -> teams . myTeam %~ listAddAndFocus h
              (Just EnemyTeamN, Just h) ->
                teams . enemyTeam %~ listAddAndFocus h
              (Just BansN, Just h) -> teams . bans %~ listAddAndFocus h
              _                    -> id
          )
        & (heroPopupState %~ showPopup .~ False)
    _ ->
      continue =<< T.handleEventLensed st heroPopupState handleHeroPopupEvent ev
  False -> case (F.focusGetCurrent (st ^. panel), ev) of
    (_, V.EvKey V.KEsc []) -> halt st

    (_, V.EvKey V.KLeft [V.MShift]) ->
      continue $ st & panel %~ F.focusSetCurrent TeamsPanel
    (_, V.EvKey V.KRight [V.MShift]) ->
      continue $ st & panel %~ F.focusSetCurrent RecomendationPanel
    (_, V.EvKey V.KDown [V.MShift]) ->
      continue $ st & teams . teamFocus %~ F.focusNext
    (_, V.EvKey V.KUp [V.MShift]) ->
      continue $ st & teams . teamFocus %~ F.focusPrev

    (Just TeamsPanel, _) -> case ev of
      V.EvKey V.KEnter [] -> continue
        ( st
        & (heroPopupState . showPopup .~ True)
        . (heroPopupState .~ initialPopupState (st ^. dataSources))
        )
      V.EvKey V.KDel [] ->
        continue $ case F.focusGetCurrent (st ^. teams . teamFocus) of -- TODO: sort of duplicated from popup closing
          Just MyTeamN    -> st & teams . myTeam %~ listRemoveSeelected
          Just EnemyTeamN -> st & teams . enemyTeam %~ listRemoveSeelected
          Just BansN      -> st & teams . bans %~ listRemoveSeelected
          _               -> st
      V.EvKey V.KDel [V.MShift] ->
        continue
          $ st
          & (teams . myTeam %~ L.listClear)
          & (teams . enemyTeam %~ L.listClear)
          & (teams . bans %~ L.listClear)
      _ -> continue =<< case F.focusGetCurrent (st ^. teams . teamFocus) of
        Just MyTeamN ->
          T.handleEventLensed st (teams . myTeam) L.handleListEvent ev
        Just EnemyTeamN ->
          T.handleEventLensed st (teams . enemyTeam) L.handleListEvent ev
        Just BansN ->
          T.handleEventLensed st (teams . enemyTeam) L.handleListEvent ev
        _ -> pure st
    (Just RecomendationPanel, _) -> case (st ^. statsState, ev) of
      (Loaded, V.EvKey V.KEnter []) ->
        continue
          $ st
          & (case
                ( F.focusGetCurrent (st ^. teams . teamFocus)
                , selectedHero (resultHero <$> st ^. stats)
                )
              of -- TODO: Duplicated of the handleListEvent when the popup is closed 
                (Just MyTeamN, Just h) -> teams . myTeam %~ listAddAndFocus h
                (Just EnemyTeamN, Just h) ->
                  teams . enemyTeam %~ listAddAndFocus h
                (Just BansN, Just h) -> teams . bans %~ listAddAndFocus h
                _                    -> id
            )
          & (heroPopupState %~ showPopup .~ False)
      (Loaded, _) ->
        continue =<< T.handleEventLensed st stats L.handleListEvent ev
      _ -> continue st
    _ -> continue st
appEvent st (T.AppEvent ev) = case ev of
  NewStats newStats ->
    continue
      $ st
      & (statsState .~ Loaded)
      & (stats %~ L.listReplace (fromList newStats) Nothing)
appEvent st _ = continue st

initialPopupState :: DataSources -> HeroPopupState
initialPopupState ds = HeroPopupState False
                                      (E.editor HeroInput (Just 1) "")
                                      (heroList db Tx.empty)
                                      db
  where db = dataSourceHeroDB ds

innitialTeamsState :: TeamsState
innitialTeamsState = TeamsState (F.focusRing [MyTeamN, EnemyTeamN, BansN])
                                (L.list MyTeamN mempty 1)
                                (L.list EnemyTeamN mempty 1)
                                (L.list BansN mempty 1)


innitialStatsState :: StatsState
innitialStatsState = NotFetched

initialState :: DataSources -> State
initialState ds = State ds
                        (initialPopupState ds)
                        innitialTeamsState
                        (L.list RecomendationPanel mempty 1)
                        innitialStatsState
                        (F.focusRing [TeamsPanel, RecomendationPanel])

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (E.editAttr               , V.white `on` V.blue)
  , (E.editFocusedAttr        , V.black `on` V.yellow)
  , (L.listSelectedFocusedAttr, V.blue `on` V.black)
  , (popupAttr                , V.white `on` V.yellow)
  , (bannedAttr               , V.withStyle mempty VA.italic)
  ]

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = M.neverShowCursor

type Callback a = (IO a -> IO ())

theApp :: Callback StatsEvent -> M.App State StatsEvent Name
theApp cb = M.App { M.appDraw         = drawUI
                  , M.appChooseCursor = appCursor
                  , M.appHandleEvent  = appEvent' cb
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
