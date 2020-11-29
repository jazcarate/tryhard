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
import qualified Data.Vector                   as Vec
import qualified Control.Concurrent            as Co
import qualified Tryhard.TUI.Popup             as P

import           Tryhard.Types
import           Tryhard.Stats
import           Tryhard.OpenDota.HeroDB        ( findAll
                                                , HeroDB
                                                )

import           Control.Monad                  ( void )
import           Data.Algebra.Free              ( collapse )
import           Tryhard.Stats.Mode             ( numberOfMatches
                                                , unKeepHeroValue
                                                , WinPercentage(WinPercentage)
                                                , invert
                                                , ignore
                                                , Max(Max)
                                                , Sum(Sum)
                                                )
import           Tryhard.Engine                 ( resultHero
                                                , Result
                                                , recomend
                                                )
import           Data.Char                      ( chr
                                                , ord
                                                )
import qualified Data.Function                 as F
import           Data.List                      ( sort )

heroNameOmni :: [Hero] -> Text -> [Hero]
heroNameOmni db query =
  Fuzzy.original <$> Fuzzy.filter query db mempty mempty heroName False

data Event = Event

data Strategy =  TeamCombos | AllCombos | NumberOfMatches | AverageWinPercentage | MaxWinPercentage | TeamLegged deriving (Eq, Enum, Bounded)

data State = State
  { _dataSources ::  DataSources
  , _heroSelect :: P.Popup HeroSelectState
  , _help :: P.Popup ()
  , _teams :: TeamsState
  , _recomendations :: L.List Name Result
  , _recomendationsState :: StatsState --TODO merge _recomendationsState and _recomendations
  , _strategies :: L.List Name Strategy
  , _panel :: F.FocusRing Name
  }

data Name = HeroInput | HeroSelection | MyTeamN | EnemyTeamN | BansN | TeamsPanel | RecomendationPanel | StrategyPanel deriving (Ord, Show, Eq)
data StatsState = NotFetched | Loading | Loaded

data HeroSelectState = HeroSelectState
  { _input :: E.Editor Text Name
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
makeLenses ''HeroSelectState
makeLenses ''TeamsState

instance (Named TeamsState Name) where
  getName _ = TeamsPanel

popupAttr :: A.AttrName
popupAttr = "popup"

bannedAttr :: A.AttrName
bannedAttr = "banned"

-- | Help popup
helpPopup :: () -> T.Widget Name
helpPopup _ =
  C.centerLayer
    $   withDefAttr popupAttr
    $   withBorderStyle BS.unicodeBold
    $   B.borderWithLabel (str "Help")
    $   vBox
    $   C.center
    <$> [txt "TODO help"]

-- | hero choosing popup
heroPopup :: HeroSelectState -> T.Widget Name
heroPopup st =
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

status :: State -> T.Widget n
status st = hBox $ toW <$> (content <> stateAware)
 where
  toW (graph, text) = txt graph <+> txt ". " <+> txtWrap text
  content =
    [ ("^h"   , "Help")
    , ("^↑/^↓", "Move though teams and bans")
    , ("^←/^→", "Move though panels")
    , ("a-f"  , "Change strategy")
    , ("^DEL" , "Reset")
    ]
  stateAware = case F.focusGetCurrent (st ^. panel) of
    Just TeamsPanel         -> [("⤶", "Add a hero"), ("DEL", "Remove a hero")]
    Just RecomendationPanel -> [("⤶", "Add selected hero")]
    _                       -> mempty

-- TODO: It's not bounded, but coudn't come up with a better name. "peek"?
renderListWithIndexBounded
  :: (Ord n, Show n)
  => (Int -> Bool -> a -> T.Widget n)
  -> Bool
  -> L.List n a
  -> T.Widget n
renderListWithIndexBounded drawElem foc l =
  topHelper <=> L.renderListWithIndex drawElem foc l <=> bottomHelper
 where
  threshold = 3
  elements  = Vec.indexed $ L.listElements l
  items     = length elements
  selectedI = maybe 0 id $ L.listSelected l
  drawElem' (i, e) = drawElem i False e

  topHelper = if selectedI > threshold
    then
      (vBox $ Vec.toList $ drawElem' <$> Vec.take threshold elements)
        <=> B.hBorder
    else emptyWidget
  bottomHelper = if selectedI < items - threshold
    then
      B.hBorder
        <=> (   vBox
            $   Vec.toList
            $   drawElem'
            <$> Vec.drop (items - threshold) elements
            )
    else emptyWidget

drawUI :: State -> [T.Widget Name]
drawUI st =
  [ P.withPopup emptyWidget helpPopup (st ^. help)
  , P.withPopup emptyWidget heroPopup (st ^. heroSelect)
  , context
  ]
 where
  context =
    vBox [vLimit 1 $ C.center $ str "Tryhard", selecction, vLimit 1 $ status st]

  selecction = hBox
    [ F.withFocusRing (st ^. panel) teamsUI (st ^. teams)
    , F.withFocusRing (st ^. panel) (recomendationsUI st) (st ^. recomendations)
    , F.withFocusRing (st ^. panel) strategyUI (st ^. strategies)
    ]

focusBorder :: Bool -> T.Widget n -> T.Widget n
focusBorder focus = if focus
  then withBorderStyle BS.unicodeBold
  else withBorderStyle BS.defaultBorderStyle

strategyUI :: (Ord n, Show n) => Bool -> L.List n Strategy -> T.Widget n
strategyUI focus ls =
  (focusBorder focus)
    $ B.borderWithLabel (txt "S1trategies")
    $ L.renderListWithIndex
        (\i _ t -> str ([chr (ord 'a' + i)]) <+> txt ". " <+> strategyHelp t)
        focus
        ls

recomendationsUI
  :: (Ord n, Show n) => State -> Bool -> L.List n Result -> T.Widget n
recomendationsUI st focus ls =
  (focusBorder focus)
    $ B.borderWithLabel (txt "Recomendation")
    $ case (st ^. recomendationsState) of
        Loading    -> txt "Loading..." <+> fill ' '
        NotFetched -> txt "Add heroes to see the recomendations" <+> fill ' '
        Loaded     -> renderListWithIndexBounded
          (\i _ recomened ->
            (if baned (resultHero recomened) then withAttr bannedAttr else id)
              $  txt
              $  Tx.pack
              $  show (i + 1)
              <> ". "
              <> (if baned (resultHero recomened) then "(banned) " else mempty)
              <> (show recomened)
          )
          focus
          ls
  where baned hero = hero `elem` L.listElements (st ^. teams . bans)

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
  :: V.Event -> HeroSelectState -> T.EventM Name HeroSelectState
handleHeroPopupEvent ev st =
  T.handleEventLensed st choices L.handleListEvent ev
    >>= (\st' -> T.handleEventLensed st' input E.handleEditorEvent ev)
    >>= (\st' -> conditionalModifyHeroList st')


 where
  conditionalModifyHeroList :: HeroSelectState -> T.EventM Name HeroSelectState
  conditionalModifyHeroList st' =
    if E.getEditContents (st ^. input) == E.getEditContents (st' ^. input)
      then pure st'
      else modifyHeroList st'

  modifyHeroList :: HeroSelectState -> T.EventM Name HeroSelectState
  modifyHeroList st' = pure $ st' & choices .~ heroList
    (st ^. popupDB)
    (Tx.unwords $ E.getEditContents $ st ^. input)

heroList :: HeroDB -> Text -> L.List Name Hero
heroList db search = L.list HeroSelection (filterHeroes (findAll db) search) 1


filterHeroes :: [Hero] -> Text -> Vec.Vector Hero
filterHeroes db search = Vec.fromList $ sort $ Fuzzy.original <$> found
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

toComp :: TeamsState -> MatchComp
toComp t = (foldTC with myTeam) <> (foldTC against enemyTeam)
 where
  foldTC teamF ls =
    foldl (flip teamF) comp $ heroTC <$> (L.listElements (t ^. ls))

shouldFetch :: State -> State -> Bool
shouldFetch a b =
  (mempty /= toComp (b ^. teams))
    && ((cmp myTeam) a b || (cmp enemyTeam) a b || (cmpStrat a b))
 where
  cmp getter = (/=) `F.on` (L.listElements . (\st -> st ^. teams . getter))
  cmpStrat = (/=) `F.on` (L.listSelectedElement . (\st -> st ^. strategies))

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
          let strategy = maybe MaxWinPercentage snd
                $ L.listSelectedElement (st' ^. strategies)
          recomendatios <- stats (st' ^. dataSources) strategy (st' ^. teams)
          pure (NewStats recomendatios)
        pure
          $ st'
          & (recomendationsState .~ Loading)
          & (recomendations %~ L.listClear)

stats :: DataSources -> Strategy -> TeamsState -> IO [Result]
stats ds s teamState = case s of
  TeamLegged ->
    run (myTeamComp matchComp) (collapse Sum <$> dataSourceNumberOfLegs ds)
  MaxWinPercentage -> run
    matchComp
    (   (collapse
          (Max . (\t -> (\x -> ignore $ invert $ WinPercentage <$> x) <$> t))
        )
    <$> dataSourceMatchup ds
    )
  AverageWinPercentage -> run
    matchComp
    (   (collapse (Sum . WinPercentage . ignore . unKeepHeroValue))
    <$> dataSourceMatchup ds
    )
  TeamCombos      -> run (myTeamComp matchComp) (ByWith <$> dataSourceCombo ds)
  AllCombos       -> run matchComp (ByWith <$> dataSourceCombo ds)
  NumberOfMatches -> run
    matchComp
    (   (collapse (Sum . numberOfMatches . ignore . unKeepHeroValue))
    <$> dataSourceMatchup ds
    )
 where
  matchComp = toComp teamState
  run :: (ToIO m, Ord res, Show res) => MatchComp -> Stats m res -> IO [Result]
  run who what = (lift $ runStats what who) >>= (pure . recomend)

data Continuation a = Next a | Halt a

continue :: a -> T.EventM Name (Continuation a)
continue = pure . Next

halt :: a -> T.EventM Name (Continuation a)
halt = pure . Halt

appEvent
  :: State -> T.BrickEvent Name StatsEvent -> T.EventM Name (Continuation State)
appEvent st (T.VtyEvent ev) = case st ^. heroSelect . P.shown of
  True -> case ev of -- Popup
    V.EvKey V.KEsc [] -> continue $ st & heroSelect %~ P.hide
    V.EvKey V.KEnter [] ->
      continue
        $ st
        & (case
              ( F.focusGetCurrent (st ^. teams . teamFocus)
              , selectedHero (st ^. heroSelect . P.inner . choices)
              )
            of -- TODO: Duplicated of the handleListEvent when the popup is closed 
              (Just MyTeamN, Just h) -> teams . myTeam %~ listAddAndFocus h
              (Just EnemyTeamN, Just h) ->
                teams . enemyTeam %~ listAddAndFocus h
              (Just BansN, Just h) -> teams . bans %~ listAddAndFocus h
              _                    -> id
          )
        & (heroSelect %~ P.hide)
    _ ->
      continue
        =<< T.handleEventLensed st
                                (heroSelect . P.inner)
                                handleHeroPopupEvent
                                ev
  False -> case (F.focusGetCurrent (st ^. panel), ev) of
    (_, V.EvKey V.KEsc []          ) -> halt st


    (_, V.EvKey (V.KChar 'H') []   ) -> continue $ st & help %~ P.toggle
    (_, V.EvKey V.KRight [V.MShift]) -> continue $ st & panel %~ F.focusNext
    (_, V.EvKey V.KLeft [V.MShift] ) -> continue $ st & panel %~ F.focusPrev
    (_, V.EvKey V.KDown [V.MShift]) ->
      continue $ st & teams . teamFocus %~ F.focusNext
    (_, V.EvKey V.KUp [V.MShift]) ->
      continue $ st & teams . teamFocus %~ F.focusPrev

    (_, V.EvKey (V.KChar c) []) ->
      if i > 0 && i < (length $ L.listElements $ st ^. strategies)
        then continue $ st & strategies %~ L.listMoveTo i
        else continue st
      where i = (ord c - ord 'a')

    (_, V.EvKey V.KDel [V.MShift]) ->
      continue
        $ st
        & (teams . myTeam %~ L.listClear)
        & (teams . enemyTeam %~ L.listClear)
        & (teams . bans %~ L.listClear)
        & (recomendations %~ L.listClear)

    (Just TeamsPanel, _) -> case ev of
      V.EvKey V.KEnter [] -> continue
        ( st
        & (heroSelect %~ P.show)
        . (heroSelect .~ P.popup (initialPopupState (st ^. dataSources)))
        )
      V.EvKey V.KDel [] ->
        continue $ case F.focusGetCurrent (st ^. teams . teamFocus) of -- TODO: sort of duplicated from popup closing
          Just MyTeamN    -> st & teams . myTeam %~ listRemoveSeelected
          Just EnemyTeamN -> st & teams . enemyTeam %~ listRemoveSeelected
          Just BansN      -> st & teams . bans %~ listRemoveSeelected
          _               -> st
      _ -> continue =<< case F.focusGetCurrent (st ^. teams . teamFocus) of
        Just MyTeamN ->
          T.handleEventLensed st (teams . myTeam) L.handleListEvent ev
        Just EnemyTeamN ->
          T.handleEventLensed st (teams . enemyTeam) L.handleListEvent ev
        Just BansN ->
          T.handleEventLensed st (teams . enemyTeam) L.handleListEvent ev
        _ -> pure st
    (Just RecomendationPanel, _) -> case (st ^. recomendationsState, ev) of
      (Loaded, V.EvKey V.KEnter []) ->
        continue
          $ st
          & (case
                ( F.focusGetCurrent (st ^. teams . teamFocus)
                , selectedHero (resultHero <$> st ^. recomendations)
                )
              of -- TODO: Duplicated of the handleListEvent when the popup is closed 
                (Just MyTeamN, Just h) -> teams . myTeam %~ listAddAndFocus h
                (Just EnemyTeamN, Just h) ->
                  teams . enemyTeam %~ listAddAndFocus h
                (Just BansN, Just h) -> teams . bans %~ listAddAndFocus h
                _                    -> id
            )
          & (heroSelect %~ P.show)
      (Loaded, _) ->
        continue =<< T.handleEventLensed st recomendations L.handleListEvent ev
      _ -> continue st
    (Just StrategyPanel, _) -> case ev of
      _ -> continue =<< T.handleEventLensed st strategies L.handleListEvent ev
    _ -> continue st
appEvent st (T.AppEvent ev) = case ev of
  NewStats newStats ->
    continue
      $ st
      & (recomendationsState .~ Loaded)
      & (recomendations %~ L.listReplace (Vec.fromList newStats) Nothing)
appEvent st _ = continue st

initialPopupState :: DataSources -> HeroSelectState
initialPopupState ds = HeroSelectState (E.editor HeroInput (Just 1) "")
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
initialState ds = State
  { _dataSources         = ds
  , _heroSelect          = P.popup (initialPopupState ds)
  , _help                = P.popup ()
  , _teams               = innitialTeamsState
  , _strategies          = (L.list StrategyPanel innitialStrategies 1)
  , _recomendations      = (L.list RecomendationPanel mempty 1)
  , _recomendationsState = innitialStatsState
  , _panel = (F.focusRing [TeamsPanel, RecomendationPanel, StrategyPanel])
  }


strategyHelp :: Strategy -> T.Widget n
strategyHelp st = case st of
  MaxWinPercentage     -> txt "Max win percentage"
  AverageWinPercentage -> txt "Average win percentage"
  TeamLegged           -> txt "your team legs"
  TeamCombos           -> txt "Combos of your team"
  AllCombos            -> txt "Combos al full composition"
  NumberOfMatches      -> txt "number of matches"

innitialStrategies :: Vec.Vector Strategy
innitialStrategies = Vec.fromList [(minBound :: Strategy) ..]

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (E.editAttr               , V.white `on` V.blue)
  , (E.editFocusedAttr        , V.black `on` V.yellow)
  , (L.listSelectedAttr       , V.blue `on` V.yellow)
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
