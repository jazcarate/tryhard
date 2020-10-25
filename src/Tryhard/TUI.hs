{-# LANGUAGE TemplateHaskell #-}

module Tryhard.TUI where

import           Lens.Micro
import           Lens.Micro.TH
import qualified Graphics.Vty                  as V

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Core             ( (<+>)
                                                , hLimit
                                                , vLimit
                                                , str
                                                , txt
                                                , (<=>)
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Edit            as E
import qualified Brick.AttrMap                 as A
import qualified Brick.Focus                   as F
import           Brick.Util                     ( on )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Tx


import           Tryhard.Types                  ( Hero
                                                , heroName
                                                )
import qualified Text.Fuzzy                    as Fuzzy

heroNameOmni :: [Hero] -> Text -> [Hero]
heroNameOmni db query =
  Fuzzy.original <$> Fuzzy.filter query db mempty mempty heroName False


data Name = Edit1
          deriving (Ord, Show, Eq)

data State =
    State {_focusRing :: F.FocusRing Name, _input :: E.Editor Text Name }

makeLenses ''State

drawUI :: [Hero] -> State -> [T.Widget Name]
drawUI heroes st = [ui]
 where
  e1 = F.withFocusRing (st ^. focusRing)
                       (E.renderEditor (txt . Tx.unlines))
                       (st ^. input)

  ui =
    C.center
      $   (str "Hero: " <+> (hLimit 30 $ vLimit 5 e1))
      <=> (   str "Result: "
          <+> (txt $ Tx.unlines $ heroName <$> heroNameOmni
                heroes
                (head $ E.getEditContents (st ^. input))
              )
          )

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent st (T.VtyEvent ev) = case ev of
  V.EvKey V.KEsc         [] -> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
  V.EvKey V.KBackTab     [] -> M.continue $ st & focusRing %~ F.focusPrev

  _ -> M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
    Just Edit1 -> T.handleEventLensed st input E.handleEditorEvent ev
    Nothing    -> return st
appEvent st _ = M.continue st

initialState :: State
initialState = State (F.focusRing [Edit1]) (E.editor Edit1 (Just 1) "")

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (E.editAttr       , V.white `on` V.blue)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  ]

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: [Hero] -> M.App State e Name
theApp heroes = M.App { M.appDraw         = drawUI heroes
                      , M.appChooseCursor = appCursor
                      , M.appHandleEvent  = appEvent
                      , M.appStartEvent   = return
                      , M.appAttrMap      = const theMap
                      }

start :: [Hero] -> IO State
start heroes' = M.defaultMain (theApp heroes') initialState
