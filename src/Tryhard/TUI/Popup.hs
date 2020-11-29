{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Tryhard.TUI.Popup where

import           Lens.Micro                     ( Lens'
                                                , (.~)
                                                , (%~)
                                                , (&)
                                                , (^.)
                                                )
import           Lens.Micro.TH

data Popup st = Popup
    { _shown :: Bool
    , _st :: st
    }

makeLenses ''Popup

instance Functor Popup where
  fmap = (%~) st

-- | Render the popup conditionaly
withPopup :: a -> (st -> a) -> Popup st -> a
withPopup def f p = case p ^. shown of
  False -> def
  True  -> f $ p ^. st

toggle :: Popup st -> Popup st
toggle p = p & shown %~ not

set :: Bool -> Popup st -> Popup st
set newVal p = p & shown .~ newVal

popup :: st -> Popup st
popup innerState = Popup { _shown = False, _st = innerState }

show :: Popup st -> Popup st
show = set True

hide :: Popup st -> Popup st
hide = set False

inner :: Lens' (Popup st) st
inner = st
