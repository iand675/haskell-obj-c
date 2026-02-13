{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for effects that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
--
-- Generated bindings for @NSSymbolEffect@.
module ObjC.Symbols.NSSymbolEffect
  ( NSSymbolEffect
  , IsNSSymbolEffect(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSSymbolEffect)
new  =
  do
    cls' <- getRequiredClass "NSSymbolEffect"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSSymbolEffect nsSymbolEffect => nsSymbolEffect -> IO (Id NSSymbolEffect)
init_ nsSymbolEffect =
  sendOwnedMessage nsSymbolEffect initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSSymbolEffect)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSymbolEffect)
initSelector = mkSelector "init"

