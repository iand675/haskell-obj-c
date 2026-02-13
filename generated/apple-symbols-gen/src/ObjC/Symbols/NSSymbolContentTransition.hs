{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for transitions that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
--
-- Generated bindings for @NSSymbolContentTransition@.
module ObjC.Symbols.NSSymbolContentTransition
  ( NSSymbolContentTransition
  , IsNSSymbolContentTransition(..)
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
new :: IO (Id NSSymbolContentTransition)
new  =
  do
    cls' <- getRequiredClass "NSSymbolContentTransition"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSSymbolContentTransition nsSymbolContentTransition => nsSymbolContentTransition -> IO (Id NSSymbolContentTransition)
init_ nsSymbolContentTransition =
  sendOwnedMessage nsSymbolContentTransition initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSSymbolContentTransition)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSymbolContentTransition)
initSelector = mkSelector "init"

