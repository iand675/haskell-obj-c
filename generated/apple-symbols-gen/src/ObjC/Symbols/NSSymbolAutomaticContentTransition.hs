{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The default symbol transition, resolves to a particular transition in a context-sensitive manner.
--
-- Generated bindings for @NSSymbolAutomaticContentTransition@.
module ObjC.Symbols.NSSymbolAutomaticContentTransition
  ( NSSymbolAutomaticContentTransition
  , IsNSSymbolAutomaticContentTransition(..)
  , transition
  , transitionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default automatic transition, determined by the system.
--
-- ObjC selector: @+ transition@
transition :: IO (Id NSSymbolAutomaticContentTransition)
transition  =
  do
    cls' <- getRequiredClass "NSSymbolAutomaticContentTransition"
    sendClassMessage cls' transitionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transition@
transitionSelector :: Selector '[] (Id NSSymbolAutomaticContentTransition)
transitionSelector = mkSelector "transition"

