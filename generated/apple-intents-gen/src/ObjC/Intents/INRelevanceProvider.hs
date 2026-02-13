{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A relevance provider represents a piece of relevance information that can be used by Siri when predicting relevant shortcuts.
--
-- Generated bindings for @INRelevanceProvider@.
module ObjC.Intents.INRelevanceProvider
  ( INRelevanceProvider
  , IsINRelevanceProvider(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Note: @INRelevanceProvider@ should not be initilaized directly. Use one of the subclasses instead.
--
-- ObjC selector: @- init@
init_ :: IsINRelevanceProvider inRelevanceProvider => inRelevanceProvider -> IO (Id INRelevanceProvider)
init_ inRelevanceProvider =
  sendOwnedMessage inRelevanceProvider initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRelevanceProvider)
initSelector = mkSelector "init"

