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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Note: @INRelevanceProvider@ should not be initilaized directly. Use one of the subclasses instead.
--
-- ObjC selector: @- init@
init_ :: IsINRelevanceProvider inRelevanceProvider => inRelevanceProvider -> IO (Id INRelevanceProvider)
init_ inRelevanceProvider  =
  sendMsg inRelevanceProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

