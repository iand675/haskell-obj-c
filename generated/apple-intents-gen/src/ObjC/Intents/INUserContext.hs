{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUserContext@.
module ObjC.Intents.INUserContext
  ( INUserContext
  , IsINUserContext(..)
  , init_
  , becomeCurrent
  , becomeCurrentSelector
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

-- | @- init@
init_ :: IsINUserContext inUserContext => inUserContext -> IO (Id INUserContext)
init_ inUserContext =
  sendOwnedMessage inUserContext initSelector

-- | Each call replaces previous context object of the same underlying type. Use this only in your iOS/watchOS app. Do not try to set the user context as current from your Intents extension or Intents UI extension.
--
-- ObjC selector: @- becomeCurrent@
becomeCurrent :: IsINUserContext inUserContext => inUserContext -> IO ()
becomeCurrent inUserContext =
  sendMessage inUserContext becomeCurrentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INUserContext)
initSelector = mkSelector "init"

-- | @Selector@ for @becomeCurrent@
becomeCurrentSelector :: Selector '[] ()
becomeCurrentSelector = mkSelector "becomeCurrent"

