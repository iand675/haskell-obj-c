{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetRideStatusIntent@.
module ObjC.Intents.INGetRideStatusIntent
  ( INGetRideStatusIntent
  , IsINGetRideStatusIntent(..)
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

-- | @- init@
init_ :: IsINGetRideStatusIntent inGetRideStatusIntent => inGetRideStatusIntent -> IO (Id INGetRideStatusIntent)
init_ inGetRideStatusIntent =
  sendOwnedMessage inGetRideStatusIntent initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INGetRideStatusIntent)
initSelector = mkSelector "init"

