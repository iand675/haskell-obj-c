{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationalErrorEvent@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationalErrorEvent
  ( MTROvenCavityOperationalStateClusterOperationalErrorEvent
  , IsMTROvenCavityOperationalStateClusterOperationalErrorEvent(..)
  , errorState
  , setErrorState
  , errorStateSelector
  , setErrorStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- errorState@
errorState :: IsMTROvenCavityOperationalStateClusterOperationalErrorEvent mtrOvenCavityOperationalStateClusterOperationalErrorEvent => mtrOvenCavityOperationalStateClusterOperationalErrorEvent -> IO (Id MTROvenCavityOperationalStateClusterErrorStateStruct)
errorState mtrOvenCavityOperationalStateClusterOperationalErrorEvent =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalErrorEvent errorStateSelector

-- | @- setErrorState:@
setErrorState :: (IsMTROvenCavityOperationalStateClusterOperationalErrorEvent mtrOvenCavityOperationalStateClusterOperationalErrorEvent, IsMTROvenCavityOperationalStateClusterErrorStateStruct value) => mtrOvenCavityOperationalStateClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrOvenCavityOperationalStateClusterOperationalErrorEvent value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalErrorEvent setErrorStateSelector (toMTROvenCavityOperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector '[] (Id MTROvenCavityOperationalStateClusterErrorStateStruct)
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector '[Id MTROvenCavityOperationalStateClusterErrorStateStruct] ()
setErrorStateSelector = mkSelector "setErrorState:"

