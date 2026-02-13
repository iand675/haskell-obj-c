{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateClusterStateChangeEvent@.
module ObjC.Matter.MTRBooleanStateClusterStateChangeEvent
  ( MTRBooleanStateClusterStateChangeEvent
  , IsMTRBooleanStateClusterStateChangeEvent(..)
  , stateValue
  , setStateValue
  , setStateValueSelector
  , stateValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stateValue@
stateValue :: IsMTRBooleanStateClusterStateChangeEvent mtrBooleanStateClusterStateChangeEvent => mtrBooleanStateClusterStateChangeEvent -> IO (Id NSNumber)
stateValue mtrBooleanStateClusterStateChangeEvent =
  sendMessage mtrBooleanStateClusterStateChangeEvent stateValueSelector

-- | @- setStateValue:@
setStateValue :: (IsMTRBooleanStateClusterStateChangeEvent mtrBooleanStateClusterStateChangeEvent, IsNSNumber value) => mtrBooleanStateClusterStateChangeEvent -> value -> IO ()
setStateValue mtrBooleanStateClusterStateChangeEvent value =
  sendMessage mtrBooleanStateClusterStateChangeEvent setStateValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateValue@
stateValueSelector :: Selector '[] (Id NSNumber)
stateValueSelector = mkSelector "stateValue"

-- | @Selector@ for @setStateValue:@
setStateValueSelector :: Selector '[Id NSNumber] ()
setStateValueSelector = mkSelector "setStateValue:"

