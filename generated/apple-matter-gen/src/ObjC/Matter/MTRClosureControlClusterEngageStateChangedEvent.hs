{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterEngageStateChangedEvent@.
module ObjC.Matter.MTRClosureControlClusterEngageStateChangedEvent
  ( MTRClosureControlClusterEngageStateChangedEvent
  , IsMTRClosureControlClusterEngageStateChangedEvent(..)
  , engageValue
  , setEngageValue
  , engageValueSelector
  , setEngageValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- engageValue@
engageValue :: IsMTRClosureControlClusterEngageStateChangedEvent mtrClosureControlClusterEngageStateChangedEvent => mtrClosureControlClusterEngageStateChangedEvent -> IO (Id NSNumber)
engageValue mtrClosureControlClusterEngageStateChangedEvent =
  sendMessage mtrClosureControlClusterEngageStateChangedEvent engageValueSelector

-- | @- setEngageValue:@
setEngageValue :: (IsMTRClosureControlClusterEngageStateChangedEvent mtrClosureControlClusterEngageStateChangedEvent, IsNSNumber value) => mtrClosureControlClusterEngageStateChangedEvent -> value -> IO ()
setEngageValue mtrClosureControlClusterEngageStateChangedEvent value =
  sendMessage mtrClosureControlClusterEngageStateChangedEvent setEngageValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @engageValue@
engageValueSelector :: Selector '[] (Id NSNumber)
engageValueSelector = mkSelector "engageValue"

-- | @Selector@ for @setEngageValue:@
setEngageValueSelector :: Selector '[Id NSNumber] ()
setEngageValueSelector = mkSelector "setEngageValue:"

