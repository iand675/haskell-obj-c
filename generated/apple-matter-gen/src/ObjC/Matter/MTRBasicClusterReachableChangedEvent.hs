{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterReachableChangedEvent@.
module ObjC.Matter.MTRBasicClusterReachableChangedEvent
  ( MTRBasicClusterReachableChangedEvent
  , IsMTRBasicClusterReachableChangedEvent(..)
  , reachableNewValue
  , setReachableNewValue
  , reachableNewValueSelector
  , setReachableNewValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reachableNewValue@
reachableNewValue :: IsMTRBasicClusterReachableChangedEvent mtrBasicClusterReachableChangedEvent => mtrBasicClusterReachableChangedEvent -> IO (Id NSNumber)
reachableNewValue mtrBasicClusterReachableChangedEvent =
  sendMessage mtrBasicClusterReachableChangedEvent reachableNewValueSelector

-- | @- setReachableNewValue:@
setReachableNewValue :: (IsMTRBasicClusterReachableChangedEvent mtrBasicClusterReachableChangedEvent, IsNSNumber value) => mtrBasicClusterReachableChangedEvent -> value -> IO ()
setReachableNewValue mtrBasicClusterReachableChangedEvent value =
  sendMessage mtrBasicClusterReachableChangedEvent setReachableNewValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reachableNewValue@
reachableNewValueSelector :: Selector '[] (Id NSNumber)
reachableNewValueSelector = mkSelector "reachableNewValue"

-- | @Selector@ for @setReachableNewValue:@
setReachableNewValueSelector :: Selector '[Id NSNumber] ()
setReachableNewValueSelector = mkSelector "setReachableNewValue:"

