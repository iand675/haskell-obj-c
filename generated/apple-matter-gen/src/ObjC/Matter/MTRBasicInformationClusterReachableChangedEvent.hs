{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterReachableChangedEvent@.
module ObjC.Matter.MTRBasicInformationClusterReachableChangedEvent
  ( MTRBasicInformationClusterReachableChangedEvent
  , IsMTRBasicInformationClusterReachableChangedEvent(..)
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
reachableNewValue :: IsMTRBasicInformationClusterReachableChangedEvent mtrBasicInformationClusterReachableChangedEvent => mtrBasicInformationClusterReachableChangedEvent -> IO (Id NSNumber)
reachableNewValue mtrBasicInformationClusterReachableChangedEvent =
  sendMessage mtrBasicInformationClusterReachableChangedEvent reachableNewValueSelector

-- | @- setReachableNewValue:@
setReachableNewValue :: (IsMTRBasicInformationClusterReachableChangedEvent mtrBasicInformationClusterReachableChangedEvent, IsNSNumber value) => mtrBasicInformationClusterReachableChangedEvent -> value -> IO ()
setReachableNewValue mtrBasicInformationClusterReachableChangedEvent value =
  sendMessage mtrBasicInformationClusterReachableChangedEvent setReachableNewValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reachableNewValue@
reachableNewValueSelector :: Selector '[] (Id NSNumber)
reachableNewValueSelector = mkSelector "reachableNewValue"

-- | @Selector@ for @setReachableNewValue:@
setReachableNewValueSelector :: Selector '[Id NSNumber] ()
setReachableNewValueSelector = mkSelector "setReachableNewValue:"

