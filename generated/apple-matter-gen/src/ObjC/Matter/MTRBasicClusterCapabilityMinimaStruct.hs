{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterCapabilityMinimaStruct@.
module ObjC.Matter.MTRBasicClusterCapabilityMinimaStruct
  ( MTRBasicClusterCapabilityMinimaStruct
  , IsMTRBasicClusterCapabilityMinimaStruct(..)
  , caseSessionsPerFabric
  , setCaseSessionsPerFabric
  , subscriptionsPerFabric
  , setSubscriptionsPerFabric
  , caseSessionsPerFabricSelector
  , setCaseSessionsPerFabricSelector
  , setSubscriptionsPerFabricSelector
  , subscriptionsPerFabricSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- caseSessionsPerFabric@
caseSessionsPerFabric :: IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct => mtrBasicClusterCapabilityMinimaStruct -> IO (Id NSNumber)
caseSessionsPerFabric mtrBasicClusterCapabilityMinimaStruct =
  sendMessage mtrBasicClusterCapabilityMinimaStruct caseSessionsPerFabricSelector

-- | @- setCaseSessionsPerFabric:@
setCaseSessionsPerFabric :: (IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicClusterCapabilityMinimaStruct -> value -> IO ()
setCaseSessionsPerFabric mtrBasicClusterCapabilityMinimaStruct value =
  sendMessage mtrBasicClusterCapabilityMinimaStruct setCaseSessionsPerFabricSelector (toNSNumber value)

-- | @- subscriptionsPerFabric@
subscriptionsPerFabric :: IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct => mtrBasicClusterCapabilityMinimaStruct -> IO (Id NSNumber)
subscriptionsPerFabric mtrBasicClusterCapabilityMinimaStruct =
  sendMessage mtrBasicClusterCapabilityMinimaStruct subscriptionsPerFabricSelector

-- | @- setSubscriptionsPerFabric:@
setSubscriptionsPerFabric :: (IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicClusterCapabilityMinimaStruct -> value -> IO ()
setSubscriptionsPerFabric mtrBasicClusterCapabilityMinimaStruct value =
  sendMessage mtrBasicClusterCapabilityMinimaStruct setSubscriptionsPerFabricSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @caseSessionsPerFabric@
caseSessionsPerFabricSelector :: Selector '[] (Id NSNumber)
caseSessionsPerFabricSelector = mkSelector "caseSessionsPerFabric"

-- | @Selector@ for @setCaseSessionsPerFabric:@
setCaseSessionsPerFabricSelector :: Selector '[Id NSNumber] ()
setCaseSessionsPerFabricSelector = mkSelector "setCaseSessionsPerFabric:"

-- | @Selector@ for @subscriptionsPerFabric@
subscriptionsPerFabricSelector :: Selector '[] (Id NSNumber)
subscriptionsPerFabricSelector = mkSelector "subscriptionsPerFabric"

-- | @Selector@ for @setSubscriptionsPerFabric:@
setSubscriptionsPerFabricSelector :: Selector '[Id NSNumber] ()
setSubscriptionsPerFabricSelector = mkSelector "setSubscriptionsPerFabric:"

