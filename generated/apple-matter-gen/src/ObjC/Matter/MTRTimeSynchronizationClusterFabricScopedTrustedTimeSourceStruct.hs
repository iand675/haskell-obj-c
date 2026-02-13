{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct
  ( MTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct
  , IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct(..)
  , nodeID
  , setNodeID
  , endpoint
  , setEndpoint
  , endpointSelector
  , nodeIDSelector
  , setEndpointSelector
  , setNodeIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- nodeID@
nodeID :: IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> IO (Id NSNumber)
nodeID mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct =
  sendMessage mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> value -> IO ()
setNodeID mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct value =
  sendMessage mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct setNodeIDSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> IO (Id NSNumber)
endpoint mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct =
  sendMessage mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct -> value -> IO ()
setEndpoint mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct value =
  sendMessage mtrTimeSynchronizationClusterFabricScopedTrustedTimeSourceStruct setEndpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

