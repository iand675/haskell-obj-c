{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct
  ( MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct(..)
  , node
  , setNode
  , group
  , setGroup
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , clusterSelector
  , endpointSelector
  , groupSelector
  , nodeSelector
  , setClusterSelector
  , setEndpointSelector
  , setGroupSelector
  , setNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- node@
node :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
node mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct nodeSelector

-- | @- setNode:@
setNode :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setNode mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct setNodeSelector (toNSNumber value)

-- | @- group@
group :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
group mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct groupSelector

-- | @- setGroup:@
setGroup :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setGroup mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct setGroupSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
endpoint mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setEndpoint mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct setEndpointSelector (toNSNumber value)

-- | @- cluster@
cluster :: IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> IO (Id NSNumber)
cluster mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct clusterSelector

-- | @- setCluster:@
setCluster :: (IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct -> value -> IO ()
setCluster mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreBindingTargetStruct setClusterSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id NSNumber)
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector '[Id NSNumber] ()
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id NSNumber)
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector '[Id NSNumber] ()
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector '[Id NSNumber] ()
setClusterSelector = mkSelector "setCluster:"

