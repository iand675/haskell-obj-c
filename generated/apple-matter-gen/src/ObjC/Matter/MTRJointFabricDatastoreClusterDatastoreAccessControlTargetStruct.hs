{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct
  ( MTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct(..)
  , cluster
  , setCluster
  , endpoint
  , setEndpoint
  , deviceType
  , setDeviceType
  , clusterSelector
  , deviceTypeSelector
  , endpointSelector
  , setClusterSelector
  , setDeviceTypeSelector
  , setEndpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cluster@
cluster :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
cluster mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct clusterSelector

-- | @- setCluster:@
setCluster :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setCluster mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct setClusterSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
endpoint mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setEndpoint mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct setEndpointSelector (toNSNumber value)

-- | @- deviceType@
deviceType :: IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> IO (Id NSNumber)
deviceType mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct deviceTypeSelector

-- | @- setDeviceType:@
setDeviceType :: (IsMTRJointFabricDatastoreClusterDatastoreAccessControlTargetStruct mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct -> value -> IO ()
setDeviceType mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAccessControlTargetStruct setDeviceTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector '[Id NSNumber] ()
setClusterSelector = mkSelector "setCluster:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] (Id NSNumber)
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector '[Id NSNumber] ()
setDeviceTypeSelector = mkSelector "setDeviceType:"

