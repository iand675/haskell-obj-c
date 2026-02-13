{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlTargetStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlTargetStruct
  ( MTRAccessControlClusterAccessControlTargetStruct
  , IsMTRAccessControlClusterAccessControlTargetStruct(..)
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
cluster :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterAccessControlTargetStruct =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct clusterSelector

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setCluster mtrAccessControlClusterAccessControlTargetStruct value =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct setClusterSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterAccessControlTargetStruct =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterAccessControlTargetStruct value =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct setEndpointSelector (toNSNumber value)

-- | @- deviceType@
deviceType :: IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct => mtrAccessControlClusterAccessControlTargetStruct -> IO (Id NSNumber)
deviceType mtrAccessControlClusterAccessControlTargetStruct =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct deviceTypeSelector

-- | @- setDeviceType:@
setDeviceType :: (IsMTRAccessControlClusterAccessControlTargetStruct mtrAccessControlClusterAccessControlTargetStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlTargetStruct -> value -> IO ()
setDeviceType mtrAccessControlClusterAccessControlTargetStruct value =
  sendMessage mtrAccessControlClusterAccessControlTargetStruct setDeviceTypeSelector (toNSNumber value)

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

