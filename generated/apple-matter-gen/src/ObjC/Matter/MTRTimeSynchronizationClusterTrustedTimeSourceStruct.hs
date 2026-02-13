{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTrustedTimeSourceStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterTrustedTimeSourceStruct
  ( MTRTimeSynchronizationClusterTrustedTimeSourceStruct
  , IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct(..)
  , fabricIndex
  , setFabricIndex
  , nodeID
  , setNodeID
  , endpoint
  , setEndpoint
  , endpointSelector
  , fabricIndexSelector
  , nodeIDSelector
  , setEndpointSelector
  , setFabricIndexSelector
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

-- | @- fabricIndex@
fabricIndex :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
fabricIndex mtrTimeSynchronizationClusterTrustedTimeSourceStruct =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setFabricIndex mtrTimeSynchronizationClusterTrustedTimeSourceStruct value =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct setFabricIndexSelector (toNSNumber value)

-- | @- nodeID@
nodeID :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
nodeID mtrTimeSynchronizationClusterTrustedTimeSourceStruct =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setNodeID mtrTimeSynchronizationClusterTrustedTimeSourceStruct value =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct setNodeIDSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> IO (Id NSNumber)
endpoint mtrTimeSynchronizationClusterTrustedTimeSourceStruct =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTimeSynchronizationClusterTrustedTimeSourceStruct mtrTimeSynchronizationClusterTrustedTimeSourceStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTrustedTimeSourceStruct -> value -> IO ()
setEndpoint mtrTimeSynchronizationClusterTrustedTimeSourceStruct value =
  sendMessage mtrTimeSynchronizationClusterTrustedTimeSourceStruct setEndpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

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

