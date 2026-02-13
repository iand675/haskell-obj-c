{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , groupID
  , setGroupID
  , statusEntry
  , setStatusEntry
  , endpointIDSelector
  , groupIDSelector
  , nodeIDSelector
  , setEndpointIDSelector
  , setGroupIDSelector
  , setNodeIDSelector
  , setStatusEntrySelector
  , statusEntrySelector


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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct setEndpointIDSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct setGroupIDSelector (toNSNumber value)

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct statusEntrySelector

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct setStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setStatusEntrySelector = mkSelector "setStatusEntry:"

