{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct(..)
  , nodeID
  , setNodeID
  , groupKeySetID
  , setGroupKeySetID
  , statusEntry
  , setStatusEntry
  , groupKeySetIDSelector
  , nodeIDSelector
  , setGroupKeySetIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct setGroupKeySetIDSelector (toNSNumber value)

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct statusEntrySelector

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct setStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector '[] (Id NSNumber)
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector '[Id NSNumber] ()
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setStatusEntrySelector = mkSelector "setStatusEntry:"

