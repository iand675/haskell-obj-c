{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreACLEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreACLEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreACLEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct(..)
  , nodeID
  , setNodeID
  , listID
  , setListID
  , aclEntry
  , setAclEntry
  , statusEntry
  , setStatusEntry
  , aclEntrySelector
  , listIDSelector
  , nodeIDSelector
  , setAclEntrySelector
  , setListIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct listIDSelector

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setListID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct setListIDSelector (toNSNumber value)

-- | @- aclEntry@
aclEntry :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct)
aclEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct aclEntrySelector

-- | @- setAclEntry:@
setAclEntry :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setAclEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct setAclEntrySelector (toMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value)

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct statusEntrySelector

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreACLEntryStruct setStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @listID@
listIDSelector :: Selector '[] (Id NSNumber)
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector '[Id NSNumber] ()
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @aclEntry@
aclEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct)
aclEntrySelector = mkSelector "aclEntry"

-- | @Selector@ for @setAclEntry:@
setAclEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct] ()
setAclEntrySelector = mkSelector "setAclEntry:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setStatusEntrySelector = mkSelector "setStatusEntry:"

