{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , commissioningStatusEntry
  , setCommissioningStatusEntry
  , commissioningStatusEntrySelector
  , friendlyNameSelector
  , nodeIDSelector
  , setCommissioningStatusEntrySelector
  , setFriendlyNameSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct setFriendlyNameSelector (toNSString value)

-- | @- commissioningStatusEntry@
commissioningStatusEntry :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
commissioningStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct commissioningStatusEntrySelector

-- | @- setCommissioningStatusEntry:@
setCommissioningStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setCommissioningStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct setCommissioningStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector '[] (Id NSString)
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector '[Id NSString] ()
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @commissioningStatusEntry@
commissioningStatusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
commissioningStatusEntrySelector = mkSelector "commissioningStatusEntry"

-- | @Selector@ for @setCommissioningStatusEntry:@
setCommissioningStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setCommissioningStatusEntrySelector = mkSelector "setCommissioningStatusEntry:"

