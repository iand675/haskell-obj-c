{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct(..)
  , endpointID
  , setEndpointID
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , statusEntry
  , setStatusEntry
  , endpointIDSelector
  , friendlyNameSelector
  , nodeIDSelector
  , setEndpointIDSelector
  , setFriendlyNameSelector
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

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct setEndpointIDSelector (toNSNumber value)

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct setFriendlyNameSelector (toNSString value)

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct statusEntrySelector

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct setStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

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

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setStatusEntrySelector = mkSelector "setStatusEntry:"

