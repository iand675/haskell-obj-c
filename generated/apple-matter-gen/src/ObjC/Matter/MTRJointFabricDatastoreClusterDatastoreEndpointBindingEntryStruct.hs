{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , listID
  , setListID
  , binding
  , setBinding
  , statusEntry
  , setStatusEntry
  , bindingSelector
  , endpointIDSelector
  , listIDSelector
  , nodeIDSelector
  , setBindingSelector
  , setEndpointIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct setEndpointIDSelector (toNSNumber value)

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct listIDSelector

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setListID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct setListIDSelector (toNSNumber value)

-- | @- binding@
binding :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct)
binding mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct bindingSelector

-- | @- setBinding:@
setBinding :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setBinding mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct setBindingSelector (toMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct value)

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct statusEntrySelector

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct setStatusEntrySelector (toMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value)

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

-- | @Selector@ for @listID@
listIDSelector :: Selector '[] (Id NSNumber)
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector '[Id NSNumber] ()
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @binding@
bindingSelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct)
bindingSelector = mkSelector "binding"

-- | @Selector@ for @setBinding:@
setBindingSelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct] ()
setBindingSelector = mkSelector "setBinding:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct] ()
setStatusEntrySelector = mkSelector "setStatusEntry:"

