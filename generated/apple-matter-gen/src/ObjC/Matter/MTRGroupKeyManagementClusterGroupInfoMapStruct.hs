{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterGroupInfoMapStruct@.
module ObjC.Matter.MTRGroupKeyManagementClusterGroupInfoMapStruct
  ( MTRGroupKeyManagementClusterGroupInfoMapStruct
  , IsMTRGroupKeyManagementClusterGroupInfoMapStruct(..)
  , groupId
  , setGroupId
  , endpoints
  , setEndpoints
  , groupName
  , setGroupName
  , fabricIndex
  , setFabricIndex
  , endpointsSelector
  , fabricIndexSelector
  , groupIdSelector
  , groupNameSelector
  , setEndpointsSelector
  , setFabricIndexSelector
  , setGroupIdSelector
  , setGroupNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- groupId@
groupId :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSNumber)
groupId mtrGroupKeyManagementClusterGroupInfoMapStruct =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct groupIdSelector

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setGroupId mtrGroupKeyManagementClusterGroupInfoMapStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct setGroupIdSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSArray)
endpoints mtrGroupKeyManagementClusterGroupInfoMapStruct =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSArray value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setEndpoints mtrGroupKeyManagementClusterGroupInfoMapStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct setEndpointsSelector (toNSArray value)

-- | @- groupName@
groupName :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSString)
groupName mtrGroupKeyManagementClusterGroupInfoMapStruct =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct groupNameSelector

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSString value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setGroupName mtrGroupKeyManagementClusterGroupInfoMapStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct setGroupNameSelector (toNSString value)

-- | @- fabricIndex@
fabricIndex :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSNumber)
fabricIndex mtrGroupKeyManagementClusterGroupInfoMapStruct =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setFabricIndex mtrGroupKeyManagementClusterGroupInfoMapStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupInfoMapStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupId@
groupIdSelector :: Selector '[] (Id NSNumber)
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector '[Id NSNumber] ()
setGroupIdSelector = mkSelector "setGroupId:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id NSString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector '[Id NSString] ()
setGroupNameSelector = mkSelector "setGroupName:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

