{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct(..)
  , groupID
  , setGroupID
  , friendlyName
  , setFriendlyName
  , groupKeySetID
  , setGroupKeySetID
  , groupCAT
  , setGroupCAT
  , groupCATVersion
  , setGroupCATVersion
  , groupPermission
  , setGroupPermission
  , friendlyNameSelector
  , groupCATSelector
  , groupCATVersionSelector
  , groupIDSelector
  , groupKeySetIDSelector
  , groupPermissionSelector
  , setFriendlyNameSelector
  , setGroupCATSelector
  , setGroupCATVersionSelector
  , setGroupIDSelector
  , setGroupKeySetIDSelector
  , setGroupPermissionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setGroupIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setFriendlyNameSelector (toNSString value)

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setGroupKeySetIDSelector (toNSNumber value)

-- | @- groupCAT@
groupCAT :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupCAT mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct groupCATSelector

-- | @- setGroupCAT:@
setGroupCAT :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupCAT mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setGroupCATSelector (toNSNumber value)

-- | @- groupCATVersion@
groupCATVersion :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupCATVersion mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct groupCATVersionSelector

-- | @- setGroupCATVersion:@
setGroupCATVersion :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupCATVersion mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setGroupCATVersionSelector (toNSNumber value)

-- | @- groupPermission@
groupPermission :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupPermission mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct groupPermissionSelector

-- | @- setGroupPermission:@
setGroupPermission :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupPermission mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct setGroupPermissionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector '[] (Id NSString)
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector '[Id NSString] ()
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector '[] (Id NSNumber)
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector '[Id NSNumber] ()
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupCAT@
groupCATSelector :: Selector '[] (Id NSNumber)
groupCATSelector = mkSelector "groupCAT"

-- | @Selector@ for @setGroupCAT:@
setGroupCATSelector :: Selector '[Id NSNumber] ()
setGroupCATSelector = mkSelector "setGroupCAT:"

-- | @Selector@ for @groupCATVersion@
groupCATVersionSelector :: Selector '[] (Id NSNumber)
groupCATVersionSelector = mkSelector "groupCATVersion"

-- | @Selector@ for @setGroupCATVersion:@
setGroupCATVersionSelector :: Selector '[Id NSNumber] ()
setGroupCATVersionSelector = mkSelector "setGroupCATVersion:"

-- | @Selector@ for @groupPermission@
groupPermissionSelector :: Selector '[] (Id NSNumber)
groupPermissionSelector = mkSelector "groupPermission"

-- | @Selector@ for @setGroupPermission:@
setGroupPermissionSelector :: Selector '[Id NSNumber] ()
setGroupPermissionSelector = mkSelector "setGroupPermission:"

