{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , vendorID
  , setVendorID
  , icac
  , setIcac
  , friendlyNameSelector
  , icacSelector
  , nodeIDSelector
  , setFriendlyNameSelector
  , setIcacSelector
  , setNodeIDSelector
  , setVendorIDSelector
  , vendorIDSelector


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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct setFriendlyNameSelector (toNSString value)

-- | @- vendorID@
vendorID :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSNumber)
vendorID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setVendorID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct setVendorIDSelector (toNSNumber value)

-- | @- icac@
icac :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSData)
icac mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct icacSelector

-- | @- setIcac:@
setIcac :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setIcac mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct setIcacSelector (toNSData value)

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

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @icac@
icacSelector :: Selector '[] (Id NSData)
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector '[Id NSData] ()
setIcacSelector = mkSelector "setIcac:"

