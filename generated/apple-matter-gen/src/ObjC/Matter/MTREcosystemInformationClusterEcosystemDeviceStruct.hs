{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterEcosystemDeviceStruct@.
module ObjC.Matter.MTREcosystemInformationClusterEcosystemDeviceStruct
  ( MTREcosystemInformationClusterEcosystemDeviceStruct
  , IsMTREcosystemInformationClusterEcosystemDeviceStruct(..)
  , deviceName
  , setDeviceName
  , deviceNameLastEdit
  , setDeviceNameLastEdit
  , bridgedEndpoint
  , setBridgedEndpoint
  , originalEndpoint
  , setOriginalEndpoint
  , deviceTypes
  , setDeviceTypes
  , uniqueLocationIDs
  , setUniqueLocationIDs
  , uniqueLocationIDsLastEdit
  , setUniqueLocationIDsLastEdit
  , fabricIndex
  , setFabricIndex
  , bridgedEndpointSelector
  , deviceNameLastEditSelector
  , deviceNameSelector
  , deviceTypesSelector
  , fabricIndexSelector
  , originalEndpointSelector
  , setBridgedEndpointSelector
  , setDeviceNameLastEditSelector
  , setDeviceNameSelector
  , setDeviceTypesSelector
  , setFabricIndexSelector
  , setOriginalEndpointSelector
  , setUniqueLocationIDsLastEditSelector
  , setUniqueLocationIDsSelector
  , uniqueLocationIDsLastEditSelector
  , uniqueLocationIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deviceName@
deviceName :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSString)
deviceName mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct deviceNameSelector

-- | @- setDeviceName:@
setDeviceName :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSString value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceName mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setDeviceNameSelector (toNSString value)

-- | @- deviceNameLastEdit@
deviceNameLastEdit :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
deviceNameLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct deviceNameLastEditSelector

-- | @- setDeviceNameLastEdit:@
setDeviceNameLastEdit :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceNameLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setDeviceNameLastEditSelector (toNSNumber value)

-- | @- bridgedEndpoint@
bridgedEndpoint :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
bridgedEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct bridgedEndpointSelector

-- | @- setBridgedEndpoint:@
setBridgedEndpoint :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setBridgedEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setBridgedEndpointSelector (toNSNumber value)

-- | @- originalEndpoint@
originalEndpoint :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
originalEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct originalEndpointSelector

-- | @- setOriginalEndpoint:@
setOriginalEndpoint :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setOriginalEndpoint mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setOriginalEndpointSelector (toNSNumber value)

-- | @- deviceTypes@
deviceTypes :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSArray)
deviceTypes mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct deviceTypesSelector

-- | @- setDeviceTypes:@
setDeviceTypes :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSArray value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setDeviceTypes mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setDeviceTypesSelector (toNSArray value)

-- | @- uniqueLocationIDs@
uniqueLocationIDs :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSArray)
uniqueLocationIDs mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct uniqueLocationIDsSelector

-- | @- setUniqueLocationIDs:@
setUniqueLocationIDs :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSArray value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setUniqueLocationIDs mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setUniqueLocationIDsSelector (toNSArray value)

-- | @- uniqueLocationIDsLastEdit@
uniqueLocationIDsLastEdit :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
uniqueLocationIDsLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct uniqueLocationIDsLastEditSelector

-- | @- setUniqueLocationIDsLastEdit:@
setUniqueLocationIDsLastEdit :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setUniqueLocationIDsLastEdit mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setUniqueLocationIDsLastEditSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct => mtrEcosystemInformationClusterEcosystemDeviceStruct -> IO (Id NSNumber)
fabricIndex mtrEcosystemInformationClusterEcosystemDeviceStruct =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTREcosystemInformationClusterEcosystemDeviceStruct mtrEcosystemInformationClusterEcosystemDeviceStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemDeviceStruct -> value -> IO ()
setFabricIndex mtrEcosystemInformationClusterEcosystemDeviceStruct value =
  sendMessage mtrEcosystemInformationClusterEcosystemDeviceStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector '[] (Id NSString)
deviceNameSelector = mkSelector "deviceName"

-- | @Selector@ for @setDeviceName:@
setDeviceNameSelector :: Selector '[Id NSString] ()
setDeviceNameSelector = mkSelector "setDeviceName:"

-- | @Selector@ for @deviceNameLastEdit@
deviceNameLastEditSelector :: Selector '[] (Id NSNumber)
deviceNameLastEditSelector = mkSelector "deviceNameLastEdit"

-- | @Selector@ for @setDeviceNameLastEdit:@
setDeviceNameLastEditSelector :: Selector '[Id NSNumber] ()
setDeviceNameLastEditSelector = mkSelector "setDeviceNameLastEdit:"

-- | @Selector@ for @bridgedEndpoint@
bridgedEndpointSelector :: Selector '[] (Id NSNumber)
bridgedEndpointSelector = mkSelector "bridgedEndpoint"

-- | @Selector@ for @setBridgedEndpoint:@
setBridgedEndpointSelector :: Selector '[Id NSNumber] ()
setBridgedEndpointSelector = mkSelector "setBridgedEndpoint:"

-- | @Selector@ for @originalEndpoint@
originalEndpointSelector :: Selector '[] (Id NSNumber)
originalEndpointSelector = mkSelector "originalEndpoint"

-- | @Selector@ for @setOriginalEndpoint:@
setOriginalEndpointSelector :: Selector '[Id NSNumber] ()
setOriginalEndpointSelector = mkSelector "setOriginalEndpoint:"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector '[] (Id NSArray)
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @setDeviceTypes:@
setDeviceTypesSelector :: Selector '[Id NSArray] ()
setDeviceTypesSelector = mkSelector "setDeviceTypes:"

-- | @Selector@ for @uniqueLocationIDs@
uniqueLocationIDsSelector :: Selector '[] (Id NSArray)
uniqueLocationIDsSelector = mkSelector "uniqueLocationIDs"

-- | @Selector@ for @setUniqueLocationIDs:@
setUniqueLocationIDsSelector :: Selector '[Id NSArray] ()
setUniqueLocationIDsSelector = mkSelector "setUniqueLocationIDs:"

-- | @Selector@ for @uniqueLocationIDsLastEdit@
uniqueLocationIDsLastEditSelector :: Selector '[] (Id NSNumber)
uniqueLocationIDsLastEditSelector = mkSelector "uniqueLocationIDsLastEdit"

-- | @Selector@ for @setUniqueLocationIDsLastEdit:@
setUniqueLocationIDsLastEditSelector :: Selector '[Id NSNumber] ()
setUniqueLocationIDsLastEditSelector = mkSelector "setUniqueLocationIDsLastEdit:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

