{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams
  ( MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams
  , IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , statusText
  , setStatusText
  , initWithResponseValue_errorSelector
  , setStatusSelector
  , setStatusTextSelector
  , statusSelector
  , statusTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams value =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeResponseParams mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams, IsNSString value) => mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams value =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDeviceEnergyManagementModeClusterChangeToModeResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @statusText@
statusTextSelector :: Selector '[] (Id NSString)
statusTextSelector = mkSelector "statusText"

-- | @Selector@ for @setStatusText:@
setStatusTextSelector :: Selector '[Id NSString] ()
setStatusTextSelector = mkSelector "setStatusText:"

