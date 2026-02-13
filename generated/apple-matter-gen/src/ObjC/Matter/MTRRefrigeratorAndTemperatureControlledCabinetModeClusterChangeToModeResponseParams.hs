{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams
  ( MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams
  , IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams, IsNSString value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeResponseParams)
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

