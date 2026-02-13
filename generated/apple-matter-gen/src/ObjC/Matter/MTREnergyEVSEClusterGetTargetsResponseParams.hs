{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterGetTargetsResponseParams@.
module ObjC.Matter.MTREnergyEVSEClusterGetTargetsResponseParams
  ( MTREnergyEVSEClusterGetTargetsResponseParams
  , IsMTREnergyEVSEClusterGetTargetsResponseParams(..)
  , initWithResponseValue_error
  , chargingTargetSchedules
  , setChargingTargetSchedules
  , chargingTargetSchedulesSelector
  , initWithResponseValue_errorSelector
  , setChargingTargetSchedulesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTREnergyEVSEClusterGetTargetsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrEnergyEVSEClusterGetTargetsResponseParams -> responseValue -> error_ -> IO (Id MTREnergyEVSEClusterGetTargetsResponseParams)
initWithResponseValue_error mtrEnergyEVSEClusterGetTargetsResponseParams responseValue error_ =
  sendOwnedMessage mtrEnergyEVSEClusterGetTargetsResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- chargingTargetSchedules@
chargingTargetSchedules :: IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams => mtrEnergyEVSEClusterGetTargetsResponseParams -> IO (Id NSArray)
chargingTargetSchedules mtrEnergyEVSEClusterGetTargetsResponseParams =
  sendMessage mtrEnergyEVSEClusterGetTargetsResponseParams chargingTargetSchedulesSelector

-- | @- setChargingTargetSchedules:@
setChargingTargetSchedules :: (IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams, IsNSArray value) => mtrEnergyEVSEClusterGetTargetsResponseParams -> value -> IO ()
setChargingTargetSchedules mtrEnergyEVSEClusterGetTargetsResponseParams value =
  sendMessage mtrEnergyEVSEClusterGetTargetsResponseParams setChargingTargetSchedulesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTREnergyEVSEClusterGetTargetsResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @chargingTargetSchedules@
chargingTargetSchedulesSelector :: Selector '[] (Id NSArray)
chargingTargetSchedulesSelector = mkSelector "chargingTargetSchedules"

-- | @Selector@ for @setChargingTargetSchedules:@
setChargingTargetSchedulesSelector :: Selector '[Id NSArray] ()
setChargingTargetSchedulesSelector = mkSelector "setChargingTargetSchedules:"

