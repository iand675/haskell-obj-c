{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTREnergyEVSEModeClusterChangeToModeResponseParams
  ( MTREnergyEVSEModeClusterChangeToModeResponseParams
  , IsMTREnergyEVSEModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTREnergyEVSEModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTREnergyEVSEModeClusterChangeToModeResponseParams mtrEnergyEVSEModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrEnergyEVSEModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTREnergyEVSEModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrEnergyEVSEModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrEnergyEVSEModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTREnergyEVSEModeClusterChangeToModeResponseParams mtrEnergyEVSEModeClusterChangeToModeResponseParams => mtrEnergyEVSEModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrEnergyEVSEModeClusterChangeToModeResponseParams =
  sendMessage mtrEnergyEVSEModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTREnergyEVSEModeClusterChangeToModeResponseParams mtrEnergyEVSEModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrEnergyEVSEModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrEnergyEVSEModeClusterChangeToModeResponseParams value =
  sendMessage mtrEnergyEVSEModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTREnergyEVSEModeClusterChangeToModeResponseParams mtrEnergyEVSEModeClusterChangeToModeResponseParams => mtrEnergyEVSEModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrEnergyEVSEModeClusterChangeToModeResponseParams =
  sendMessage mtrEnergyEVSEModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTREnergyEVSEModeClusterChangeToModeResponseParams mtrEnergyEVSEModeClusterChangeToModeResponseParams, IsNSString value) => mtrEnergyEVSEModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrEnergyEVSEModeClusterChangeToModeResponseParams value =
  sendMessage mtrEnergyEVSEModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTREnergyEVSEModeClusterChangeToModeResponseParams)
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

