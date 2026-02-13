{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams
  ( MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams
  , IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams(..)
  , initWithResponseValue_error
  , transportConfiguration
  , setTransportConfiguration
  , initWithResponseValue_errorSelector
  , setTransportConfigurationSelector
  , transportConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> responseValue -> error_ -> IO (Id MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams)
initWithResponseValue_error mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams responseValue error_ =
  sendOwnedMessage mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- transportConfiguration@
transportConfiguration :: IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> IO (Id MTRPushAVStreamTransportClusterTransportConfigurationStruct)
transportConfiguration mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams transportConfigurationSelector

-- | @- setTransportConfiguration:@
setTransportConfiguration :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams, IsMTRPushAVStreamTransportClusterTransportConfigurationStruct value) => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> value -> IO ()
setTransportConfiguration mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams value =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams setTransportConfigurationSelector (toMTRPushAVStreamTransportClusterTransportConfigurationStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @transportConfiguration@
transportConfigurationSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportConfigurationStruct)
transportConfigurationSelector = mkSelector "transportConfiguration"

-- | @Selector@ for @setTransportConfiguration:@
setTransportConfigurationSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportConfigurationStruct] ()
setTransportConfigurationSelector = mkSelector "setTransportConfiguration:"

