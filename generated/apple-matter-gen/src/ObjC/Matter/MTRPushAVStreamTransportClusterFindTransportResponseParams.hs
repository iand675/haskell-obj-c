{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterFindTransportResponseParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterFindTransportResponseParams
  ( MTRPushAVStreamTransportClusterFindTransportResponseParams
  , IsMTRPushAVStreamTransportClusterFindTransportResponseParams(..)
  , initWithResponseValue_error
  , transportConfigurations
  , setTransportConfigurations
  , initWithResponseValue_errorSelector
  , setTransportConfigurationsSelector
  , transportConfigurationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRPushAVStreamTransportClusterFindTransportResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrPushAVStreamTransportClusterFindTransportResponseParams -> responseValue -> error_ -> IO (Id MTRPushAVStreamTransportClusterFindTransportResponseParams)
initWithResponseValue_error mtrPushAVStreamTransportClusterFindTransportResponseParams responseValue error_ =
  sendOwnedMessage mtrPushAVStreamTransportClusterFindTransportResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- transportConfigurations@
transportConfigurations :: IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams => mtrPushAVStreamTransportClusterFindTransportResponseParams -> IO (Id NSArray)
transportConfigurations mtrPushAVStreamTransportClusterFindTransportResponseParams =
  sendMessage mtrPushAVStreamTransportClusterFindTransportResponseParams transportConfigurationsSelector

-- | @- setTransportConfigurations:@
setTransportConfigurations :: (IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams, IsNSArray value) => mtrPushAVStreamTransportClusterFindTransportResponseParams -> value -> IO ()
setTransportConfigurations mtrPushAVStreamTransportClusterFindTransportResponseParams value =
  sendMessage mtrPushAVStreamTransportClusterFindTransportResponseParams setTransportConfigurationsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRPushAVStreamTransportClusterFindTransportResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @transportConfigurations@
transportConfigurationsSelector :: Selector '[] (Id NSArray)
transportConfigurationsSelector = mkSelector "transportConfigurations"

-- | @Selector@ for @setTransportConfigurations:@
setTransportConfigurationsSelector :: Selector '[Id NSArray] ()
setTransportConfigurationsSelector = mkSelector "setTransportConfigurations:"

