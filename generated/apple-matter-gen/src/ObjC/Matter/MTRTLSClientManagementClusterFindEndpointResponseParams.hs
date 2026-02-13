{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterFindEndpointResponseParams@.
module ObjC.Matter.MTRTLSClientManagementClusterFindEndpointResponseParams
  ( MTRTLSClientManagementClusterFindEndpointResponseParams
  , IsMTRTLSClientManagementClusterFindEndpointResponseParams(..)
  , initWithResponseValue_error
  , endpoint
  , setEndpoint
  , endpointSelector
  , initWithResponseValue_errorSelector
  , setEndpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSClientManagementClusterFindEndpointResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsClientManagementClusterFindEndpointResponseParams -> responseValue -> error_ -> IO (Id MTRTLSClientManagementClusterFindEndpointResponseParams)
initWithResponseValue_error mtrtlsClientManagementClusterFindEndpointResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsClientManagementClusterFindEndpointResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- endpoint@
endpoint :: IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams => mtrtlsClientManagementClusterFindEndpointResponseParams -> IO (Id MTRTLSClientManagementClusterTLSEndpointStruct)
endpoint mtrtlsClientManagementClusterFindEndpointResponseParams =
  sendMessage mtrtlsClientManagementClusterFindEndpointResponseParams endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams, IsMTRTLSClientManagementClusterTLSEndpointStruct value) => mtrtlsClientManagementClusterFindEndpointResponseParams -> value -> IO ()
setEndpoint mtrtlsClientManagementClusterFindEndpointResponseParams value =
  sendMessage mtrtlsClientManagementClusterFindEndpointResponseParams setEndpointSelector (toMTRTLSClientManagementClusterTLSEndpointStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSClientManagementClusterFindEndpointResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id MTRTLSClientManagementClusterTLSEndpointStruct)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id MTRTLSClientManagementClusterTLSEndpointStruct] ()
setEndpointSelector = mkSelector "setEndpoint:"

