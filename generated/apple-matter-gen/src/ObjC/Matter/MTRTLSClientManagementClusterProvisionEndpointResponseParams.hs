{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterProvisionEndpointResponseParams@.
module ObjC.Matter.MTRTLSClientManagementClusterProvisionEndpointResponseParams
  ( MTRTLSClientManagementClusterProvisionEndpointResponseParams
  , IsMTRTLSClientManagementClusterProvisionEndpointResponseParams(..)
  , initWithResponseValue_error
  , endpointID
  , setEndpointID
  , endpointIDSelector
  , initWithResponseValue_errorSelector
  , setEndpointIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSClientManagementClusterProvisionEndpointResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> responseValue -> error_ -> IO (Id MTRTLSClientManagementClusterProvisionEndpointResponseParams)
initWithResponseValue_error mtrtlsClientManagementClusterProvisionEndpointResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsClientManagementClusterProvisionEndpointResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterProvisionEndpointResponseParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointResponseParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterProvisionEndpointResponseParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointResponseParams setEndpointIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSClientManagementClusterProvisionEndpointResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

