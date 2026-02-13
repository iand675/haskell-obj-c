{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams(..)
  , initWithResponseValue_error
  , caid
  , setCaid
  , caidSelector
  , initWithResponseValue_errorSelector
  , setCaidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams setCaidSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

