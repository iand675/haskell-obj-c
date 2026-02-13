{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams
  ( MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams(..)
  , initWithResponseValue_error
  , ccdid
  , setCcdid
  , ccdidSelector
  , initWithResponseValue_errorSelector
  , setCcdidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams setCcdidSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

