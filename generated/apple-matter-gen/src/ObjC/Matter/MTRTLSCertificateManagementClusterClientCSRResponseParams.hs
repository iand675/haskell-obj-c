{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterClientCSRResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterClientCSRResponseParams
  ( MTRTLSCertificateManagementClusterClientCSRResponseParams
  , IsMTRTLSCertificateManagementClusterClientCSRResponseParams(..)
  , initWithResponseValue_error
  , ccdid
  , setCcdid
  , csr
  , setCsr
  , nonceSignature
  , setNonceSignature
  , ccdidSelector
  , csrSelector
  , initWithResponseValue_errorSelector
  , nonceSignatureSelector
  , setCcdidSelector
  , setCsrSelector
  , setNonceSignatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSCertificateManagementClusterClientCSRResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterClientCSRResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterClientCSRResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterClientCSRResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterClientCSRResponseParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterClientCSRResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams setCcdidSelector (toNSNumber value)

-- | @- csr@
csr :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSData)
csr mtrtlsCertificateManagementClusterClientCSRResponseParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams csrSelector

-- | @- setCsr:@
setCsr :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSData value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setCsr mtrtlsCertificateManagementClusterClientCSRResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams setCsrSelector (toNSData value)

-- | @- nonceSignature@
nonceSignature :: IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams => mtrtlsCertificateManagementClusterClientCSRResponseParams -> IO (Id NSData)
nonceSignature mtrtlsCertificateManagementClusterClientCSRResponseParams =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams nonceSignatureSelector

-- | @- setNonceSignature:@
setNonceSignature :: (IsMTRTLSCertificateManagementClusterClientCSRResponseParams mtrtlsCertificateManagementClusterClientCSRResponseParams, IsNSData value) => mtrtlsCertificateManagementClusterClientCSRResponseParams -> value -> IO ()
setNonceSignature mtrtlsCertificateManagementClusterClientCSRResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterClientCSRResponseParams setNonceSignatureSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterClientCSRResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @csr@
csrSelector :: Selector '[] (Id NSData)
csrSelector = mkSelector "csr"

-- | @Selector@ for @setCsr:@
setCsrSelector :: Selector '[Id NSData] ()
setCsrSelector = mkSelector "setCsr:"

-- | @Selector@ for @nonceSignature@
nonceSignatureSelector :: Selector '[] (Id NSData)
nonceSignatureSelector = mkSelector "nonceSignature"

-- | @Selector@ for @setNonceSignature:@
setNonceSignatureSelector :: Selector '[Id NSData] ()
setNonceSignatureSelector = mkSelector "setNonceSignature:"

