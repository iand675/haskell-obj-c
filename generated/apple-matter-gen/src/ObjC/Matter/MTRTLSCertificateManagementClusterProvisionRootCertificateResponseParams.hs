{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams(..)
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

-- | Initialize an MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams setCaidSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

