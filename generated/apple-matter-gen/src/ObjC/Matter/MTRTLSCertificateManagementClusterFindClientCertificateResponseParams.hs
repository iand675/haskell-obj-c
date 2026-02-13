{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindClientCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindClientCertificateResponseParams
  ( MTRTLSCertificateManagementClusterFindClientCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams(..)
  , initWithResponseValue_error
  , certificateDetails
  , setCertificateDetails
  , certificateDetailsSelector
  , initWithResponseValue_errorSelector
  , setCertificateDetailsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTLSCertificateManagementClusterFindClientCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterFindClientCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterFindClientCertificateResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterFindClientCertificateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- certificateDetails@
certificateDetails :: IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> IO (Id NSArray)
certificateDetails mtrtlsCertificateManagementClusterFindClientCertificateResponseParams =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateResponseParams certificateDetailsSelector

-- | @- setCertificateDetails:@
setCertificateDetails :: (IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams, IsNSArray value) => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> value -> IO ()
setCertificateDetails mtrtlsCertificateManagementClusterFindClientCertificateResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterFindClientCertificateResponseParams setCertificateDetailsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterFindClientCertificateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @certificateDetails@
certificateDetailsSelector :: Selector '[] (Id NSArray)
certificateDetailsSelector = mkSelector "certificateDetails"

-- | @Selector@ for @setCertificateDetails:@
setCertificateDetailsSelector :: Selector '[Id NSArray] ()
setCertificateDetailsSelector = mkSelector "setCertificateDetails:"

