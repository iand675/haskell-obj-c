{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterFindRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams(..)
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

-- | Initialize an MTRTLSCertificateManagementClusterFindRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterFindRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterFindRootCertificateResponseParams responseValue error_ =
  sendOwnedMessage mtrtlsCertificateManagementClusterFindRootCertificateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- certificateDetails@
certificateDetails :: IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> IO (Id NSArray)
certificateDetails mtrtlsCertificateManagementClusterFindRootCertificateResponseParams =
  sendMessage mtrtlsCertificateManagementClusterFindRootCertificateResponseParams certificateDetailsSelector

-- | @- setCertificateDetails:@
setCertificateDetails :: (IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams, IsNSArray value) => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> value -> IO ()
setCertificateDetails mtrtlsCertificateManagementClusterFindRootCertificateResponseParams value =
  sendMessage mtrtlsCertificateManagementClusterFindRootCertificateResponseParams setCertificateDetailsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTLSCertificateManagementClusterFindRootCertificateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @certificateDetails@
certificateDetailsSelector :: Selector '[] (Id NSArray)
certificateDetailsSelector = mkSelector "certificateDetails"

-- | @Selector@ for @setCertificateDetails:@
setCertificateDetailsSelector :: Selector '[Id NSArray] ()
setCertificateDetailsSelector = mkSelector "setCertificateDetails:"

