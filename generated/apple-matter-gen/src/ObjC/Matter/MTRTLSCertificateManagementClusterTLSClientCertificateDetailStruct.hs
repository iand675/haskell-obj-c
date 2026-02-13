{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct@.
module ObjC.Matter.MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct
  ( MTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct
  , IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct(..)
  , ccdid
  , setCcdid
  , clientCertificate
  , setClientCertificate
  , intermediateCertificates
  , setIntermediateCertificates
  , fabricIndex
  , setFabricIndex
  , ccdidSelector
  , clientCertificateSelector
  , fabricIndexSelector
  , intermediateCertificatesSelector
  , setCcdidSelector
  , setClientCertificateSelector
  , setFabricIndexSelector
  , setIntermediateCertificatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct setCcdidSelector (toNSNumber value)

-- | @- clientCertificate@
clientCertificate :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSData)
clientCertificate mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct clientCertificateSelector

-- | @- setClientCertificate:@
setClientCertificate :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSData value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setClientCertificate mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct setClientCertificateSelector (toNSData value)

-- | @- intermediateCertificates@
intermediateCertificates :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSArray)
intermediateCertificates mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct intermediateCertificatesSelector

-- | @- setIntermediateCertificates:@
setIntermediateCertificates :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSArray value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setIntermediateCertificates mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct setIntermediateCertificatesSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> IO (Id NSNumber)
fabricIndex mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSCertificateManagementClusterTLSClientCertificateDetailStruct mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct -> value -> IO ()
setFabricIndex mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSClientCertificateDetailStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @clientCertificate@
clientCertificateSelector :: Selector '[] (Id NSData)
clientCertificateSelector = mkSelector "clientCertificate"

-- | @Selector@ for @setClientCertificate:@
setClientCertificateSelector :: Selector '[Id NSData] ()
setClientCertificateSelector = mkSelector "setClientCertificate:"

-- | @Selector@ for @intermediateCertificates@
intermediateCertificatesSelector :: Selector '[] (Id NSArray)
intermediateCertificatesSelector = mkSelector "intermediateCertificates"

-- | @Selector@ for @setIntermediateCertificates:@
setIntermediateCertificatesSelector :: Selector '[Id NSArray] ()
setIntermediateCertificatesSelector = mkSelector "setIntermediateCertificates:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

