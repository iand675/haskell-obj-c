{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterTLSCertStruct@.
module ObjC.Matter.MTRTLSCertificateManagementClusterTLSCertStruct
  ( MTRTLSCertificateManagementClusterTLSCertStruct
  , IsMTRTLSCertificateManagementClusterTLSCertStruct(..)
  , caid
  , setCaid
  , certificate
  , setCertificate
  , fabricIndex
  , setFabricIndex
  , caidSelector
  , certificateSelector
  , fabricIndexSelector
  , setCaidSelector
  , setCertificateSelector
  , setFabricIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterTLSCertStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterTLSCertStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct setCaidSelector (toNSNumber value)

-- | @- certificate@
certificate :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSData)
certificate mtrtlsCertificateManagementClusterTLSCertStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct certificateSelector

-- | @- setCertificate:@
setCertificate :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSData value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setCertificate mtrtlsCertificateManagementClusterTLSCertStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct setCertificateSelector (toNSData value)

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct => mtrtlsCertificateManagementClusterTLSCertStruct -> IO (Id NSNumber)
fabricIndex mtrtlsCertificateManagementClusterTLSCertStruct =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSCertificateManagementClusterTLSCertStruct mtrtlsCertificateManagementClusterTLSCertStruct, IsNSNumber value) => mtrtlsCertificateManagementClusterTLSCertStruct -> value -> IO ()
setFabricIndex mtrtlsCertificateManagementClusterTLSCertStruct value =
  sendMessage mtrtlsCertificateManagementClusterTLSCertStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector '[] (Id NSData)
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector '[Id NSData] ()
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

