{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterTLSEndpointStruct@.
module ObjC.Matter.MTRTLSClientManagementClusterTLSEndpointStruct
  ( MTRTLSClientManagementClusterTLSEndpointStruct
  , IsMTRTLSClientManagementClusterTLSEndpointStruct(..)
  , endpointID
  , setEndpointID
  , hostname
  , setHostname
  , port
  , setPort
  , caid
  , setCaid
  , ccdid
  , setCcdid
  , referenceCount
  , setReferenceCount
  , fabricIndex
  , setFabricIndex
  , caidSelector
  , ccdidSelector
  , endpointIDSelector
  , fabricIndexSelector
  , hostnameSelector
  , portSelector
  , referenceCountSelector
  , setCaidSelector
  , setCcdidSelector
  , setEndpointIDSelector
  , setFabricIndexSelector
  , setHostnameSelector
  , setPortSelector
  , setReferenceCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setEndpointIDSelector (toNSNumber value)

-- | @- hostname@
hostname :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSData)
hostname mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct hostnameSelector

-- | @- setHostname:@
setHostname :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSData value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setHostname mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setHostnameSelector (toNSData value)

-- | @- port@
port :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
port mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct portSelector

-- | @- setPort:@
setPort :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setPort mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setPortSelector (toNSNumber value)

-- | @- caid@
caid :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
caid mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setCaid mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setCaidSelector (toNSNumber value)

-- | @- ccdid@
ccdid :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
ccdid mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setCcdid mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setCcdidSelector (toNSNumber value)

-- | @- referenceCount@
referenceCount :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
referenceCount mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct referenceCountSelector

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setReferenceCount mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setReferenceCountSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct => mtrtlsClientManagementClusterTLSEndpointStruct -> IO (Id NSNumber)
fabricIndex mtrtlsClientManagementClusterTLSEndpointStruct =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTLSClientManagementClusterTLSEndpointStruct mtrtlsClientManagementClusterTLSEndpointStruct, IsNSNumber value) => mtrtlsClientManagementClusterTLSEndpointStruct -> value -> IO ()
setFabricIndex mtrtlsClientManagementClusterTLSEndpointStruct value =
  sendMessage mtrtlsClientManagementClusterTLSEndpointStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector '[] (Id NSData)
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @setHostname:@
setHostnameSelector :: Selector '[Id NSData] ()
setHostnameSelector = mkSelector "setHostname:"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSNumber)
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector '[Id NSNumber] ()
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector '[] (Id NSNumber)
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector '[Id NSNumber] ()
setReferenceCountSelector = mkSelector "setReferenceCount:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

