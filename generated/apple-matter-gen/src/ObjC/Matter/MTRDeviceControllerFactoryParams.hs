{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerFactoryParams@.
module ObjC.Matter.MTRDeviceControllerFactoryParams
  ( MTRDeviceControllerFactoryParams
  , IsMTRDeviceControllerFactoryParams(..)
  , init_
  , initWithStorage
  , storage
  , otaProviderDelegate
  , setOtaProviderDelegate
  , productAttestationAuthorityCertificates
  , setProductAttestationAuthorityCertificates
  , certificationDeclarationCertificates
  , setCertificationDeclarationCertificates
  , port
  , setPort
  , shouldStartServer
  , setShouldStartServer
  , certificationDeclarationCertificatesSelector
  , initSelector
  , initWithStorageSelector
  , otaProviderDelegateSelector
  , portSelector
  , productAttestationAuthorityCertificatesSelector
  , setCertificationDeclarationCertificatesSelector
  , setOtaProviderDelegateSelector
  , setPortSelector
  , setProductAttestationAuthorityCertificatesSelector
  , setShouldStartServerSelector
  , shouldStartServerSelector
  , storageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id MTRDeviceControllerFactoryParams)
init_ mtrDeviceControllerFactoryParams =
  sendOwnedMessage mtrDeviceControllerFactoryParams initSelector

-- | @- initWithStorage:@
initWithStorage :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> RawId -> IO (Id MTRDeviceControllerFactoryParams)
initWithStorage mtrDeviceControllerFactoryParams storage =
  sendOwnedMessage mtrDeviceControllerFactoryParams initWithStorageSelector storage

-- | @- storage@
storage :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO RawId
storage mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams storageSelector

-- | @- otaProviderDelegate@
otaProviderDelegate :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO RawId
otaProviderDelegate mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams otaProviderDelegateSelector

-- | @- setOtaProviderDelegate:@
setOtaProviderDelegate :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> RawId -> IO ()
setOtaProviderDelegate mtrDeviceControllerFactoryParams value =
  sendMessage mtrDeviceControllerFactoryParams setOtaProviderDelegateSelector value

-- | @- productAttestationAuthorityCertificates@
productAttestationAuthorityCertificates :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSArray)
productAttestationAuthorityCertificates mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams productAttestationAuthorityCertificatesSelector

-- | @- setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificates :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSArray value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setProductAttestationAuthorityCertificates mtrDeviceControllerFactoryParams value =
  sendMessage mtrDeviceControllerFactoryParams setProductAttestationAuthorityCertificatesSelector (toNSArray value)

-- | @- certificationDeclarationCertificates@
certificationDeclarationCertificates :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSArray)
certificationDeclarationCertificates mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams certificationDeclarationCertificatesSelector

-- | @- setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificates :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSArray value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setCertificationDeclarationCertificates mtrDeviceControllerFactoryParams value =
  sendMessage mtrDeviceControllerFactoryParams setCertificationDeclarationCertificatesSelector (toNSArray value)

-- | @- port@
port :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSNumber)
port mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams portSelector

-- | @- setPort:@
setPort :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSNumber value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setPort mtrDeviceControllerFactoryParams value =
  sendMessage mtrDeviceControllerFactoryParams setPortSelector (toNSNumber value)

-- | @- shouldStartServer@
shouldStartServer :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO Bool
shouldStartServer mtrDeviceControllerFactoryParams =
  sendMessage mtrDeviceControllerFactoryParams shouldStartServerSelector

-- | @- setShouldStartServer:@
setShouldStartServer :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> Bool -> IO ()
setShouldStartServer mtrDeviceControllerFactoryParams value =
  sendMessage mtrDeviceControllerFactoryParams setShouldStartServerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceControllerFactoryParams)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStorage:@
initWithStorageSelector :: Selector '[RawId] (Id MTRDeviceControllerFactoryParams)
initWithStorageSelector = mkSelector "initWithStorage:"

-- | @Selector@ for @storage@
storageSelector :: Selector '[] RawId
storageSelector = mkSelector "storage"

-- | @Selector@ for @otaProviderDelegate@
otaProviderDelegateSelector :: Selector '[] RawId
otaProviderDelegateSelector = mkSelector "otaProviderDelegate"

-- | @Selector@ for @setOtaProviderDelegate:@
setOtaProviderDelegateSelector :: Selector '[RawId] ()
setOtaProviderDelegateSelector = mkSelector "setOtaProviderDelegate:"

-- | @Selector@ for @productAttestationAuthorityCertificates@
productAttestationAuthorityCertificatesSelector :: Selector '[] (Id NSArray)
productAttestationAuthorityCertificatesSelector = mkSelector "productAttestationAuthorityCertificates"

-- | @Selector@ for @setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificatesSelector :: Selector '[Id NSArray] ()
setProductAttestationAuthorityCertificatesSelector = mkSelector "setProductAttestationAuthorityCertificates:"

-- | @Selector@ for @certificationDeclarationCertificates@
certificationDeclarationCertificatesSelector :: Selector '[] (Id NSArray)
certificationDeclarationCertificatesSelector = mkSelector "certificationDeclarationCertificates"

-- | @Selector@ for @setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificatesSelector :: Selector '[Id NSArray] ()
setCertificationDeclarationCertificatesSelector = mkSelector "setCertificationDeclarationCertificates:"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSNumber)
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector '[Id NSNumber] ()
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @shouldStartServer@
shouldStartServerSelector :: Selector '[] Bool
shouldStartServerSelector = mkSelector "shouldStartServer"

-- | @Selector@ for @setShouldStartServer:@
setShouldStartServerSelector :: Selector '[Bool] ()
setShouldStartServerSelector = mkSelector "setShouldStartServer:"

