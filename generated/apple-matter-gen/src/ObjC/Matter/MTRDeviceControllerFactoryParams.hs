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
  , initSelector
  , initWithStorageSelector
  , storageSelector
  , otaProviderDelegateSelector
  , setOtaProviderDelegateSelector
  , productAttestationAuthorityCertificatesSelector
  , setProductAttestationAuthorityCertificatesSelector
  , certificationDeclarationCertificatesSelector
  , setCertificationDeclarationCertificatesSelector
  , portSelector
  , setPortSelector
  , shouldStartServerSelector
  , setShouldStartServerSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id MTRDeviceControllerFactoryParams)
init_ mtrDeviceControllerFactoryParams  =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithStorage:@
initWithStorage :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> RawId -> IO (Id MTRDeviceControllerFactoryParams)
initWithStorage mtrDeviceControllerFactoryParams  storage =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "initWithStorage:") (retPtr retVoid) [argPtr (castPtr (unRawId storage) :: Ptr ())] >>= ownedObject . castPtr

-- | @- storage@
storage :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO RawId
storage mtrDeviceControllerFactoryParams  =
    fmap (RawId . castPtr) $ sendMsg mtrDeviceControllerFactoryParams (mkSelector "storage") (retPtr retVoid) []

-- | @- otaProviderDelegate@
otaProviderDelegate :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO RawId
otaProviderDelegate mtrDeviceControllerFactoryParams  =
    fmap (RawId . castPtr) $ sendMsg mtrDeviceControllerFactoryParams (mkSelector "otaProviderDelegate") (retPtr retVoid) []

-- | @- setOtaProviderDelegate:@
setOtaProviderDelegate :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> RawId -> IO ()
setOtaProviderDelegate mtrDeviceControllerFactoryParams  value =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "setOtaProviderDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- productAttestationAuthorityCertificates@
productAttestationAuthorityCertificates :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSArray)
productAttestationAuthorityCertificates mtrDeviceControllerFactoryParams  =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "productAttestationAuthorityCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificates :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSArray value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setProductAttestationAuthorityCertificates mtrDeviceControllerFactoryParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerFactoryParams (mkSelector "setProductAttestationAuthorityCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- certificationDeclarationCertificates@
certificationDeclarationCertificates :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSArray)
certificationDeclarationCertificates mtrDeviceControllerFactoryParams  =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "certificationDeclarationCertificates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificates :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSArray value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setCertificationDeclarationCertificates mtrDeviceControllerFactoryParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerFactoryParams (mkSelector "setCertificationDeclarationCertificates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- port@
port :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO (Id NSNumber)
port mtrDeviceControllerFactoryParams  =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPort:@
setPort :: (IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams, IsNSNumber value) => mtrDeviceControllerFactoryParams -> value -> IO ()
setPort mtrDeviceControllerFactoryParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerFactoryParams (mkSelector "setPort:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldStartServer@
shouldStartServer :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> IO Bool
shouldStartServer mtrDeviceControllerFactoryParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceControllerFactoryParams (mkSelector "shouldStartServer") retCULong []

-- | @- setShouldStartServer:@
setShouldStartServer :: IsMTRDeviceControllerFactoryParams mtrDeviceControllerFactoryParams => mtrDeviceControllerFactoryParams -> Bool -> IO ()
setShouldStartServer mtrDeviceControllerFactoryParams  value =
    sendMsg mtrDeviceControllerFactoryParams (mkSelector "setShouldStartServer:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStorage:@
initWithStorageSelector :: Selector
initWithStorageSelector = mkSelector "initWithStorage:"

-- | @Selector@ for @storage@
storageSelector :: Selector
storageSelector = mkSelector "storage"

-- | @Selector@ for @otaProviderDelegate@
otaProviderDelegateSelector :: Selector
otaProviderDelegateSelector = mkSelector "otaProviderDelegate"

-- | @Selector@ for @setOtaProviderDelegate:@
setOtaProviderDelegateSelector :: Selector
setOtaProviderDelegateSelector = mkSelector "setOtaProviderDelegate:"

-- | @Selector@ for @productAttestationAuthorityCertificates@
productAttestationAuthorityCertificatesSelector :: Selector
productAttestationAuthorityCertificatesSelector = mkSelector "productAttestationAuthorityCertificates"

-- | @Selector@ for @setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificatesSelector :: Selector
setProductAttestationAuthorityCertificatesSelector = mkSelector "setProductAttestationAuthorityCertificates:"

-- | @Selector@ for @certificationDeclarationCertificates@
certificationDeclarationCertificatesSelector :: Selector
certificationDeclarationCertificatesSelector = mkSelector "certificationDeclarationCertificates"

-- | @Selector@ for @setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificatesSelector :: Selector
setCertificationDeclarationCertificatesSelector = mkSelector "setCertificationDeclarationCertificates:"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @shouldStartServer@
shouldStartServerSelector :: Selector
shouldStartServerSelector = mkSelector "shouldStartServer"

-- | @Selector@ for @setShouldStartServer:@
setShouldStartServerSelector :: Selector
setShouldStartServerSelector = mkSelector "setShouldStartServer:"

