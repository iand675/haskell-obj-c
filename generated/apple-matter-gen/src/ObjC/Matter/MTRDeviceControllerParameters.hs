{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Parameters that can be used to initialize an MTRDeviceController which has a node identity.
--
-- Generated bindings for @MTRDeviceControllerParameters@.
module ObjC.Matter.MTRDeviceControllerParameters
  ( MTRDeviceControllerParameters
  , IsMTRDeviceControllerParameters(..)
  , setOperationalCertificateIssuer_queue
  , setOTAProviderDelegate_queue
  , productAttestationAuthorityCertificates
  , setProductAttestationAuthorityCertificates
  , certificationDeclarationCertificates
  , setCertificationDeclarationCertificates
  , shouldAdvertiseOperational
  , setShouldAdvertiseOperational
  , concurrentSubscriptionEstablishmentsAllowedOnThread
  , setConcurrentSubscriptionEstablishmentsAllowedOnThread
  , storageBehaviorConfiguration
  , setStorageBehaviorConfiguration
  , certificationDeclarationCertificatesSelector
  , concurrentSubscriptionEstablishmentsAllowedOnThreadSelector
  , productAttestationAuthorityCertificatesSelector
  , setCertificationDeclarationCertificatesSelector
  , setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector
  , setOTAProviderDelegate_queueSelector
  , setOperationalCertificateIssuer_queueSelector
  , setProductAttestationAuthorityCertificatesSelector
  , setShouldAdvertiseOperationalSelector
  , setStorageBehaviorConfigurationSelector
  , shouldAdvertiseOperationalSelector
  , storageBehaviorConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set an MTROperationalCertificateIssuer to call (on the provided queue) when operational certificates need to be provided during commissioning.
--
-- ObjC selector: @- setOperationalCertificateIssuer:queue:@
setOperationalCertificateIssuer_queue :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSObject queue) => mtrDeviceControllerParameters -> RawId -> queue -> IO ()
setOperationalCertificateIssuer_queue mtrDeviceControllerParameters operationalCertificateIssuer queue =
  sendMessage mtrDeviceControllerParameters setOperationalCertificateIssuer_queueSelector operationalCertificateIssuer (toNSObject queue)

-- | Set an MTROTAProviderDelegate to call (on the provided queue).  Only needs to be called if this controller should be able to handle OTA for devices.
--
-- ObjC selector: @- setOTAProviderDelegate:queue:@
setOTAProviderDelegate_queue :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSObject queue) => mtrDeviceControllerParameters -> RawId -> queue -> IO ()
setOTAProviderDelegate_queue mtrDeviceControllerParameters otaProviderDelegate queue =
  sendMessage mtrDeviceControllerParameters setOTAProviderDelegate_queueSelector otaProviderDelegate (toNSObject queue)

-- | The Product Attestation Authority certificates that are trusted to sign device attestation information (and in particular to sign Product Attestation Intermediate certificates, which then sign Device Attestation Certificates).
--
-- Defaults to nil.
--
-- ObjC selector: @- productAttestationAuthorityCertificates@
productAttestationAuthorityCertificates :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id NSArray)
productAttestationAuthorityCertificates mtrDeviceControllerParameters =
  sendMessage mtrDeviceControllerParameters productAttestationAuthorityCertificatesSelector

-- | The Product Attestation Authority certificates that are trusted to sign device attestation information (and in particular to sign Product Attestation Intermediate certificates, which then sign Device Attestation Certificates).
--
-- Defaults to nil.
--
-- ObjC selector: @- setProductAttestationAuthorityCertificates:@
setProductAttestationAuthorityCertificates :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSArray value) => mtrDeviceControllerParameters -> value -> IO ()
setProductAttestationAuthorityCertificates mtrDeviceControllerParameters value =
  sendMessage mtrDeviceControllerParameters setProductAttestationAuthorityCertificatesSelector (toNSArray value)

-- | The Certification Declaration certificates whose public keys correspond to private keys that are trusted to sign certification declarations.  Defaults to nil.
--
-- These certificates are used in addition to, not replacing, the default set of well-known certification declaration signing keys.
--
-- ObjC selector: @- certificationDeclarationCertificates@
certificationDeclarationCertificates :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id NSArray)
certificationDeclarationCertificates mtrDeviceControllerParameters =
  sendMessage mtrDeviceControllerParameters certificationDeclarationCertificatesSelector

-- | The Certification Declaration certificates whose public keys correspond to private keys that are trusted to sign certification declarations.  Defaults to nil.
--
-- These certificates are used in addition to, not replacing, the default set of well-known certification declaration signing keys.
--
-- ObjC selector: @- setCertificationDeclarationCertificates:@
setCertificationDeclarationCertificates :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsNSArray value) => mtrDeviceControllerParameters -> value -> IO ()
setCertificationDeclarationCertificates mtrDeviceControllerParameters value =
  sendMessage mtrDeviceControllerParameters setCertificationDeclarationCertificatesSelector (toNSArray value)

-- | Whether the controller should advertise its operational identity.  Defaults to NO.
--
-- ObjC selector: @- shouldAdvertiseOperational@
shouldAdvertiseOperational :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO Bool
shouldAdvertiseOperational mtrDeviceControllerParameters =
  sendMessage mtrDeviceControllerParameters shouldAdvertiseOperationalSelector

-- | Whether the controller should advertise its operational identity.  Defaults to NO.
--
-- ObjC selector: @- setShouldAdvertiseOperational:@
setShouldAdvertiseOperational :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> Bool -> IO ()
setShouldAdvertiseOperational mtrDeviceControllerParameters value =
  sendMessage mtrDeviceControllerParameters setShouldAdvertiseOperationalSelector value

-- | Sets the maximum simultaneous subscription establishments that can be happening at one time for devices on Thread. This defaults to a large number.
--
-- If this value is 0, the maximum subscription establishments allowed at a time will be set to 1.
--
-- ObjC selector: @- concurrentSubscriptionEstablishmentsAllowedOnThread@
concurrentSubscriptionEstablishmentsAllowedOnThread :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO CULong
concurrentSubscriptionEstablishmentsAllowedOnThread mtrDeviceControllerParameters =
  sendMessage mtrDeviceControllerParameters concurrentSubscriptionEstablishmentsAllowedOnThreadSelector

-- | Sets the maximum simultaneous subscription establishments that can be happening at one time for devices on Thread. This defaults to a large number.
--
-- If this value is 0, the maximum subscription establishments allowed at a time will be set to 1.
--
-- ObjC selector: @- setConcurrentSubscriptionEstablishmentsAllowedOnThread:@
setConcurrentSubscriptionEstablishmentsAllowedOnThread :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> CULong -> IO ()
setConcurrentSubscriptionEstablishmentsAllowedOnThread mtrDeviceControllerParameters value =
  sendMessage mtrDeviceControllerParameters setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector value

-- | Sets the storage behavior configuration - see MTRDeviceStorageBehaviorConfiguration.h for details
--
-- If this value is nil, a default storage behavior configuration will be used.
--
-- ObjC selector: @- storageBehaviorConfiguration@
storageBehaviorConfiguration :: IsMTRDeviceControllerParameters mtrDeviceControllerParameters => mtrDeviceControllerParameters -> IO (Id MTRDeviceStorageBehaviorConfiguration)
storageBehaviorConfiguration mtrDeviceControllerParameters =
  sendMessage mtrDeviceControllerParameters storageBehaviorConfigurationSelector

-- | Sets the storage behavior configuration - see MTRDeviceStorageBehaviorConfiguration.h for details
--
-- If this value is nil, a default storage behavior configuration will be used.
--
-- ObjC selector: @- setStorageBehaviorConfiguration:@
setStorageBehaviorConfiguration :: (IsMTRDeviceControllerParameters mtrDeviceControllerParameters, IsMTRDeviceStorageBehaviorConfiguration value) => mtrDeviceControllerParameters -> value -> IO ()
setStorageBehaviorConfiguration mtrDeviceControllerParameters value =
  sendMessage mtrDeviceControllerParameters setStorageBehaviorConfigurationSelector (toMTRDeviceStorageBehaviorConfiguration value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setOperationalCertificateIssuer:queue:@
setOperationalCertificateIssuer_queueSelector :: Selector '[RawId, Id NSObject] ()
setOperationalCertificateIssuer_queueSelector = mkSelector "setOperationalCertificateIssuer:queue:"

-- | @Selector@ for @setOTAProviderDelegate:queue:@
setOTAProviderDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setOTAProviderDelegate_queueSelector = mkSelector "setOTAProviderDelegate:queue:"

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

-- | @Selector@ for @shouldAdvertiseOperational@
shouldAdvertiseOperationalSelector :: Selector '[] Bool
shouldAdvertiseOperationalSelector = mkSelector "shouldAdvertiseOperational"

-- | @Selector@ for @setShouldAdvertiseOperational:@
setShouldAdvertiseOperationalSelector :: Selector '[Bool] ()
setShouldAdvertiseOperationalSelector = mkSelector "setShouldAdvertiseOperational:"

-- | @Selector@ for @concurrentSubscriptionEstablishmentsAllowedOnThread@
concurrentSubscriptionEstablishmentsAllowedOnThreadSelector :: Selector '[] CULong
concurrentSubscriptionEstablishmentsAllowedOnThreadSelector = mkSelector "concurrentSubscriptionEstablishmentsAllowedOnThread"

-- | @Selector@ for @setConcurrentSubscriptionEstablishmentsAllowedOnThread:@
setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector :: Selector '[CULong] ()
setConcurrentSubscriptionEstablishmentsAllowedOnThreadSelector = mkSelector "setConcurrentSubscriptionEstablishmentsAllowedOnThread:"

-- | @Selector@ for @storageBehaviorConfiguration@
storageBehaviorConfigurationSelector :: Selector '[] (Id MTRDeviceStorageBehaviorConfiguration)
storageBehaviorConfigurationSelector = mkSelector "storageBehaviorConfiguration"

-- | @Selector@ for @setStorageBehaviorConfiguration:@
setStorageBehaviorConfigurationSelector :: Selector '[Id MTRDeviceStorageBehaviorConfiguration] ()
setStorageBehaviorConfigurationSelector = mkSelector "setStorageBehaviorConfiguration:"

