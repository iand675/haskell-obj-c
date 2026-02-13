{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents information relating to product attestation.
--
-- Generated bindings for @MTRDeviceAttestationInfo@.
module ObjC.Matter.MTRDeviceAttestationInfo
  ( MTRDeviceAttestationInfo
  , IsMTRDeviceAttestationInfo(..)
  , initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo
  , challenge
  , nonce
  , elementsTLV
  , elementsSignature
  , deviceAttestationCertificate
  , productAttestationIntermediateCertificate
  , certificationDeclaration
  , firmwareInfo
  , certificationDeclarationSelector
  , challengeSelector
  , deviceAttestationCertificateSelector
  , elementsSignatureSelector
  , elementsTLVSelector
  , firmwareInfoSelector
  , initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector
  , nonceSelector
  , productAttestationIntermediateCertificateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:@
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo :: (IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo, IsNSData challenge, IsNSData nonce, IsNSData elementsTLV, IsNSData elementsSignature, IsNSData deviceAttestationCertificate, IsNSData processAttestationIntermediateCertificate, IsNSData certificationDeclaration, IsNSData firmwareInfo) => mtrDeviceAttestationInfo -> challenge -> nonce -> elementsTLV -> elementsSignature -> deviceAttestationCertificate -> processAttestationIntermediateCertificate -> certificationDeclaration -> firmwareInfo -> IO (Id MTRDeviceAttestationInfo)
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfo mtrDeviceAttestationInfo challenge nonce elementsTLV elementsSignature deviceAttestationCertificate processAttestationIntermediateCertificate certificationDeclaration firmwareInfo =
  sendOwnedMessage mtrDeviceAttestationInfo initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector (toNSData challenge) (toNSData nonce) (toNSData elementsTLV) (toNSData elementsSignature) (toNSData deviceAttestationCertificate) (toNSData processAttestationIntermediateCertificate) (toNSData certificationDeclaration) (toNSData firmwareInfo)

-- | The attestation challenge from the secure session.
--
-- ObjC selector: @- challenge@
challenge :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
challenge mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo challengeSelector

-- | The attestation nonce from the AttestationRequest command.
--
-- ObjC selector: @- nonce@
nonce :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
nonce mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo nonceSelector

-- | The TLV-encoded attestation_elements_message that was used to find the certificationDeclaration and firmwareInfo.
--
-- ObjC selector: @- elementsTLV@
elementsTLV :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
elementsTLV mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo elementsTLVSelector

-- | A signature, using the device attestation private key of the device that sent the attestation information, over the concatenation of elementsTLV and the attestation challenge from the secure session.
--
-- ObjC selector: @- elementsSignature@
elementsSignature :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
elementsSignature mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo elementsSignatureSelector

-- | The device attestation certificate for the device.  This can be used to verify signatures created with the device attestation private key.
--
-- ObjC selector: @- deviceAttestationCertificate@
deviceAttestationCertificate :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
deviceAttestationCertificate mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo deviceAttestationCertificateSelector

-- | The product attestation intermediate certificate that can be used to verify the authenticity of the device attestation certificate.
--
-- ObjC selector: @- productAttestationIntermediateCertificate@
productAttestationIntermediateCertificate :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
productAttestationIntermediateCertificate mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo productAttestationIntermediateCertificateSelector

-- | The certification declaration of the device.  This is a DER-encoded string representing a CMS-formatted certification declaration.
--
-- ObjC selector: @- certificationDeclaration@
certificationDeclaration :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
certificationDeclaration mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo certificationDeclarationSelector

-- | @- firmwareInfo@
firmwareInfo :: IsMTRDeviceAttestationInfo mtrDeviceAttestationInfo => mtrDeviceAttestationInfo -> IO (Id NSData)
firmwareInfo mtrDeviceAttestationInfo =
  sendMessage mtrDeviceAttestationInfo firmwareInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:@
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector :: Selector '[Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData] (Id MTRDeviceAttestationInfo)
initWithDeviceAttestationChallenge_nonce_elementsTLV_elementsSignature_deviceAttestationCertificate_productAttestationIntermediateCertificate_certificationDeclaration_firmwareInfoSelector = mkSelector "initWithDeviceAttestationChallenge:nonce:elementsTLV:elementsSignature:deviceAttestationCertificate:productAttestationIntermediateCertificate:certificationDeclaration:firmwareInfo:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector '[] (Id NSData)
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSData)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @elementsTLV@
elementsTLVSelector :: Selector '[] (Id NSData)
elementsTLVSelector = mkSelector "elementsTLV"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector '[] (Id NSData)
elementsSignatureSelector = mkSelector "elementsSignature"

-- | @Selector@ for @deviceAttestationCertificate@
deviceAttestationCertificateSelector :: Selector '[] (Id NSData)
deviceAttestationCertificateSelector = mkSelector "deviceAttestationCertificate"

-- | @Selector@ for @productAttestationIntermediateCertificate@
productAttestationIntermediateCertificateSelector :: Selector '[] (Id NSData)
productAttestationIntermediateCertificateSelector = mkSelector "productAttestationIntermediateCertificate"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector '[] (Id NSData)
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @firmwareInfo@
firmwareInfoSelector :: Selector '[] (Id NSData)
firmwareInfoSelector = mkSelector "firmwareInfo"

