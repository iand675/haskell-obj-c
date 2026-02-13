{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceAttestationDeviceInfo@.
module ObjC.Matter.MTRDeviceAttestationDeviceInfo
  ( MTRDeviceAttestationDeviceInfo
  , IsMTRDeviceAttestationDeviceInfo(..)
  , init_
  , new
  , vendorID
  , productID
  , basicInformationVendorID
  , basicInformationProductID
  , dacCertificate
  , dacPAICertificate
  , certificateDeclaration
  , attestationChallenge
  , attestationNonce
  , elementsTLV
  , certificationDeclaration
  , elementsSignature
  , attestationChallengeSelector
  , attestationNonceSelector
  , basicInformationProductIDSelector
  , basicInformationVendorIDSelector
  , certificateDeclarationSelector
  , certificationDeclarationSelector
  , dacCertificateSelector
  , dacPAICertificateSelector
  , elementsSignatureSelector
  , elementsTLVSelector
  , initSelector
  , newSelector
  , productIDSelector
  , vendorIDSelector


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
init_ :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id MTRDeviceAttestationDeviceInfo)
init_ mtrDeviceAttestationDeviceInfo =
  sendOwnedMessage mtrDeviceAttestationDeviceInfo initSelector

-- | @+ new@
new :: IO (Id MTRDeviceAttestationDeviceInfo)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceAttestationDeviceInfo"
    sendOwnedClassMessage cls' newSelector

-- | The vendor ID from the Device Attestation Certificate. May be nil only if attestation verification failed.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
vendorID mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo vendorIDSelector

-- | The product ID from the Device Attestation Certificate. May be nil only if attestation verification failed.
--
-- ObjC selector: @- productID@
productID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
productID mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo productIDSelector

-- | The vendor ID value from the device's Basic Information cluster that was used for device attestation.  If attestation succeeds, this must match the vendor ID from the certification declaration.
--
-- ObjC selector: @- basicInformationVendorID@
basicInformationVendorID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
basicInformationVendorID mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo basicInformationVendorIDSelector

-- | The product ID value from the device's Basic Information cluster that was used for device attestation.  If attestation succeeds, this must match one of the product IDs from the certification declaration.
--
-- ObjC selector: @- basicInformationProductID@
basicInformationProductID :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSNumber)
basicInformationProductID mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo basicInformationProductIDSelector

-- | @- dacCertificate@
dacCertificate :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
dacCertificate mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo dacCertificateSelector

-- | @- dacPAICertificate@
dacPAICertificate :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
dacPAICertificate mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo dacPAICertificateSelector

-- | @- certificateDeclaration@
certificateDeclaration :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
certificateDeclaration mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo certificateDeclarationSelector

-- | The attestation challenge from the secure session.
--
-- ObjC selector: @- attestationChallenge@
attestationChallenge :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
attestationChallenge mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo attestationChallengeSelector

-- | The attestation nonce from the AttestationRequest command.
--
-- ObjC selector: @- attestationNonce@
attestationNonce :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
attestationNonce mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo attestationNonceSelector

-- | The TLV-encoded attestation_elements_message that was used to find the certificationDeclaration (possibly unsuccessfully).
--
-- ObjC selector: @- elementsTLV@
elementsTLV :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
elementsTLV mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo elementsTLVSelector

-- | The certification declaration of the device, if available.  This is a DER-encoded string representing a CMS-formatted certification declaration.  May be nil only if attestation verification failed.
--
-- ObjC selector: @- certificationDeclaration@
certificationDeclaration :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
certificationDeclaration mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo certificationDeclarationSelector

-- | A signature, using the device attestation private key of the device that sent the attestation information, over the concatenation of elementsTLV and attestationChallenge.
--
-- ObjC selector: @- elementsSignature@
elementsSignature :: IsMTRDeviceAttestationDeviceInfo mtrDeviceAttestationDeviceInfo => mtrDeviceAttestationDeviceInfo -> IO (Id NSData)
elementsSignature mtrDeviceAttestationDeviceInfo =
  sendMessage mtrDeviceAttestationDeviceInfo elementsSignatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceAttestationDeviceInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceAttestationDeviceInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @basicInformationVendorID@
basicInformationVendorIDSelector :: Selector '[] (Id NSNumber)
basicInformationVendorIDSelector = mkSelector "basicInformationVendorID"

-- | @Selector@ for @basicInformationProductID@
basicInformationProductIDSelector :: Selector '[] (Id NSNumber)
basicInformationProductIDSelector = mkSelector "basicInformationProductID"

-- | @Selector@ for @dacCertificate@
dacCertificateSelector :: Selector '[] (Id NSData)
dacCertificateSelector = mkSelector "dacCertificate"

-- | @Selector@ for @dacPAICertificate@
dacPAICertificateSelector :: Selector '[] (Id NSData)
dacPAICertificateSelector = mkSelector "dacPAICertificate"

-- | @Selector@ for @certificateDeclaration@
certificateDeclarationSelector :: Selector '[] (Id NSData)
certificateDeclarationSelector = mkSelector "certificateDeclaration"

-- | @Selector@ for @attestationChallenge@
attestationChallengeSelector :: Selector '[] (Id NSData)
attestationChallengeSelector = mkSelector "attestationChallenge"

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector '[] (Id NSData)
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @elementsTLV@
elementsTLVSelector :: Selector '[] (Id NSData)
elementsTLVSelector = mkSelector "elementsTLV"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector '[] (Id NSData)
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector '[] (Id NSData)
elementsSignatureSelector = mkSelector "elementsSignature"

