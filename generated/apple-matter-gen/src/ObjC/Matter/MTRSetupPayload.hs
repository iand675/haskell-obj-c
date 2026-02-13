{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A Matter Onboarding Payload.
--
-- It can be represented as a numeric Manual Pairing Code or as QR Code. The QR Code format contains more information though, so creating a QR Code from a payload that was initialized from a Manual Pairing Code will not work, because some required information will be missing.
--
-- This class can also be used to create an onboarding payload directly from the underlying values (passcode, discriminator, etc).
--
-- Generated bindings for @MTRSetupPayload@.
module ObjC.Matter.MTRSetupPayload
  ( MTRSetupPayload
  , IsMTRSetupPayload(..)
  , initWithPayload
  , vendorElementWithTag
  , removeVendorElementWithTag
  , addOrReplaceVendorElement
  , generateRandomPIN
  , generateRandomSetupPasscode
  , initWithSetupPasscode_discriminator
  , manualEntryCode
  , qrCodeString
  , isValidSetupPasscode
  , init_
  , new
  , setupPayloadWithOnboardingPayload_error
  , getAllOptionalVendorData
  , concatenated
  , subPayloads
  , setSubPayloads
  , version
  , setVersion
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , commissioningFlow
  , setCommissioningFlow
  , discoveryCapabilities
  , setDiscoveryCapabilities
  , discriminator
  , setDiscriminator
  , hasShortDiscriminator
  , setHasShortDiscriminator
  , setupPasscode
  , setSetupPasscode
  , serialNumber
  , setSerialNumber
  , vendorElements
  , rendezvousInformation
  , setRendezvousInformation
  , setUpPINCode
  , setSetUpPINCode
  , addOrReplaceVendorElementSelector
  , commissioningFlowSelector
  , concatenatedSelector
  , discoveryCapabilitiesSelector
  , discriminatorSelector
  , generateRandomPINSelector
  , generateRandomSetupPasscodeSelector
  , getAllOptionalVendorDataSelector
  , hasShortDiscriminatorSelector
  , initSelector
  , initWithPayloadSelector
  , initWithSetupPasscode_discriminatorSelector
  , isValidSetupPasscodeSelector
  , manualEntryCodeSelector
  , newSelector
  , productIDSelector
  , qrCodeStringSelector
  , removeVendorElementWithTagSelector
  , rendezvousInformationSelector
  , serialNumberSelector
  , setCommissioningFlowSelector
  , setDiscoveryCapabilitiesSelector
  , setDiscriminatorSelector
  , setHasShortDiscriminatorSelector
  , setProductIDSelector
  , setRendezvousInformationSelector
  , setSerialNumberSelector
  , setSetUpPINCodeSelector
  , setSetupPasscodeSelector
  , setSubPayloadsSelector
  , setUpPINCodeSelector
  , setVendorIDSelector
  , setVersionSelector
  , setupPasscodeSelector
  , setupPayloadWithOnboardingPayload_errorSelector
  , subPayloadsSelector
  , vendorElementWithTagSelector
  , vendorElementsSelector
  , vendorIDSelector
  , versionSelector

  -- * Enum types
  , MTRCommissioningFlow(MTRCommissioningFlow)
  , pattern MTRCommissioningFlowStandard
  , pattern MTRCommissioningFlowUserActionRequired
  , pattern MTRCommissioningFlowCustom
  , pattern MTRCommissioningFlowInvalid
  , MTRDiscoveryCapabilities(MTRDiscoveryCapabilities)
  , pattern MTRDiscoveryCapabilitiesUnknown
  , pattern MTRDiscoveryCapabilitiesNone
  , pattern MTRDiscoveryCapabilitiesSoftAP
  , pattern MTRDiscoveryCapabilitiesBLE
  , pattern MTRDiscoveryCapabilitiesOnNetwork
  , pattern MTRDiscoveryCapabilitiesNFC
  , pattern MTRDiscoveryCapabilitiesAllMask

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes the payload object from the provide QR Code or Manual Pairing Code string. Returns nil if the payload is not valid.
--
-- ObjC selector: @- initWithPayload:@
initWithPayload :: (IsMTRSetupPayload mtrSetupPayload, IsNSString payload) => mtrSetupPayload -> payload -> IO (Id MTRSetupPayload)
initWithPayload mtrSetupPayload payload =
  sendOwnedMessage mtrSetupPayload initWithPayloadSelector (toNSString payload)

-- | Returns the Manufacturer-specific extension element with the specified tag, if any. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- vendorElementWithTag:@
vendorElementWithTag :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber tag) => mtrSetupPayload -> tag -> IO (Id MTROptionalQRCodeInfo)
vendorElementWithTag mtrSetupPayload tag =
  sendMessage mtrSetupPayload vendorElementWithTagSelector (toNSNumber tag)

-- | Removes the extension element with the specified tag, if any. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- removeVendorElementWithTag:@
removeVendorElementWithTag :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber tag) => mtrSetupPayload -> tag -> IO ()
removeVendorElementWithTag mtrSetupPayload tag =
  sendMessage mtrSetupPayload removeVendorElementWithTagSelector (toNSNumber tag)

-- | Adds or replaces a Manufacturer-specific extension element.
--
-- ObjC selector: @- addOrReplaceVendorElement:@
addOrReplaceVendorElement :: (IsMTRSetupPayload mtrSetupPayload, IsMTROptionalQRCodeInfo element) => mtrSetupPayload -> element -> IO ()
addOrReplaceVendorElement mtrSetupPayload element =
  sendMessage mtrSetupPayload addOrReplaceVendorElementSelector (toMTROptionalQRCodeInfo element)

-- | Generate a random Matter-valid setup PIN.
--
-- ObjC selector: @+ generateRandomPIN@
generateRandomPIN :: IO CULong
generateRandomPIN  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMessage cls' generateRandomPINSelector

-- | Generate a random Matter-valid setup passcode.
--
-- ObjC selector: @+ generateRandomSetupPasscode@
generateRandomSetupPasscode :: IO (Id NSNumber)
generateRandomSetupPasscode  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMessage cls' generateRandomSetupPasscodeSelector

-- | Initialize an MTRSetupPayload with the given passcode and discriminator. This will pre-set version, product id, and vendor id to 0.
--
-- ObjC selector: @- initWithSetupPasscode:discriminator:@
initWithSetupPasscode_discriminator :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber setupPasscode, IsNSNumber discriminator) => mtrSetupPayload -> setupPasscode -> discriminator -> IO (Id MTRSetupPayload)
initWithSetupPasscode_discriminator mtrSetupPayload setupPasscode discriminator =
  sendOwnedMessage mtrSetupPayload initWithSetupPasscode_discriminatorSelector (toNSNumber setupPasscode) (toNSNumber discriminator)

-- | Creates a Manual Pairing Code from this setup payload. Returns nil if this payload cannot be represented as a valid Manual Pairing Code.
--
-- The following properties must be populated for a valid Manual Pairing Code:  - setupPasscode  - discriminator (short or long)
--
-- In most cases the pairing code will be 11 digits long. If the payload indicates a @commissioningFlow@ other than @MTRCommissioningFlowStandard@, a 21 digit code will be produced that includes the vendorID and productID values.
--
-- ObjC selector: @- manualEntryCode@
manualEntryCode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
manualEntryCode mtrSetupPayload =
  sendMessage mtrSetupPayload manualEntryCodeSelector

-- | Creates a QR Code payload from this setup payload. Returns nil if this payload cannot be represented as a valid QR Code.
--
-- The following properties must be populated for a valid QR Code: - setupPasscode - discriminator (must be long) - discoveryCapabilities (not MTRDiscoveryCapabilitiesUnknown)
--
-- If this object represents a @concatenated@ payload, then this property will include the QR Code strings of all the underlying @subPayloads.@
--
-- ObjC selector: @- qrCodeString@
qrCodeString :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
qrCodeString mtrSetupPayload =
  sendMessage mtrSetupPayload qrCodeStringSelector

-- | Check whether the provided setup passcode (represented as an unsigned integer) is a valid setup passcode.
--
-- ObjC selector: @+ isValidSetupPasscode:@
isValidSetupPasscode :: IsNSNumber setupPasscode => setupPasscode -> IO Bool
isValidSetupPasscode setupPasscode =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMessage cls' isValidSetupPasscodeSelector (toNSNumber setupPasscode)

-- | @- init@
init_ :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id MTRSetupPayload)
init_ mtrSetupPayload =
  sendOwnedMessage mtrSetupPayload initSelector

-- | @+ new@
new :: IO (Id MTRSetupPayload)
new  =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendOwnedClassMessage cls' newSelector

-- | @+ setupPayloadWithOnboardingPayload:error:@
setupPayloadWithOnboardingPayload_error :: (IsNSString onboardingPayload, IsNSError error_) => onboardingPayload -> error_ -> IO (Id MTRSetupPayload)
setupPayloadWithOnboardingPayload_error onboardingPayload error_ =
  do
    cls' <- getRequiredClass "MTRSetupPayload"
    sendClassMessage cls' setupPayloadWithOnboardingPayload_errorSelector (toNSString onboardingPayload) (toNSError error_)

-- | @- getAllOptionalVendorData:@
getAllOptionalVendorData :: (IsMTRSetupPayload mtrSetupPayload, IsNSError error_) => mtrSetupPayload -> error_ -> IO (Id NSArray)
getAllOptionalVendorData mtrSetupPayload error_ =
  sendMessage mtrSetupPayload getAllOptionalVendorDataSelector (toNSError error_)

-- | Whether this object represents a concatenated QR Code payload consisting of two or more underlying payloads. If YES, then:
--
-- - The constituent payloads are exposed in the @subPayloads@ property.
--
-- - Properties other than @subPayloads@ and @qrCodeString@ (e.g. @vendorID@, @discriminator@)   are not relevant to a concatenated payload and should not be used. If accessed, they will   act as if the payload was not in fact concatenated, and return the relevant value associated   with the first sub-payload. Mutating such a property will discard the additional sub-payloads.
--
-- ObjC selector: @- concatenated@
concatenated :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO Bool
concatenated mtrSetupPayload =
  sendMessage mtrSetupPayload concatenatedSelector

-- | The individual constituent payloads, if the receiver represents a concatenated payload.
--
-- See: concatenated
--
-- ObjC selector: @- subPayloads@
subPayloads :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSArray)
subPayloads mtrSetupPayload =
  sendMessage mtrSetupPayload subPayloadsSelector

-- | The individual constituent payloads, if the receiver represents a concatenated payload.
--
-- See: concatenated
--
-- ObjC selector: @- setSubPayloads:@
setSubPayloads :: (IsMTRSetupPayload mtrSetupPayload, IsNSArray value) => mtrSetupPayload -> value -> IO ()
setSubPayloads mtrSetupPayload value =
  sendMessage mtrSetupPayload setSubPayloadsSelector (toNSArray value)

-- | @- version@
version :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
version mtrSetupPayload =
  sendMessage mtrSetupPayload versionSelector

-- | @- setVersion:@
setVersion :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setVersion mtrSetupPayload value =
  sendMessage mtrSetupPayload setVersionSelector (toNSNumber value)

-- | @- vendorID@
vendorID :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
vendorID mtrSetupPayload =
  sendMessage mtrSetupPayload vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setVendorID mtrSetupPayload value =
  sendMessage mtrSetupPayload setVendorIDSelector (toNSNumber value)

-- | @- productID@
productID :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
productID mtrSetupPayload =
  sendMessage mtrSetupPayload productIDSelector

-- | @- setProductID:@
setProductID :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setProductID mtrSetupPayload value =
  sendMessage mtrSetupPayload setProductIDSelector (toNSNumber value)

-- | @- commissioningFlow@
commissioningFlow :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO MTRCommissioningFlow
commissioningFlow mtrSetupPayload =
  sendMessage mtrSetupPayload commissioningFlowSelector

-- | @- setCommissioningFlow:@
setCommissioningFlow :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> MTRCommissioningFlow -> IO ()
setCommissioningFlow mtrSetupPayload value =
  sendMessage mtrSetupPayload setCommissioningFlowSelector value

-- | The value of discoveryCapabilities is made up of the various MTRDiscoveryCapabilities flags.  If the discovery capabilities are not known, this will be set to MTRDiscoveryCapabilitiesUnknown.
--
-- ObjC selector: @- discoveryCapabilities@
discoveryCapabilities :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO MTRDiscoveryCapabilities
discoveryCapabilities mtrSetupPayload =
  sendMessage mtrSetupPayload discoveryCapabilitiesSelector

-- | The value of discoveryCapabilities is made up of the various MTRDiscoveryCapabilities flags.  If the discovery capabilities are not known, this will be set to MTRDiscoveryCapabilitiesUnknown.
--
-- ObjC selector: @- setDiscoveryCapabilities:@
setDiscoveryCapabilities :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> MTRDiscoveryCapabilities -> IO ()
setDiscoveryCapabilities mtrSetupPayload value =
  sendMessage mtrSetupPayload setDiscoveryCapabilitiesSelector value

-- | @- discriminator@
discriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
discriminator mtrSetupPayload =
  sendMessage mtrSetupPayload discriminatorSelector

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setDiscriminator mtrSetupPayload value =
  sendMessage mtrSetupPayload setDiscriminatorSelector (toNSNumber value)

-- | If hasShortDiscriminator is true, the discriminator value contains just the high 4 bits of the full discriminator.  For example, if hasShortDiscriminator is true and discriminator is 0xA, then the full discriminator can be anything in the range 0xA00 to 0xAFF.
--
-- ObjC selector: @- hasShortDiscriminator@
hasShortDiscriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO Bool
hasShortDiscriminator mtrSetupPayload =
  sendMessage mtrSetupPayload hasShortDiscriminatorSelector

-- | If hasShortDiscriminator is true, the discriminator value contains just the high 4 bits of the full discriminator.  For example, if hasShortDiscriminator is true and discriminator is 0xA, then the full discriminator can be anything in the range 0xA00 to 0xAFF.
--
-- ObjC selector: @- setHasShortDiscriminator:@
setHasShortDiscriminator :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> Bool -> IO ()
setHasShortDiscriminator mtrSetupPayload value =
  sendMessage mtrSetupPayload setHasShortDiscriminatorSelector value

-- | @- setupPasscode@
setupPasscode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
setupPasscode mtrSetupPayload =
  sendMessage mtrSetupPayload setupPasscodeSelector

-- | @- setSetupPasscode:@
setSetupPasscode :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setSetupPasscode mtrSetupPayload value =
  sendMessage mtrSetupPayload setSetupPasscodeSelector (toNSNumber value)

-- | The value of the Serial Number extension element, if any.
--
-- ObjC selector: @- serialNumber@
serialNumber :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSString)
serialNumber mtrSetupPayload =
  sendMessage mtrSetupPayload serialNumberSelector

-- | The value of the Serial Number extension element, if any.
--
-- ObjC selector: @- setSerialNumber:@
setSerialNumber :: (IsMTRSetupPayload mtrSetupPayload, IsNSString value) => mtrSetupPayload -> value -> IO ()
setSerialNumber mtrSetupPayload value =
  sendMessage mtrSetupPayload setSerialNumberSelector (toNSString value)

-- | The list of Manufacturer-specific extension elements contained in the setup code. May be empty.
--
-- ObjC selector: @- vendorElements@
vendorElements :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSArray)
vendorElements mtrSetupPayload =
  sendMessage mtrSetupPayload vendorElementsSelector

-- | @- rendezvousInformation@
rendezvousInformation :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
rendezvousInformation mtrSetupPayload =
  sendMessage mtrSetupPayload rendezvousInformationSelector

-- | @- setRendezvousInformation:@
setRendezvousInformation :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setRendezvousInformation mtrSetupPayload value =
  sendMessage mtrSetupPayload setRendezvousInformationSelector (toNSNumber value)

-- | @- setUpPINCode@
setUpPINCode :: IsMTRSetupPayload mtrSetupPayload => mtrSetupPayload -> IO (Id NSNumber)
setUpPINCode mtrSetupPayload =
  sendMessage mtrSetupPayload setUpPINCodeSelector

-- | @- setSetUpPINCode:@
setSetUpPINCode :: (IsMTRSetupPayload mtrSetupPayload, IsNSNumber value) => mtrSetupPayload -> value -> IO ()
setSetUpPINCode mtrSetupPayload value =
  sendMessage mtrSetupPayload setSetUpPINCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayload:@
initWithPayloadSelector :: Selector '[Id NSString] (Id MTRSetupPayload)
initWithPayloadSelector = mkSelector "initWithPayload:"

-- | @Selector@ for @vendorElementWithTag:@
vendorElementWithTagSelector :: Selector '[Id NSNumber] (Id MTROptionalQRCodeInfo)
vendorElementWithTagSelector = mkSelector "vendorElementWithTag:"

-- | @Selector@ for @removeVendorElementWithTag:@
removeVendorElementWithTagSelector :: Selector '[Id NSNumber] ()
removeVendorElementWithTagSelector = mkSelector "removeVendorElementWithTag:"

-- | @Selector@ for @addOrReplaceVendorElement:@
addOrReplaceVendorElementSelector :: Selector '[Id MTROptionalQRCodeInfo] ()
addOrReplaceVendorElementSelector = mkSelector "addOrReplaceVendorElement:"

-- | @Selector@ for @generateRandomPIN@
generateRandomPINSelector :: Selector '[] CULong
generateRandomPINSelector = mkSelector "generateRandomPIN"

-- | @Selector@ for @generateRandomSetupPasscode@
generateRandomSetupPasscodeSelector :: Selector '[] (Id NSNumber)
generateRandomSetupPasscodeSelector = mkSelector "generateRandomSetupPasscode"

-- | @Selector@ for @initWithSetupPasscode:discriminator:@
initWithSetupPasscode_discriminatorSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MTRSetupPayload)
initWithSetupPasscode_discriminatorSelector = mkSelector "initWithSetupPasscode:discriminator:"

-- | @Selector@ for @manualEntryCode@
manualEntryCodeSelector :: Selector '[] (Id NSString)
manualEntryCodeSelector = mkSelector "manualEntryCode"

-- | @Selector@ for @qrCodeString@
qrCodeStringSelector :: Selector '[] (Id NSString)
qrCodeStringSelector = mkSelector "qrCodeString"

-- | @Selector@ for @isValidSetupPasscode:@
isValidSetupPasscodeSelector :: Selector '[Id NSNumber] Bool
isValidSetupPasscodeSelector = mkSelector "isValidSetupPasscode:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRSetupPayload)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRSetupPayload)
newSelector = mkSelector "new"

-- | @Selector@ for @setupPayloadWithOnboardingPayload:error:@
setupPayloadWithOnboardingPayload_errorSelector :: Selector '[Id NSString, Id NSError] (Id MTRSetupPayload)
setupPayloadWithOnboardingPayload_errorSelector = mkSelector "setupPayloadWithOnboardingPayload:error:"

-- | @Selector@ for @getAllOptionalVendorData:@
getAllOptionalVendorDataSelector :: Selector '[Id NSError] (Id NSArray)
getAllOptionalVendorDataSelector = mkSelector "getAllOptionalVendorData:"

-- | @Selector@ for @concatenated@
concatenatedSelector :: Selector '[] Bool
concatenatedSelector = mkSelector "concatenated"

-- | @Selector@ for @subPayloads@
subPayloadsSelector :: Selector '[] (Id NSArray)
subPayloadsSelector = mkSelector "subPayloads"

-- | @Selector@ for @setSubPayloads:@
setSubPayloadsSelector :: Selector '[Id NSArray] ()
setSubPayloadsSelector = mkSelector "setSubPayloads:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSNumber)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSNumber] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector '[Id NSNumber] ()
setProductIDSelector = mkSelector "setProductID:"

-- | @Selector@ for @commissioningFlow@
commissioningFlowSelector :: Selector '[] MTRCommissioningFlow
commissioningFlowSelector = mkSelector "commissioningFlow"

-- | @Selector@ for @setCommissioningFlow:@
setCommissioningFlowSelector :: Selector '[MTRCommissioningFlow] ()
setCommissioningFlowSelector = mkSelector "setCommissioningFlow:"

-- | @Selector@ for @discoveryCapabilities@
discoveryCapabilitiesSelector :: Selector '[] MTRDiscoveryCapabilities
discoveryCapabilitiesSelector = mkSelector "discoveryCapabilities"

-- | @Selector@ for @setDiscoveryCapabilities:@
setDiscoveryCapabilitiesSelector :: Selector '[MTRDiscoveryCapabilities] ()
setDiscoveryCapabilitiesSelector = mkSelector "setDiscoveryCapabilities:"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector '[] (Id NSNumber)
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @setDiscriminator:@
setDiscriminatorSelector :: Selector '[Id NSNumber] ()
setDiscriminatorSelector = mkSelector "setDiscriminator:"

-- | @Selector@ for @hasShortDiscriminator@
hasShortDiscriminatorSelector :: Selector '[] Bool
hasShortDiscriminatorSelector = mkSelector "hasShortDiscriminator"

-- | @Selector@ for @setHasShortDiscriminator:@
setHasShortDiscriminatorSelector :: Selector '[Bool] ()
setHasShortDiscriminatorSelector = mkSelector "setHasShortDiscriminator:"

-- | @Selector@ for @setupPasscode@
setupPasscodeSelector :: Selector '[] (Id NSNumber)
setupPasscodeSelector = mkSelector "setupPasscode"

-- | @Selector@ for @setSetupPasscode:@
setSetupPasscodeSelector :: Selector '[Id NSNumber] ()
setSetupPasscodeSelector = mkSelector "setSetupPasscode:"

-- | @Selector@ for @serialNumber@
serialNumberSelector :: Selector '[] (Id NSString)
serialNumberSelector = mkSelector "serialNumber"

-- | @Selector@ for @setSerialNumber:@
setSerialNumberSelector :: Selector '[Id NSString] ()
setSerialNumberSelector = mkSelector "setSerialNumber:"

-- | @Selector@ for @vendorElements@
vendorElementsSelector :: Selector '[] (Id NSArray)
vendorElementsSelector = mkSelector "vendorElements"

-- | @Selector@ for @rendezvousInformation@
rendezvousInformationSelector :: Selector '[] (Id NSNumber)
rendezvousInformationSelector = mkSelector "rendezvousInformation"

-- | @Selector@ for @setRendezvousInformation:@
setRendezvousInformationSelector :: Selector '[Id NSNumber] ()
setRendezvousInformationSelector = mkSelector "setRendezvousInformation:"

-- | @Selector@ for @setUpPINCode@
setUpPINCodeSelector :: Selector '[] (Id NSNumber)
setUpPINCodeSelector = mkSelector "setUpPINCode"

-- | @Selector@ for @setSetUpPINCode:@
setSetUpPINCodeSelector :: Selector '[Id NSNumber] ()
setSetUpPINCodeSelector = mkSelector "setSetUpPINCode:"

