{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPassLibrary@.
module ObjC.PassKit.PKPassLibrary
  ( PKPassLibrary
  , IsPKPassLibrary(..)
  , isPassLibraryAvailable
  , requestAutomaticPassPresentationSuppressionWithResponseHandler
  , endAutomaticPassPresentationSuppressionWithRequestToken
  , isSuppressingAutomaticPassPresentation
  , pkPassLibraryIsPaymentPassActivationAvailable
  , isPaymentPassActivationAvailable
  , passes
  , passWithPassTypeIdentifier_serialNumber
  , passesWithReaderIdentifier
  , passesOfType
  , remotePaymentPasses
  , removePass
  , containsPass
  , replacePassWithPass
  , addPasses_withCompletionHandler
  , openPaymentSetup
  , presentPaymentPass
  , presentSecureElementPass
  , canAddPaymentPassWithPrimaryAccountIdentifier
  , canAddSecureElementPassWithPrimaryAccountIdentifier
  , canAddFelicaPass
  , activatePaymentPass_withActivationData_completion
  , activatePaymentPass_withActivationCode_completion
  , activateSecureElementPass_withActivationData_completion
  , signData_withSecureElementPass_completion
  , encryptedServiceProviderDataForSecureElementPass_completion
  , serviceProviderDataForSecureElementPass_completion
  , authorizationStatusForCapability
  , requestAuthorizationForCapability_completion
  , secureElementPassActivationAvailable
  , remoteSecureElementPasses
  , isPassLibraryAvailableSelector
  , requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector
  , endAutomaticPassPresentationSuppressionWithRequestTokenSelector
  , isSuppressingAutomaticPassPresentationSelector
  , isPaymentPassActivationAvailableSelector
  , passesSelector
  , passWithPassTypeIdentifier_serialNumberSelector
  , passesWithReaderIdentifierSelector
  , passesOfTypeSelector
  , remotePaymentPassesSelector
  , removePassSelector
  , containsPassSelector
  , replacePassWithPassSelector
  , addPasses_withCompletionHandlerSelector
  , openPaymentSetupSelector
  , presentPaymentPassSelector
  , presentSecureElementPassSelector
  , canAddPaymentPassWithPrimaryAccountIdentifierSelector
  , canAddSecureElementPassWithPrimaryAccountIdentifierSelector
  , canAddFelicaPassSelector
  , activatePaymentPass_withActivationData_completionSelector
  , activatePaymentPass_withActivationCode_completionSelector
  , activateSecureElementPass_withActivationData_completionSelector
  , signData_withSecureElementPass_completionSelector
  , encryptedServiceProviderDataForSecureElementPass_completionSelector
  , serviceProviderDataForSecureElementPass_completionSelector
  , authorizationStatusForCapabilitySelector
  , requestAuthorizationForCapability_completionSelector
  , secureElementPassActivationAvailableSelector
  , remoteSecureElementPassesSelector

  -- * Enum types
  , PKPassLibraryAuthorizationStatus(PKPassLibraryAuthorizationStatus)
  , pattern PKPassLibraryAuthorizationStatusNotDetermined
  , pattern PKPassLibraryAuthorizationStatusDenied
  , pattern PKPassLibraryAuthorizationStatusAuthorized
  , pattern PKPassLibraryAuthorizationStatusRestricted
  , PKPassLibraryCapability(PKPassLibraryCapability)
  , pattern PKPassLibraryCapabilityBackgroundAddPasses
  , PKPassType(PKPassType)
  , pattern PKPassTypeBarcode
  , pattern PKPassTypeSecureElement
  , pattern PKPassTypePayment
  , pattern PKPassTypeAny

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ isPassLibraryAvailable@
isPassLibraryAvailable :: IO Bool
isPassLibraryAvailable  =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPassLibraryAvailable") retCULong []

-- | @+ requestAutomaticPassPresentationSuppressionWithResponseHandler:@
requestAutomaticPassPresentationSuppressionWithResponseHandler :: Ptr () -> IO CULong
requestAutomaticPassPresentationSuppressionWithResponseHandler responseHandler =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMsg cls' (mkSelector "requestAutomaticPassPresentationSuppressionWithResponseHandler:") retCULong [argPtr (castPtr responseHandler :: Ptr ())]

-- | @+ endAutomaticPassPresentationSuppressionWithRequestToken:@
endAutomaticPassPresentationSuppressionWithRequestToken :: CULong -> IO ()
endAutomaticPassPresentationSuppressionWithRequestToken requestToken =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMsg cls' (mkSelector "endAutomaticPassPresentationSuppressionWithRequestToken:") retVoid [argCULong requestToken]

-- | @+ isSuppressingAutomaticPassPresentation@
isSuppressingAutomaticPassPresentation :: IO Bool
isSuppressingAutomaticPassPresentation  =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isSuppressingAutomaticPassPresentation") retCULong []

-- | @+ isPaymentPassActivationAvailable@
pkPassLibraryIsPaymentPassActivationAvailable :: IO Bool
pkPassLibraryIsPaymentPassActivationAvailable  =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPaymentPassActivationAvailable") retCULong []

-- | @- isPaymentPassActivationAvailable@
isPaymentPassActivationAvailable :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
isPaymentPassActivationAvailable pkPassLibrary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "isPaymentPassActivationAvailable") retCULong []

-- | @- passes@
passes :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
passes pkPassLibrary  =
    sendMsg pkPassLibrary (mkSelector "passes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- passWithPassTypeIdentifier:serialNumber:@
passWithPassTypeIdentifier_serialNumber :: (IsPKPassLibrary pkPassLibrary, IsNSString identifier, IsNSString serialNumber) => pkPassLibrary -> identifier -> serialNumber -> IO (Id PKPass)
passWithPassTypeIdentifier_serialNumber pkPassLibrary  identifier serialNumber =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr serialNumber $ \raw_serialNumber ->
        sendMsg pkPassLibrary (mkSelector "passWithPassTypeIdentifier:serialNumber:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_serialNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @- passesWithReaderIdentifier:@
passesWithReaderIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString readerIdentifier) => pkPassLibrary -> readerIdentifier -> IO (Id NSSet)
passesWithReaderIdentifier pkPassLibrary  readerIdentifier =
  withObjCPtr readerIdentifier $ \raw_readerIdentifier ->
      sendMsg pkPassLibrary (mkSelector "passesWithReaderIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_readerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- passesOfType:@
passesOfType :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassType -> IO (Id NSArray)
passesOfType pkPassLibrary  passType =
    sendMsg pkPassLibrary (mkSelector "passesOfType:") (retPtr retVoid) [argCULong (coerce passType)] >>= retainedObject . castPtr

-- | @- remotePaymentPasses@
remotePaymentPasses :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
remotePaymentPasses pkPassLibrary  =
    sendMsg pkPassLibrary (mkSelector "remotePaymentPasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removePass:@
removePass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO ()
removePass pkPassLibrary  pass =
  withObjCPtr pass $ \raw_pass ->
      sendMsg pkPassLibrary (mkSelector "removePass:") retVoid [argPtr (castPtr raw_pass :: Ptr ())]

-- | @- containsPass:@
containsPass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO Bool
containsPass pkPassLibrary  pass =
  withObjCPtr pass $ \raw_pass ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "containsPass:") retCULong [argPtr (castPtr raw_pass :: Ptr ())]

-- | @- replacePassWithPass:@
replacePassWithPass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO Bool
replacePassWithPass pkPassLibrary  pass =
  withObjCPtr pass $ \raw_pass ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "replacePassWithPass:") retCULong [argPtr (castPtr raw_pass :: Ptr ())]

-- | @- addPasses:withCompletionHandler:@
addPasses_withCompletionHandler :: (IsPKPassLibrary pkPassLibrary, IsNSArray passes) => pkPassLibrary -> passes -> Ptr () -> IO ()
addPasses_withCompletionHandler pkPassLibrary  passes completion =
  withObjCPtr passes $ \raw_passes ->
      sendMsg pkPassLibrary (mkSelector "addPasses:withCompletionHandler:") retVoid [argPtr (castPtr raw_passes :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openPaymentSetup@
openPaymentSetup :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO ()
openPaymentSetup pkPassLibrary  =
    sendMsg pkPassLibrary (mkSelector "openPaymentSetup") retVoid []

-- | @- presentPaymentPass:@
presentPaymentPass :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass pass) => pkPassLibrary -> pass -> IO ()
presentPaymentPass pkPassLibrary  pass =
  withObjCPtr pass $ \raw_pass ->
      sendMsg pkPassLibrary (mkSelector "presentPaymentPass:") retVoid [argPtr (castPtr raw_pass :: Ptr ())]

-- | @- presentSecureElementPass:@
presentSecureElementPass :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass pass) => pkPassLibrary -> pass -> IO ()
presentSecureElementPass pkPassLibrary  pass =
  withObjCPtr pass $ \raw_pass ->
      sendMsg pkPassLibrary (mkSelector "presentSecureElementPass:") retVoid [argPtr (castPtr raw_pass :: Ptr ())]

-- | @- canAddPaymentPassWithPrimaryAccountIdentifier:@
canAddPaymentPassWithPrimaryAccountIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString primaryAccountIdentifier) => pkPassLibrary -> primaryAccountIdentifier -> IO Bool
canAddPaymentPassWithPrimaryAccountIdentifier pkPassLibrary  primaryAccountIdentifier =
  withObjCPtr primaryAccountIdentifier $ \raw_primaryAccountIdentifier ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "canAddPaymentPassWithPrimaryAccountIdentifier:") retCULong [argPtr (castPtr raw_primaryAccountIdentifier :: Ptr ())]

-- | @- canAddSecureElementPassWithPrimaryAccountIdentifier:@
canAddSecureElementPassWithPrimaryAccountIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString primaryAccountIdentifier) => pkPassLibrary -> primaryAccountIdentifier -> IO Bool
canAddSecureElementPassWithPrimaryAccountIdentifier pkPassLibrary  primaryAccountIdentifier =
  withObjCPtr primaryAccountIdentifier $ \raw_primaryAccountIdentifier ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "canAddSecureElementPassWithPrimaryAccountIdentifier:") retCULong [argPtr (castPtr raw_primaryAccountIdentifier :: Ptr ())]

-- | @- canAddFelicaPass@
canAddFelicaPass :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
canAddFelicaPass pkPassLibrary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "canAddFelicaPass") retCULong []

-- | @- activatePaymentPass:withActivationData:completion:@
activatePaymentPass_withActivationData_completion :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass paymentPass, IsNSData activationData) => pkPassLibrary -> paymentPass -> activationData -> Ptr () -> IO ()
activatePaymentPass_withActivationData_completion pkPassLibrary  paymentPass activationData completion =
  withObjCPtr paymentPass $ \raw_paymentPass ->
    withObjCPtr activationData $ \raw_activationData ->
        sendMsg pkPassLibrary (mkSelector "activatePaymentPass:withActivationData:completion:") retVoid [argPtr (castPtr raw_paymentPass :: Ptr ()), argPtr (castPtr raw_activationData :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- activatePaymentPass:withActivationCode:completion:@
activatePaymentPass_withActivationCode_completion :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass paymentPass, IsNSString activationCode) => pkPassLibrary -> paymentPass -> activationCode -> Ptr () -> IO ()
activatePaymentPass_withActivationCode_completion pkPassLibrary  paymentPass activationCode completion =
  withObjCPtr paymentPass $ \raw_paymentPass ->
    withObjCPtr activationCode $ \raw_activationCode ->
        sendMsg pkPassLibrary (mkSelector "activatePaymentPass:withActivationCode:completion:") retVoid [argPtr (castPtr raw_paymentPass :: Ptr ()), argPtr (castPtr raw_activationCode :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- activateSecureElementPass:withActivationData:completion:@
activateSecureElementPass_withActivationData_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass, IsNSData activationData) => pkPassLibrary -> secureElementPass -> activationData -> Ptr () -> IO ()
activateSecureElementPass_withActivationData_completion pkPassLibrary  secureElementPass activationData completion =
  withObjCPtr secureElementPass $ \raw_secureElementPass ->
    withObjCPtr activationData $ \raw_activationData ->
        sendMsg pkPassLibrary (mkSelector "activateSecureElementPass:withActivationData:completion:") retVoid [argPtr (castPtr raw_secureElementPass :: Ptr ()), argPtr (castPtr raw_activationData :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- signData:withSecureElementPass:completion:@
signData_withSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsNSData signData, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> signData -> secureElementPass -> Ptr () -> IO ()
signData_withSecureElementPass_completion pkPassLibrary  signData secureElementPass completion =
  withObjCPtr signData $ \raw_signData ->
    withObjCPtr secureElementPass $ \raw_secureElementPass ->
        sendMsg pkPassLibrary (mkSelector "signData:withSecureElementPass:completion:") retVoid [argPtr (castPtr raw_signData :: Ptr ()), argPtr (castPtr raw_secureElementPass :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- encryptedServiceProviderDataForSecureElementPass:completion:@
encryptedServiceProviderDataForSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> secureElementPass -> Ptr () -> IO ()
encryptedServiceProviderDataForSecureElementPass_completion pkPassLibrary  secureElementPass completion =
  withObjCPtr secureElementPass $ \raw_secureElementPass ->
      sendMsg pkPassLibrary (mkSelector "encryptedServiceProviderDataForSecureElementPass:completion:") retVoid [argPtr (castPtr raw_secureElementPass :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- serviceProviderDataForSecureElementPass:completion:@
serviceProviderDataForSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> secureElementPass -> Ptr () -> IO ()
serviceProviderDataForSecureElementPass_completion pkPassLibrary  secureElementPass completion =
  withObjCPtr secureElementPass $ \raw_secureElementPass ->
      sendMsg pkPassLibrary (mkSelector "serviceProviderDataForSecureElementPass:completion:") retVoid [argPtr (castPtr raw_secureElementPass :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- authorizationStatusForCapability:@
authorizationStatusForCapability :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassLibraryCapability -> IO PKPassLibraryAuthorizationStatus
authorizationStatusForCapability pkPassLibrary  capability =
    fmap (coerce :: CLong -> PKPassLibraryAuthorizationStatus) $ sendMsg pkPassLibrary (mkSelector "authorizationStatusForCapability:") retCLong [argCLong (coerce capability)]

-- | @- requestAuthorizationForCapability:completion:@
requestAuthorizationForCapability_completion :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassLibraryCapability -> Ptr () -> IO ()
requestAuthorizationForCapability_completion pkPassLibrary  capability completion =
    sendMsg pkPassLibrary (mkSelector "requestAuthorizationForCapability:completion:") retVoid [argCLong (coerce capability), argPtr (castPtr completion :: Ptr ())]

-- | @- secureElementPassActivationAvailable@
secureElementPassActivationAvailable :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
secureElementPassActivationAvailable pkPassLibrary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPassLibrary (mkSelector "secureElementPassActivationAvailable") retCULong []

-- | @- remoteSecureElementPasses@
remoteSecureElementPasses :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
remoteSecureElementPasses pkPassLibrary  =
    sendMsg pkPassLibrary (mkSelector "remoteSecureElementPasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isPassLibraryAvailable@
isPassLibraryAvailableSelector :: Selector
isPassLibraryAvailableSelector = mkSelector "isPassLibraryAvailable"

-- | @Selector@ for @requestAutomaticPassPresentationSuppressionWithResponseHandler:@
requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector :: Selector
requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector = mkSelector "requestAutomaticPassPresentationSuppressionWithResponseHandler:"

-- | @Selector@ for @endAutomaticPassPresentationSuppressionWithRequestToken:@
endAutomaticPassPresentationSuppressionWithRequestTokenSelector :: Selector
endAutomaticPassPresentationSuppressionWithRequestTokenSelector = mkSelector "endAutomaticPassPresentationSuppressionWithRequestToken:"

-- | @Selector@ for @isSuppressingAutomaticPassPresentation@
isSuppressingAutomaticPassPresentationSelector :: Selector
isSuppressingAutomaticPassPresentationSelector = mkSelector "isSuppressingAutomaticPassPresentation"

-- | @Selector@ for @isPaymentPassActivationAvailable@
isPaymentPassActivationAvailableSelector :: Selector
isPaymentPassActivationAvailableSelector = mkSelector "isPaymentPassActivationAvailable"

-- | @Selector@ for @passes@
passesSelector :: Selector
passesSelector = mkSelector "passes"

-- | @Selector@ for @passWithPassTypeIdentifier:serialNumber:@
passWithPassTypeIdentifier_serialNumberSelector :: Selector
passWithPassTypeIdentifier_serialNumberSelector = mkSelector "passWithPassTypeIdentifier:serialNumber:"

-- | @Selector@ for @passesWithReaderIdentifier:@
passesWithReaderIdentifierSelector :: Selector
passesWithReaderIdentifierSelector = mkSelector "passesWithReaderIdentifier:"

-- | @Selector@ for @passesOfType:@
passesOfTypeSelector :: Selector
passesOfTypeSelector = mkSelector "passesOfType:"

-- | @Selector@ for @remotePaymentPasses@
remotePaymentPassesSelector :: Selector
remotePaymentPassesSelector = mkSelector "remotePaymentPasses"

-- | @Selector@ for @removePass:@
removePassSelector :: Selector
removePassSelector = mkSelector "removePass:"

-- | @Selector@ for @containsPass:@
containsPassSelector :: Selector
containsPassSelector = mkSelector "containsPass:"

-- | @Selector@ for @replacePassWithPass:@
replacePassWithPassSelector :: Selector
replacePassWithPassSelector = mkSelector "replacePassWithPass:"

-- | @Selector@ for @addPasses:withCompletionHandler:@
addPasses_withCompletionHandlerSelector :: Selector
addPasses_withCompletionHandlerSelector = mkSelector "addPasses:withCompletionHandler:"

-- | @Selector@ for @openPaymentSetup@
openPaymentSetupSelector :: Selector
openPaymentSetupSelector = mkSelector "openPaymentSetup"

-- | @Selector@ for @presentPaymentPass:@
presentPaymentPassSelector :: Selector
presentPaymentPassSelector = mkSelector "presentPaymentPass:"

-- | @Selector@ for @presentSecureElementPass:@
presentSecureElementPassSelector :: Selector
presentSecureElementPassSelector = mkSelector "presentSecureElementPass:"

-- | @Selector@ for @canAddPaymentPassWithPrimaryAccountIdentifier:@
canAddPaymentPassWithPrimaryAccountIdentifierSelector :: Selector
canAddPaymentPassWithPrimaryAccountIdentifierSelector = mkSelector "canAddPaymentPassWithPrimaryAccountIdentifier:"

-- | @Selector@ for @canAddSecureElementPassWithPrimaryAccountIdentifier:@
canAddSecureElementPassWithPrimaryAccountIdentifierSelector :: Selector
canAddSecureElementPassWithPrimaryAccountIdentifierSelector = mkSelector "canAddSecureElementPassWithPrimaryAccountIdentifier:"

-- | @Selector@ for @canAddFelicaPass@
canAddFelicaPassSelector :: Selector
canAddFelicaPassSelector = mkSelector "canAddFelicaPass"

-- | @Selector@ for @activatePaymentPass:withActivationData:completion:@
activatePaymentPass_withActivationData_completionSelector :: Selector
activatePaymentPass_withActivationData_completionSelector = mkSelector "activatePaymentPass:withActivationData:completion:"

-- | @Selector@ for @activatePaymentPass:withActivationCode:completion:@
activatePaymentPass_withActivationCode_completionSelector :: Selector
activatePaymentPass_withActivationCode_completionSelector = mkSelector "activatePaymentPass:withActivationCode:completion:"

-- | @Selector@ for @activateSecureElementPass:withActivationData:completion:@
activateSecureElementPass_withActivationData_completionSelector :: Selector
activateSecureElementPass_withActivationData_completionSelector = mkSelector "activateSecureElementPass:withActivationData:completion:"

-- | @Selector@ for @signData:withSecureElementPass:completion:@
signData_withSecureElementPass_completionSelector :: Selector
signData_withSecureElementPass_completionSelector = mkSelector "signData:withSecureElementPass:completion:"

-- | @Selector@ for @encryptedServiceProviderDataForSecureElementPass:completion:@
encryptedServiceProviderDataForSecureElementPass_completionSelector :: Selector
encryptedServiceProviderDataForSecureElementPass_completionSelector = mkSelector "encryptedServiceProviderDataForSecureElementPass:completion:"

-- | @Selector@ for @serviceProviderDataForSecureElementPass:completion:@
serviceProviderDataForSecureElementPass_completionSelector :: Selector
serviceProviderDataForSecureElementPass_completionSelector = mkSelector "serviceProviderDataForSecureElementPass:completion:"

-- | @Selector@ for @authorizationStatusForCapability:@
authorizationStatusForCapabilitySelector :: Selector
authorizationStatusForCapabilitySelector = mkSelector "authorizationStatusForCapability:"

-- | @Selector@ for @requestAuthorizationForCapability:completion:@
requestAuthorizationForCapability_completionSelector :: Selector
requestAuthorizationForCapability_completionSelector = mkSelector "requestAuthorizationForCapability:completion:"

-- | @Selector@ for @secureElementPassActivationAvailable@
secureElementPassActivationAvailableSelector :: Selector
secureElementPassActivationAvailableSelector = mkSelector "secureElementPassActivationAvailable"

-- | @Selector@ for @remoteSecureElementPasses@
remoteSecureElementPassesSelector :: Selector
remoteSecureElementPassesSelector = mkSelector "remoteSecureElementPasses"

