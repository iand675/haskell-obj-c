{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activatePaymentPass_withActivationCode_completionSelector
  , activatePaymentPass_withActivationData_completionSelector
  , activateSecureElementPass_withActivationData_completionSelector
  , addPasses_withCompletionHandlerSelector
  , authorizationStatusForCapabilitySelector
  , canAddFelicaPassSelector
  , canAddPaymentPassWithPrimaryAccountIdentifierSelector
  , canAddSecureElementPassWithPrimaryAccountIdentifierSelector
  , containsPassSelector
  , encryptedServiceProviderDataForSecureElementPass_completionSelector
  , endAutomaticPassPresentationSuppressionWithRequestTokenSelector
  , isPassLibraryAvailableSelector
  , isPaymentPassActivationAvailableSelector
  , isSuppressingAutomaticPassPresentationSelector
  , openPaymentSetupSelector
  , passWithPassTypeIdentifier_serialNumberSelector
  , passesOfTypeSelector
  , passesSelector
  , passesWithReaderIdentifierSelector
  , pkPassLibraryIsPaymentPassActivationAvailableSelector
  , presentPaymentPassSelector
  , presentSecureElementPassSelector
  , remotePaymentPassesSelector
  , remoteSecureElementPassesSelector
  , removePassSelector
  , replacePassWithPassSelector
  , requestAuthorizationForCapability_completionSelector
  , requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector
  , secureElementPassActivationAvailableSelector
  , serviceProviderDataForSecureElementPass_completionSelector
  , signData_withSecureElementPass_completionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' isPassLibraryAvailableSelector

-- | @+ requestAutomaticPassPresentationSuppressionWithResponseHandler:@
requestAutomaticPassPresentationSuppressionWithResponseHandler :: Ptr () -> IO CULong
requestAutomaticPassPresentationSuppressionWithResponseHandler responseHandler =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMessage cls' requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector responseHandler

-- | @+ endAutomaticPassPresentationSuppressionWithRequestToken:@
endAutomaticPassPresentationSuppressionWithRequestToken :: CULong -> IO ()
endAutomaticPassPresentationSuppressionWithRequestToken requestToken =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMessage cls' endAutomaticPassPresentationSuppressionWithRequestTokenSelector requestToken

-- | @+ isSuppressingAutomaticPassPresentation@
isSuppressingAutomaticPassPresentation :: IO Bool
isSuppressingAutomaticPassPresentation  =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMessage cls' isSuppressingAutomaticPassPresentationSelector

-- | @+ isPaymentPassActivationAvailable@
pkPassLibraryIsPaymentPassActivationAvailable :: IO Bool
pkPassLibraryIsPaymentPassActivationAvailable  =
  do
    cls' <- getRequiredClass "PKPassLibrary"
    sendClassMessage cls' pkPassLibraryIsPaymentPassActivationAvailableSelector

-- | @- isPaymentPassActivationAvailable@
isPaymentPassActivationAvailable :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
isPaymentPassActivationAvailable pkPassLibrary =
  sendMessage pkPassLibrary isPaymentPassActivationAvailableSelector

-- | @- passes@
passes :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
passes pkPassLibrary =
  sendMessage pkPassLibrary passesSelector

-- | @- passWithPassTypeIdentifier:serialNumber:@
passWithPassTypeIdentifier_serialNumber :: (IsPKPassLibrary pkPassLibrary, IsNSString identifier, IsNSString serialNumber) => pkPassLibrary -> identifier -> serialNumber -> IO (Id PKPass)
passWithPassTypeIdentifier_serialNumber pkPassLibrary identifier serialNumber =
  sendMessage pkPassLibrary passWithPassTypeIdentifier_serialNumberSelector (toNSString identifier) (toNSString serialNumber)

-- | @- passesWithReaderIdentifier:@
passesWithReaderIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString readerIdentifier) => pkPassLibrary -> readerIdentifier -> IO (Id NSSet)
passesWithReaderIdentifier pkPassLibrary readerIdentifier =
  sendMessage pkPassLibrary passesWithReaderIdentifierSelector (toNSString readerIdentifier)

-- | @- passesOfType:@
passesOfType :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassType -> IO (Id NSArray)
passesOfType pkPassLibrary passType =
  sendMessage pkPassLibrary passesOfTypeSelector passType

-- | @- remotePaymentPasses@
remotePaymentPasses :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
remotePaymentPasses pkPassLibrary =
  sendMessage pkPassLibrary remotePaymentPassesSelector

-- | @- removePass:@
removePass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO ()
removePass pkPassLibrary pass =
  sendMessage pkPassLibrary removePassSelector (toPKPass pass)

-- | @- containsPass:@
containsPass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO Bool
containsPass pkPassLibrary pass =
  sendMessage pkPassLibrary containsPassSelector (toPKPass pass)

-- | @- replacePassWithPass:@
replacePassWithPass :: (IsPKPassLibrary pkPassLibrary, IsPKPass pass) => pkPassLibrary -> pass -> IO Bool
replacePassWithPass pkPassLibrary pass =
  sendMessage pkPassLibrary replacePassWithPassSelector (toPKPass pass)

-- | @- addPasses:withCompletionHandler:@
addPasses_withCompletionHandler :: (IsPKPassLibrary pkPassLibrary, IsNSArray passes) => pkPassLibrary -> passes -> Ptr () -> IO ()
addPasses_withCompletionHandler pkPassLibrary passes completion =
  sendMessage pkPassLibrary addPasses_withCompletionHandlerSelector (toNSArray passes) completion

-- | @- openPaymentSetup@
openPaymentSetup :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO ()
openPaymentSetup pkPassLibrary =
  sendMessage pkPassLibrary openPaymentSetupSelector

-- | @- presentPaymentPass:@
presentPaymentPass :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass pass) => pkPassLibrary -> pass -> IO ()
presentPaymentPass pkPassLibrary pass =
  sendMessage pkPassLibrary presentPaymentPassSelector (toPKPaymentPass pass)

-- | @- presentSecureElementPass:@
presentSecureElementPass :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass pass) => pkPassLibrary -> pass -> IO ()
presentSecureElementPass pkPassLibrary pass =
  sendMessage pkPassLibrary presentSecureElementPassSelector (toPKSecureElementPass pass)

-- | @- canAddPaymentPassWithPrimaryAccountIdentifier:@
canAddPaymentPassWithPrimaryAccountIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString primaryAccountIdentifier) => pkPassLibrary -> primaryAccountIdentifier -> IO Bool
canAddPaymentPassWithPrimaryAccountIdentifier pkPassLibrary primaryAccountIdentifier =
  sendMessage pkPassLibrary canAddPaymentPassWithPrimaryAccountIdentifierSelector (toNSString primaryAccountIdentifier)

-- | @- canAddSecureElementPassWithPrimaryAccountIdentifier:@
canAddSecureElementPassWithPrimaryAccountIdentifier :: (IsPKPassLibrary pkPassLibrary, IsNSString primaryAccountIdentifier) => pkPassLibrary -> primaryAccountIdentifier -> IO Bool
canAddSecureElementPassWithPrimaryAccountIdentifier pkPassLibrary primaryAccountIdentifier =
  sendMessage pkPassLibrary canAddSecureElementPassWithPrimaryAccountIdentifierSelector (toNSString primaryAccountIdentifier)

-- | @- canAddFelicaPass@
canAddFelicaPass :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
canAddFelicaPass pkPassLibrary =
  sendMessage pkPassLibrary canAddFelicaPassSelector

-- | @- activatePaymentPass:withActivationData:completion:@
activatePaymentPass_withActivationData_completion :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass paymentPass, IsNSData activationData) => pkPassLibrary -> paymentPass -> activationData -> Ptr () -> IO ()
activatePaymentPass_withActivationData_completion pkPassLibrary paymentPass activationData completion =
  sendMessage pkPassLibrary activatePaymentPass_withActivationData_completionSelector (toPKPaymentPass paymentPass) (toNSData activationData) completion

-- | @- activatePaymentPass:withActivationCode:completion:@
activatePaymentPass_withActivationCode_completion :: (IsPKPassLibrary pkPassLibrary, IsPKPaymentPass paymentPass, IsNSString activationCode) => pkPassLibrary -> paymentPass -> activationCode -> Ptr () -> IO ()
activatePaymentPass_withActivationCode_completion pkPassLibrary paymentPass activationCode completion =
  sendMessage pkPassLibrary activatePaymentPass_withActivationCode_completionSelector (toPKPaymentPass paymentPass) (toNSString activationCode) completion

-- | @- activateSecureElementPass:withActivationData:completion:@
activateSecureElementPass_withActivationData_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass, IsNSData activationData) => pkPassLibrary -> secureElementPass -> activationData -> Ptr () -> IO ()
activateSecureElementPass_withActivationData_completion pkPassLibrary secureElementPass activationData completion =
  sendMessage pkPassLibrary activateSecureElementPass_withActivationData_completionSelector (toPKSecureElementPass secureElementPass) (toNSData activationData) completion

-- | @- signData:withSecureElementPass:completion:@
signData_withSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsNSData signData, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> signData -> secureElementPass -> Ptr () -> IO ()
signData_withSecureElementPass_completion pkPassLibrary signData secureElementPass completion =
  sendMessage pkPassLibrary signData_withSecureElementPass_completionSelector (toNSData signData) (toPKSecureElementPass secureElementPass) completion

-- | @- encryptedServiceProviderDataForSecureElementPass:completion:@
encryptedServiceProviderDataForSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> secureElementPass -> Ptr () -> IO ()
encryptedServiceProviderDataForSecureElementPass_completion pkPassLibrary secureElementPass completion =
  sendMessage pkPassLibrary encryptedServiceProviderDataForSecureElementPass_completionSelector (toPKSecureElementPass secureElementPass) completion

-- | @- serviceProviderDataForSecureElementPass:completion:@
serviceProviderDataForSecureElementPass_completion :: (IsPKPassLibrary pkPassLibrary, IsPKSecureElementPass secureElementPass) => pkPassLibrary -> secureElementPass -> Ptr () -> IO ()
serviceProviderDataForSecureElementPass_completion pkPassLibrary secureElementPass completion =
  sendMessage pkPassLibrary serviceProviderDataForSecureElementPass_completionSelector (toPKSecureElementPass secureElementPass) completion

-- | @- authorizationStatusForCapability:@
authorizationStatusForCapability :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassLibraryCapability -> IO PKPassLibraryAuthorizationStatus
authorizationStatusForCapability pkPassLibrary capability =
  sendMessage pkPassLibrary authorizationStatusForCapabilitySelector capability

-- | @- requestAuthorizationForCapability:completion:@
requestAuthorizationForCapability_completion :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> PKPassLibraryCapability -> Ptr () -> IO ()
requestAuthorizationForCapability_completion pkPassLibrary capability completion =
  sendMessage pkPassLibrary requestAuthorizationForCapability_completionSelector capability completion

-- | @- secureElementPassActivationAvailable@
secureElementPassActivationAvailable :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO Bool
secureElementPassActivationAvailable pkPassLibrary =
  sendMessage pkPassLibrary secureElementPassActivationAvailableSelector

-- | @- remoteSecureElementPasses@
remoteSecureElementPasses :: IsPKPassLibrary pkPassLibrary => pkPassLibrary -> IO (Id NSArray)
remoteSecureElementPasses pkPassLibrary =
  sendMessage pkPassLibrary remoteSecureElementPassesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isPassLibraryAvailable@
isPassLibraryAvailableSelector :: Selector '[] Bool
isPassLibraryAvailableSelector = mkSelector "isPassLibraryAvailable"

-- | @Selector@ for @requestAutomaticPassPresentationSuppressionWithResponseHandler:@
requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector :: Selector '[Ptr ()] CULong
requestAutomaticPassPresentationSuppressionWithResponseHandlerSelector = mkSelector "requestAutomaticPassPresentationSuppressionWithResponseHandler:"

-- | @Selector@ for @endAutomaticPassPresentationSuppressionWithRequestToken:@
endAutomaticPassPresentationSuppressionWithRequestTokenSelector :: Selector '[CULong] ()
endAutomaticPassPresentationSuppressionWithRequestTokenSelector = mkSelector "endAutomaticPassPresentationSuppressionWithRequestToken:"

-- | @Selector@ for @isSuppressingAutomaticPassPresentation@
isSuppressingAutomaticPassPresentationSelector :: Selector '[] Bool
isSuppressingAutomaticPassPresentationSelector = mkSelector "isSuppressingAutomaticPassPresentation"

-- | @Selector@ for @isPaymentPassActivationAvailable@
pkPassLibraryIsPaymentPassActivationAvailableSelector :: Selector '[] Bool
pkPassLibraryIsPaymentPassActivationAvailableSelector = mkSelector "isPaymentPassActivationAvailable"

-- | @Selector@ for @isPaymentPassActivationAvailable@
isPaymentPassActivationAvailableSelector :: Selector '[] Bool
isPaymentPassActivationAvailableSelector = mkSelector "isPaymentPassActivationAvailable"

-- | @Selector@ for @passes@
passesSelector :: Selector '[] (Id NSArray)
passesSelector = mkSelector "passes"

-- | @Selector@ for @passWithPassTypeIdentifier:serialNumber:@
passWithPassTypeIdentifier_serialNumberSelector :: Selector '[Id NSString, Id NSString] (Id PKPass)
passWithPassTypeIdentifier_serialNumberSelector = mkSelector "passWithPassTypeIdentifier:serialNumber:"

-- | @Selector@ for @passesWithReaderIdentifier:@
passesWithReaderIdentifierSelector :: Selector '[Id NSString] (Id NSSet)
passesWithReaderIdentifierSelector = mkSelector "passesWithReaderIdentifier:"

-- | @Selector@ for @passesOfType:@
passesOfTypeSelector :: Selector '[PKPassType] (Id NSArray)
passesOfTypeSelector = mkSelector "passesOfType:"

-- | @Selector@ for @remotePaymentPasses@
remotePaymentPassesSelector :: Selector '[] (Id NSArray)
remotePaymentPassesSelector = mkSelector "remotePaymentPasses"

-- | @Selector@ for @removePass:@
removePassSelector :: Selector '[Id PKPass] ()
removePassSelector = mkSelector "removePass:"

-- | @Selector@ for @containsPass:@
containsPassSelector :: Selector '[Id PKPass] Bool
containsPassSelector = mkSelector "containsPass:"

-- | @Selector@ for @replacePassWithPass:@
replacePassWithPassSelector :: Selector '[Id PKPass] Bool
replacePassWithPassSelector = mkSelector "replacePassWithPass:"

-- | @Selector@ for @addPasses:withCompletionHandler:@
addPasses_withCompletionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
addPasses_withCompletionHandlerSelector = mkSelector "addPasses:withCompletionHandler:"

-- | @Selector@ for @openPaymentSetup@
openPaymentSetupSelector :: Selector '[] ()
openPaymentSetupSelector = mkSelector "openPaymentSetup"

-- | @Selector@ for @presentPaymentPass:@
presentPaymentPassSelector :: Selector '[Id PKPaymentPass] ()
presentPaymentPassSelector = mkSelector "presentPaymentPass:"

-- | @Selector@ for @presentSecureElementPass:@
presentSecureElementPassSelector :: Selector '[Id PKSecureElementPass] ()
presentSecureElementPassSelector = mkSelector "presentSecureElementPass:"

-- | @Selector@ for @canAddPaymentPassWithPrimaryAccountIdentifier:@
canAddPaymentPassWithPrimaryAccountIdentifierSelector :: Selector '[Id NSString] Bool
canAddPaymentPassWithPrimaryAccountIdentifierSelector = mkSelector "canAddPaymentPassWithPrimaryAccountIdentifier:"

-- | @Selector@ for @canAddSecureElementPassWithPrimaryAccountIdentifier:@
canAddSecureElementPassWithPrimaryAccountIdentifierSelector :: Selector '[Id NSString] Bool
canAddSecureElementPassWithPrimaryAccountIdentifierSelector = mkSelector "canAddSecureElementPassWithPrimaryAccountIdentifier:"

-- | @Selector@ for @canAddFelicaPass@
canAddFelicaPassSelector :: Selector '[] Bool
canAddFelicaPassSelector = mkSelector "canAddFelicaPass"

-- | @Selector@ for @activatePaymentPass:withActivationData:completion:@
activatePaymentPass_withActivationData_completionSelector :: Selector '[Id PKPaymentPass, Id NSData, Ptr ()] ()
activatePaymentPass_withActivationData_completionSelector = mkSelector "activatePaymentPass:withActivationData:completion:"

-- | @Selector@ for @activatePaymentPass:withActivationCode:completion:@
activatePaymentPass_withActivationCode_completionSelector :: Selector '[Id PKPaymentPass, Id NSString, Ptr ()] ()
activatePaymentPass_withActivationCode_completionSelector = mkSelector "activatePaymentPass:withActivationCode:completion:"

-- | @Selector@ for @activateSecureElementPass:withActivationData:completion:@
activateSecureElementPass_withActivationData_completionSelector :: Selector '[Id PKSecureElementPass, Id NSData, Ptr ()] ()
activateSecureElementPass_withActivationData_completionSelector = mkSelector "activateSecureElementPass:withActivationData:completion:"

-- | @Selector@ for @signData:withSecureElementPass:completion:@
signData_withSecureElementPass_completionSelector :: Selector '[Id NSData, Id PKSecureElementPass, Ptr ()] ()
signData_withSecureElementPass_completionSelector = mkSelector "signData:withSecureElementPass:completion:"

-- | @Selector@ for @encryptedServiceProviderDataForSecureElementPass:completion:@
encryptedServiceProviderDataForSecureElementPass_completionSelector :: Selector '[Id PKSecureElementPass, Ptr ()] ()
encryptedServiceProviderDataForSecureElementPass_completionSelector = mkSelector "encryptedServiceProviderDataForSecureElementPass:completion:"

-- | @Selector@ for @serviceProviderDataForSecureElementPass:completion:@
serviceProviderDataForSecureElementPass_completionSelector :: Selector '[Id PKSecureElementPass, Ptr ()] ()
serviceProviderDataForSecureElementPass_completionSelector = mkSelector "serviceProviderDataForSecureElementPass:completion:"

-- | @Selector@ for @authorizationStatusForCapability:@
authorizationStatusForCapabilitySelector :: Selector '[PKPassLibraryCapability] PKPassLibraryAuthorizationStatus
authorizationStatusForCapabilitySelector = mkSelector "authorizationStatusForCapability:"

-- | @Selector@ for @requestAuthorizationForCapability:completion:@
requestAuthorizationForCapability_completionSelector :: Selector '[PKPassLibraryCapability, Ptr ()] ()
requestAuthorizationForCapability_completionSelector = mkSelector "requestAuthorizationForCapability:completion:"

-- | @Selector@ for @secureElementPassActivationAvailable@
secureElementPassActivationAvailableSelector :: Selector '[] Bool
secureElementPassActivationAvailableSelector = mkSelector "secureElementPassActivationAvailable"

-- | @Selector@ for @remoteSecureElementPasses@
remoteSecureElementPassesSelector :: Selector '[] (Id NSArray)
remoteSecureElementPassesSelector = mkSelector "remoteSecureElementPasses"

