{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddPaymentPassRequestConfiguration@.
module ObjC.PassKit.PKAddPaymentPassRequestConfiguration
  ( PKAddPaymentPassRequestConfiguration
  , IsPKAddPaymentPassRequestConfiguration(..)
  , initWithEncryptionScheme
  , encryptionScheme
  , style
  , setStyle
  , cardholderName
  , setCardholderName
  , primaryAccountSuffix
  , setPrimaryAccountSuffix
  , cardDetails
  , setCardDetails
  , localizedDescription
  , setLocalizedDescription
  , primaryAccountIdentifier
  , setPrimaryAccountIdentifier
  , paymentNetwork
  , setPaymentNetwork
  , productIdentifiers
  , setProductIdentifiers
  , requiresFelicaSecureElement
  , setRequiresFelicaSecureElement
  , cardDetailsSelector
  , cardholderNameSelector
  , encryptionSchemeSelector
  , initWithEncryptionSchemeSelector
  , localizedDescriptionSelector
  , paymentNetworkSelector
  , primaryAccountIdentifierSelector
  , primaryAccountSuffixSelector
  , productIdentifiersSelector
  , requiresFelicaSecureElementSelector
  , setCardDetailsSelector
  , setCardholderNameSelector
  , setLocalizedDescriptionSelector
  , setPaymentNetworkSelector
  , setPrimaryAccountIdentifierSelector
  , setPrimaryAccountSuffixSelector
  , setProductIdentifiersSelector
  , setRequiresFelicaSecureElementSelector
  , setStyleSelector
  , styleSelector

  -- * Enum types
  , PKAddPaymentPassStyle(PKAddPaymentPassStyle)
  , pattern PKAddPaymentPassStylePayment
  , pattern PKAddPaymentPassStyleAccess

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

-- | @- initWithEncryptionScheme:@
initWithEncryptionScheme :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString encryptionScheme) => pkAddPaymentPassRequestConfiguration -> encryptionScheme -> IO (Id PKAddPaymentPassRequestConfiguration)
initWithEncryptionScheme pkAddPaymentPassRequestConfiguration encryptionScheme =
  sendOwnedMessage pkAddPaymentPassRequestConfiguration initWithEncryptionSchemeSelector (toNSString encryptionScheme)

-- | @- encryptionScheme@
encryptionScheme :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
encryptionScheme pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration encryptionSchemeSelector

-- | @- style@
style :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO PKAddPaymentPassStyle
style pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration styleSelector

-- | @- setStyle:@
setStyle :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> PKAddPaymentPassStyle -> IO ()
setStyle pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setStyleSelector value

-- | @- cardholderName@
cardholderName :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
cardholderName pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration cardholderNameSelector

-- | @- setCardholderName:@
setCardholderName :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setCardholderName pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setCardholderNameSelector (toNSString value)

-- | @- primaryAccountSuffix@
primaryAccountSuffix :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
primaryAccountSuffix pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration primaryAccountSuffixSelector

-- | @- setPrimaryAccountSuffix:@
setPrimaryAccountSuffix :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPrimaryAccountSuffix pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setPrimaryAccountSuffixSelector (toNSString value)

-- | @- cardDetails@
cardDetails :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSArray)
cardDetails pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration cardDetailsSelector

-- | @- setCardDetails:@
setCardDetails :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSArray value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setCardDetails pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setCardDetailsSelector (toNSArray value)

-- | @- localizedDescription@
localizedDescription :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
localizedDescription pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration localizedDescriptionSelector

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setLocalizedDescription pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setLocalizedDescriptionSelector (toNSString value)

-- | @- primaryAccountIdentifier@
primaryAccountIdentifier :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
primaryAccountIdentifier pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration primaryAccountIdentifierSelector

-- | @- setPrimaryAccountIdentifier:@
setPrimaryAccountIdentifier :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPrimaryAccountIdentifier pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setPrimaryAccountIdentifierSelector (toNSString value)

-- | @- paymentNetwork@
paymentNetwork :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
paymentNetwork pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration paymentNetworkSelector

-- | @- setPaymentNetwork:@
setPaymentNetwork :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPaymentNetwork pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setPaymentNetworkSelector (toNSString value)

-- | @- productIdentifiers@
productIdentifiers :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSSet)
productIdentifiers pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration productIdentifiersSelector

-- | @- setProductIdentifiers:@
setProductIdentifiers :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSSet value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setProductIdentifiers pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setProductIdentifiersSelector (toNSSet value)

-- | @- requiresFelicaSecureElement@
requiresFelicaSecureElement :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO Bool
requiresFelicaSecureElement pkAddPaymentPassRequestConfiguration =
  sendMessage pkAddPaymentPassRequestConfiguration requiresFelicaSecureElementSelector

-- | @- setRequiresFelicaSecureElement:@
setRequiresFelicaSecureElement :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> Bool -> IO ()
setRequiresFelicaSecureElement pkAddPaymentPassRequestConfiguration value =
  sendMessage pkAddPaymentPassRequestConfiguration setRequiresFelicaSecureElementSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEncryptionScheme:@
initWithEncryptionSchemeSelector :: Selector '[Id NSString] (Id PKAddPaymentPassRequestConfiguration)
initWithEncryptionSchemeSelector = mkSelector "initWithEncryptionScheme:"

-- | @Selector@ for @encryptionScheme@
encryptionSchemeSelector :: Selector '[] (Id NSString)
encryptionSchemeSelector = mkSelector "encryptionScheme"

-- | @Selector@ for @style@
styleSelector :: Selector '[] PKAddPaymentPassStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[PKAddPaymentPassStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @cardholderName@
cardholderNameSelector :: Selector '[] (Id NSString)
cardholderNameSelector = mkSelector "cardholderName"

-- | @Selector@ for @setCardholderName:@
setCardholderNameSelector :: Selector '[Id NSString] ()
setCardholderNameSelector = mkSelector "setCardholderName:"

-- | @Selector@ for @primaryAccountSuffix@
primaryAccountSuffixSelector :: Selector '[] (Id NSString)
primaryAccountSuffixSelector = mkSelector "primaryAccountSuffix"

-- | @Selector@ for @setPrimaryAccountSuffix:@
setPrimaryAccountSuffixSelector :: Selector '[Id NSString] ()
setPrimaryAccountSuffixSelector = mkSelector "setPrimaryAccountSuffix:"

-- | @Selector@ for @cardDetails@
cardDetailsSelector :: Selector '[] (Id NSArray)
cardDetailsSelector = mkSelector "cardDetails"

-- | @Selector@ for @setCardDetails:@
setCardDetailsSelector :: Selector '[Id NSArray] ()
setCardDetailsSelector = mkSelector "setCardDetails:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @primaryAccountIdentifier@
primaryAccountIdentifierSelector :: Selector '[] (Id NSString)
primaryAccountIdentifierSelector = mkSelector "primaryAccountIdentifier"

-- | @Selector@ for @setPrimaryAccountIdentifier:@
setPrimaryAccountIdentifierSelector :: Selector '[Id NSString] ()
setPrimaryAccountIdentifierSelector = mkSelector "setPrimaryAccountIdentifier:"

-- | @Selector@ for @paymentNetwork@
paymentNetworkSelector :: Selector '[] (Id NSString)
paymentNetworkSelector = mkSelector "paymentNetwork"

-- | @Selector@ for @setPaymentNetwork:@
setPaymentNetworkSelector :: Selector '[Id NSString] ()
setPaymentNetworkSelector = mkSelector "setPaymentNetwork:"

-- | @Selector@ for @productIdentifiers@
productIdentifiersSelector :: Selector '[] (Id NSSet)
productIdentifiersSelector = mkSelector "productIdentifiers"

-- | @Selector@ for @setProductIdentifiers:@
setProductIdentifiersSelector :: Selector '[Id NSSet] ()
setProductIdentifiersSelector = mkSelector "setProductIdentifiers:"

-- | @Selector@ for @requiresFelicaSecureElement@
requiresFelicaSecureElementSelector :: Selector '[] Bool
requiresFelicaSecureElementSelector = mkSelector "requiresFelicaSecureElement"

-- | @Selector@ for @setRequiresFelicaSecureElement:@
setRequiresFelicaSecureElementSelector :: Selector '[Bool] ()
setRequiresFelicaSecureElementSelector = mkSelector "setRequiresFelicaSecureElement:"

