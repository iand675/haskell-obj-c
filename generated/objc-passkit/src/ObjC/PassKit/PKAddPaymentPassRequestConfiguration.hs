{-# LANGUAGE PatternSynonyms #-}
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
  , initWithEncryptionSchemeSelector
  , encryptionSchemeSelector
  , styleSelector
  , setStyleSelector
  , cardholderNameSelector
  , setCardholderNameSelector
  , primaryAccountSuffixSelector
  , setPrimaryAccountSuffixSelector
  , cardDetailsSelector
  , setCardDetailsSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , primaryAccountIdentifierSelector
  , setPrimaryAccountIdentifierSelector
  , paymentNetworkSelector
  , setPaymentNetworkSelector
  , productIdentifiersSelector
  , setProductIdentifiersSelector
  , requiresFelicaSecureElementSelector
  , setRequiresFelicaSecureElementSelector

  -- * Enum types
  , PKAddPaymentPassStyle(PKAddPaymentPassStyle)
  , pattern PKAddPaymentPassStylePayment
  , pattern PKAddPaymentPassStyleAccess

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

-- | @- initWithEncryptionScheme:@
initWithEncryptionScheme :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString encryptionScheme) => pkAddPaymentPassRequestConfiguration -> encryptionScheme -> IO (Id PKAddPaymentPassRequestConfiguration)
initWithEncryptionScheme pkAddPaymentPassRequestConfiguration  encryptionScheme =
withObjCPtr encryptionScheme $ \raw_encryptionScheme ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "initWithEncryptionScheme:") (retPtr retVoid) [argPtr (castPtr raw_encryptionScheme :: Ptr ())] >>= ownedObject . castPtr

-- | @- encryptionScheme@
encryptionScheme :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
encryptionScheme pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "encryptionScheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- style@
style :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO PKAddPaymentPassStyle
style pkAddPaymentPassRequestConfiguration  =
  fmap (coerce :: CLong -> PKAddPaymentPassStyle) $ sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "style") retCLong []

-- | @- setStyle:@
setStyle :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> PKAddPaymentPassStyle -> IO ()
setStyle pkAddPaymentPassRequestConfiguration  value =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- | @- cardholderName@
cardholderName :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
cardholderName pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "cardholderName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCardholderName:@
setCardholderName :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setCardholderName pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setCardholderName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- primaryAccountSuffix@
primaryAccountSuffix :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
primaryAccountSuffix pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "primaryAccountSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryAccountSuffix:@
setPrimaryAccountSuffix :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPrimaryAccountSuffix pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setPrimaryAccountSuffix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cardDetails@
cardDetails :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSArray)
cardDetails pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "cardDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCardDetails:@
setCardDetails :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSArray value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setCardDetails pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setCardDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedDescription@
localizedDescription :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
localizedDescription pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setLocalizedDescription pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- primaryAccountIdentifier@
primaryAccountIdentifier :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
primaryAccountIdentifier pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "primaryAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryAccountIdentifier:@
setPrimaryAccountIdentifier :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPrimaryAccountIdentifier pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setPrimaryAccountIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paymentNetwork@
paymentNetwork :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSString)
paymentNetwork pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "paymentNetwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentNetwork:@
setPaymentNetwork :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSString value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setPaymentNetwork pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setPaymentNetwork:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productIdentifiers@
productIdentifiers :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO (Id NSSet)
productIdentifiers pkAddPaymentPassRequestConfiguration  =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "productIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifiers:@
setProductIdentifiers :: (IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration, IsNSSet value) => pkAddPaymentPassRequestConfiguration -> value -> IO ()
setProductIdentifiers pkAddPaymentPassRequestConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setProductIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiresFelicaSecureElement@
requiresFelicaSecureElement :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> IO Bool
requiresFelicaSecureElement pkAddPaymentPassRequestConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "requiresFelicaSecureElement") retCULong []

-- | @- setRequiresFelicaSecureElement:@
setRequiresFelicaSecureElement :: IsPKAddPaymentPassRequestConfiguration pkAddPaymentPassRequestConfiguration => pkAddPaymentPassRequestConfiguration -> Bool -> IO ()
setRequiresFelicaSecureElement pkAddPaymentPassRequestConfiguration  value =
  sendMsg pkAddPaymentPassRequestConfiguration (mkSelector "setRequiresFelicaSecureElement:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEncryptionScheme:@
initWithEncryptionSchemeSelector :: Selector
initWithEncryptionSchemeSelector = mkSelector "initWithEncryptionScheme:"

-- | @Selector@ for @encryptionScheme@
encryptionSchemeSelector :: Selector
encryptionSchemeSelector = mkSelector "encryptionScheme"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @cardholderName@
cardholderNameSelector :: Selector
cardholderNameSelector = mkSelector "cardholderName"

-- | @Selector@ for @setCardholderName:@
setCardholderNameSelector :: Selector
setCardholderNameSelector = mkSelector "setCardholderName:"

-- | @Selector@ for @primaryAccountSuffix@
primaryAccountSuffixSelector :: Selector
primaryAccountSuffixSelector = mkSelector "primaryAccountSuffix"

-- | @Selector@ for @setPrimaryAccountSuffix:@
setPrimaryAccountSuffixSelector :: Selector
setPrimaryAccountSuffixSelector = mkSelector "setPrimaryAccountSuffix:"

-- | @Selector@ for @cardDetails@
cardDetailsSelector :: Selector
cardDetailsSelector = mkSelector "cardDetails"

-- | @Selector@ for @setCardDetails:@
setCardDetailsSelector :: Selector
setCardDetailsSelector = mkSelector "setCardDetails:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @primaryAccountIdentifier@
primaryAccountIdentifierSelector :: Selector
primaryAccountIdentifierSelector = mkSelector "primaryAccountIdentifier"

-- | @Selector@ for @setPrimaryAccountIdentifier:@
setPrimaryAccountIdentifierSelector :: Selector
setPrimaryAccountIdentifierSelector = mkSelector "setPrimaryAccountIdentifier:"

-- | @Selector@ for @paymentNetwork@
paymentNetworkSelector :: Selector
paymentNetworkSelector = mkSelector "paymentNetwork"

-- | @Selector@ for @setPaymentNetwork:@
setPaymentNetworkSelector :: Selector
setPaymentNetworkSelector = mkSelector "setPaymentNetwork:"

-- | @Selector@ for @productIdentifiers@
productIdentifiersSelector :: Selector
productIdentifiersSelector = mkSelector "productIdentifiers"

-- | @Selector@ for @setProductIdentifiers:@
setProductIdentifiersSelector :: Selector
setProductIdentifiersSelector = mkSelector "setProductIdentifiers:"

-- | @Selector@ for @requiresFelicaSecureElement@
requiresFelicaSecureElementSelector :: Selector
requiresFelicaSecureElementSelector = mkSelector "requiresFelicaSecureElement"

-- | @Selector@ for @setRequiresFelicaSecureElement:@
setRequiresFelicaSecureElementSelector :: Selector
setRequiresFelicaSecureElementSelector = mkSelector "setRequiresFelicaSecureElement:"

