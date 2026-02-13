{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPaymentDiscount@.
module ObjC.StoreKit.SKPaymentDiscount
  ( SKPaymentDiscount
  , IsSKPaymentDiscount(..)
  , initWithIdentifier_keyIdentifier_nonce_signature_timestamp
  , identifier
  , keyIdentifier
  , nonce
  , signature
  , timestamp
  , identifierSelector
  , initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector
  , keyIdentifierSelector
  , nonceSelector
  , signatureSelector
  , timestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifier:keyIdentifier:nonce:signature:timestamp:@
initWithIdentifier_keyIdentifier_nonce_signature_timestamp :: (IsSKPaymentDiscount skPaymentDiscount, IsNSString identifier, IsNSString keyIdentifier, IsNSUUID nonce, IsNSString signature, IsNSNumber timestamp) => skPaymentDiscount -> identifier -> keyIdentifier -> nonce -> signature -> timestamp -> IO (Id SKPaymentDiscount)
initWithIdentifier_keyIdentifier_nonce_signature_timestamp skPaymentDiscount identifier keyIdentifier nonce signature timestamp =
  sendOwnedMessage skPaymentDiscount initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector (toNSString identifier) (toNSString keyIdentifier) (toNSUUID nonce) (toNSString signature) (toNSNumber timestamp)

-- | @- identifier@
identifier :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
identifier skPaymentDiscount =
  sendMessage skPaymentDiscount identifierSelector

-- | @- keyIdentifier@
keyIdentifier :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
keyIdentifier skPaymentDiscount =
  sendMessage skPaymentDiscount keyIdentifierSelector

-- | @- nonce@
nonce :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSUUID)
nonce skPaymentDiscount =
  sendMessage skPaymentDiscount nonceSelector

-- | @- signature@
signature :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
signature skPaymentDiscount =
  sendMessage skPaymentDiscount signatureSelector

-- | @- timestamp@
timestamp :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSNumber)
timestamp skPaymentDiscount =
  sendMessage skPaymentDiscount timestampSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:keyIdentifier:nonce:signature:timestamp:@
initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector :: Selector '[Id NSString, Id NSString, Id NSUUID, Id NSString, Id NSNumber] (Id SKPaymentDiscount)
initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector = mkSelector "initWithIdentifier:keyIdentifier:nonce:signature:timestamp:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector '[] (Id NSString)
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSUUID)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @signature@
signatureSelector :: Selector '[] (Id NSString)
signatureSelector = mkSelector "signature"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSNumber)
timestampSelector = mkSelector "timestamp"

