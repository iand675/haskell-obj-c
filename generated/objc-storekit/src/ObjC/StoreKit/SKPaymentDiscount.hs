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
  , initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector
  , identifierSelector
  , keyIdentifierSelector
  , nonceSelector
  , signatureSelector
  , timestampSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifier:keyIdentifier:nonce:signature:timestamp:@
initWithIdentifier_keyIdentifier_nonce_signature_timestamp :: (IsSKPaymentDiscount skPaymentDiscount, IsNSString identifier, IsNSString keyIdentifier, IsNSUUID nonce, IsNSString signature, IsNSNumber timestamp) => skPaymentDiscount -> identifier -> keyIdentifier -> nonce -> signature -> timestamp -> IO (Id SKPaymentDiscount)
initWithIdentifier_keyIdentifier_nonce_signature_timestamp skPaymentDiscount  identifier keyIdentifier nonce signature timestamp =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr keyIdentifier $ \raw_keyIdentifier ->
    withObjCPtr nonce $ \raw_nonce ->
      withObjCPtr signature $ \raw_signature ->
        withObjCPtr timestamp $ \raw_timestamp ->
            sendMsg skPaymentDiscount (mkSelector "initWithIdentifier:keyIdentifier:nonce:signature:timestamp:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_keyIdentifier :: Ptr ()), argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr raw_timestamp :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
identifier skPaymentDiscount  =
  sendMsg skPaymentDiscount (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- keyIdentifier@
keyIdentifier :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
keyIdentifier skPaymentDiscount  =
  sendMsg skPaymentDiscount (mkSelector "keyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nonce@
nonce :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSUUID)
nonce skPaymentDiscount  =
  sendMsg skPaymentDiscount (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- signature@
signature :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSString)
signature skPaymentDiscount  =
  sendMsg skPaymentDiscount (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timestamp@
timestamp :: IsSKPaymentDiscount skPaymentDiscount => skPaymentDiscount -> IO (Id NSNumber)
timestamp skPaymentDiscount  =
  sendMsg skPaymentDiscount (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:keyIdentifier:nonce:signature:timestamp:@
initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector :: Selector
initWithIdentifier_keyIdentifier_nonce_signature_timestampSelector = mkSelector "initWithIdentifier:keyIdentifier:nonce:signature:timestamp:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

