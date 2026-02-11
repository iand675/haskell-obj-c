{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Request for information from an identity document stored as a Wallet pass.
--
-- Generated bindings for @PKIdentityRequest@.
module ObjC.PassKit.PKIdentityRequest
  ( PKIdentityRequest
  , IsPKIdentityRequest(..)
  , descriptor
  , setDescriptor
  , nonce
  , setNonce
  , merchantIdentifier
  , setMerchantIdentifier
  , usageDescriptionKey
  , setUsageDescriptionKey
  , descriptorSelector
  , setDescriptorSelector
  , nonceSelector
  , setNonceSelector
  , merchantIdentifierSelector
  , setMerchantIdentifierSelector
  , usageDescriptionKeySelector
  , setUsageDescriptionKeySelector


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
import ObjC.Foundation.Internal.Classes

-- | A descriptor describing the identity document to request.
--
-- ObjC selector: @- descriptor@
descriptor :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO RawId
descriptor pkIdentityRequest  =
    fmap (RawId . castPtr) $ sendMsg pkIdentityRequest (mkSelector "descriptor") (retPtr retVoid) []

-- | A descriptor describing the identity document to request.
--
-- ObjC selector: @- setDescriptor:@
setDescriptor :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> RawId -> IO ()
setDescriptor pkIdentityRequest  value =
    sendMsg pkIdentityRequest (mkSelector "setDescriptor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A caller-specified nonce that will be included in the signed response payload. This is treated as opaque by the PKIdentityAuthorizationController, and has a maximum allowed size of 64 bytes.
--
-- ObjC selector: @- nonce@
nonce :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSData)
nonce pkIdentityRequest  =
    sendMsg pkIdentityRequest (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A caller-specified nonce that will be included in the signed response payload. This is treated as opaque by the PKIdentityAuthorizationController, and has a maximum allowed size of 64 bytes.
--
-- ObjC selector: @- setNonce:@
setNonce :: (IsPKIdentityRequest pkIdentityRequest, IsNSData value) => pkIdentityRequest -> value -> IO ()
setNonce pkIdentityRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkIdentityRequest (mkSelector "setNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Identifies the merchant making the request, as previously agreed with Apple. This must match one of the merchant identifiers in the application's entitlement. This property must be set when requestDocument is invoked.
--
-- ObjC selector: @- merchantIdentifier@
merchantIdentifier :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSString)
merchantIdentifier pkIdentityRequest  =
    sendMsg pkIdentityRequest (mkSelector "merchantIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifies the merchant making the request, as previously agreed with Apple. This must match one of the merchant identifiers in the application's entitlement. This property must be set when requestDocument is invoked.
--
-- ObjC selector: @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKIdentityRequest pkIdentityRequest, IsNSString value) => pkIdentityRequest -> value -> IO ()
setMerchantIdentifier pkIdentityRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkIdentityRequest (mkSelector "setMerchantIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A key in the NSIdentityUsageDescriptionDictionary field of the app's Info.plist file.
--
-- The value for this key is an app-provided string that describes the reason for requesting identity information.
--
-- ObjC selector: @- usageDescriptionKey@
usageDescriptionKey :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSString)
usageDescriptionKey pkIdentityRequest  =
    sendMsg pkIdentityRequest (mkSelector "usageDescriptionKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A key in the NSIdentityUsageDescriptionDictionary field of the app's Info.plist file.
--
-- The value for this key is an app-provided string that describes the reason for requesting identity information.
--
-- ObjC selector: @- setUsageDescriptionKey:@
setUsageDescriptionKey :: (IsPKIdentityRequest pkIdentityRequest, IsNSString value) => pkIdentityRequest -> value -> IO ()
setUsageDescriptionKey pkIdentityRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkIdentityRequest (mkSelector "setUsageDescriptionKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @setDescriptor:@
setDescriptorSelector :: Selector
setDescriptorSelector = mkSelector "setDescriptor:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @usageDescriptionKey@
usageDescriptionKeySelector :: Selector
usageDescriptionKeySelector = mkSelector "usageDescriptionKey"

-- | @Selector@ for @setUsageDescriptionKey:@
setUsageDescriptionKeySelector :: Selector
setUsageDescriptionKeySelector = mkSelector "setUsageDescriptionKey:"

