{-# LANGUAGE DataKinds #-}
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
  , merchantIdentifierSelector
  , nonceSelector
  , setDescriptorSelector
  , setMerchantIdentifierSelector
  , setNonceSelector
  , setUsageDescriptionKeySelector
  , usageDescriptionKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A descriptor describing the identity document to request.
--
-- ObjC selector: @- descriptor@
descriptor :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO RawId
descriptor pkIdentityRequest =
  sendMessage pkIdentityRequest descriptorSelector

-- | A descriptor describing the identity document to request.
--
-- ObjC selector: @- setDescriptor:@
setDescriptor :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> RawId -> IO ()
setDescriptor pkIdentityRequest value =
  sendMessage pkIdentityRequest setDescriptorSelector value

-- | A caller-specified nonce that will be included in the signed response payload. This is treated as opaque by the PKIdentityAuthorizationController, and has a maximum allowed size of 64 bytes.
--
-- ObjC selector: @- nonce@
nonce :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSData)
nonce pkIdentityRequest =
  sendMessage pkIdentityRequest nonceSelector

-- | A caller-specified nonce that will be included in the signed response payload. This is treated as opaque by the PKIdentityAuthorizationController, and has a maximum allowed size of 64 bytes.
--
-- ObjC selector: @- setNonce:@
setNonce :: (IsPKIdentityRequest pkIdentityRequest, IsNSData value) => pkIdentityRequest -> value -> IO ()
setNonce pkIdentityRequest value =
  sendMessage pkIdentityRequest setNonceSelector (toNSData value)

-- | Identifies the merchant making the request, as previously agreed with Apple. This must match one of the merchant identifiers in the application's entitlement. This property must be set when requestDocument is invoked.
--
-- ObjC selector: @- merchantIdentifier@
merchantIdentifier :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSString)
merchantIdentifier pkIdentityRequest =
  sendMessage pkIdentityRequest merchantIdentifierSelector

-- | Identifies the merchant making the request, as previously agreed with Apple. This must match one of the merchant identifiers in the application's entitlement. This property must be set when requestDocument is invoked.
--
-- ObjC selector: @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKIdentityRequest pkIdentityRequest, IsNSString value) => pkIdentityRequest -> value -> IO ()
setMerchantIdentifier pkIdentityRequest value =
  sendMessage pkIdentityRequest setMerchantIdentifierSelector (toNSString value)

-- | A key in the NSIdentityUsageDescriptionDictionary field of the app's Info.plist file.
--
-- The value for this key is an app-provided string that describes the reason for requesting identity information.
--
-- ObjC selector: @- usageDescriptionKey@
usageDescriptionKey :: IsPKIdentityRequest pkIdentityRequest => pkIdentityRequest -> IO (Id NSString)
usageDescriptionKey pkIdentityRequest =
  sendMessage pkIdentityRequest usageDescriptionKeySelector

-- | A key in the NSIdentityUsageDescriptionDictionary field of the app's Info.plist file.
--
-- The value for this key is an app-provided string that describes the reason for requesting identity information.
--
-- ObjC selector: @- setUsageDescriptionKey:@
setUsageDescriptionKey :: (IsPKIdentityRequest pkIdentityRequest, IsNSString value) => pkIdentityRequest -> value -> IO ()
setUsageDescriptionKey pkIdentityRequest value =
  sendMessage pkIdentityRequest setUsageDescriptionKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] RawId
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @setDescriptor:@
setDescriptorSelector :: Selector '[RawId] ()
setDescriptorSelector = mkSelector "setDescriptor:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSData)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector '[Id NSData] ()
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector '[] (Id NSString)
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector '[Id NSString] ()
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @usageDescriptionKey@
usageDescriptionKeySelector :: Selector '[] (Id NSString)
usageDescriptionKeySelector = mkSelector "usageDescriptionKey"

-- | @Selector@ for @setUsageDescriptionKey:@
setUsageDescriptionKeySelector :: Selector '[Id NSString] ()
setUsageDescriptionKeySelector = mkSelector "setUsageDescriptionKey:"

