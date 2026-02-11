{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddPaymentPassRequest@.
module ObjC.PassKit.PKAddPaymentPassRequest
  ( PKAddPaymentPassRequest
  , IsPKAddPaymentPassRequest(..)
  , init_
  , encryptedPassData
  , setEncryptedPassData
  , activationData
  , setActivationData
  , ephemeralPublicKey
  , setEphemeralPublicKey
  , wrappedKey
  , setWrappedKey
  , initSelector
  , encryptedPassDataSelector
  , setEncryptedPassDataSelector
  , activationDataSelector
  , setActivationDataSelector
  , ephemeralPublicKeySelector
  , setEphemeralPublicKeySelector
  , wrappedKeySelector
  , setWrappedKeySelector


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

-- | @- init@
init_ :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id PKAddPaymentPassRequest)
init_ pkAddPaymentPassRequest  =
  sendMsg pkAddPaymentPassRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- encryptedPassData@
encryptedPassData :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
encryptedPassData pkAddPaymentPassRequest  =
  sendMsg pkAddPaymentPassRequest (mkSelector "encryptedPassData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncryptedPassData:@
setEncryptedPassData :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setEncryptedPassData pkAddPaymentPassRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequest (mkSelector "setEncryptedPassData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activationData@
activationData :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
activationData pkAddPaymentPassRequest  =
  sendMsg pkAddPaymentPassRequest (mkSelector "activationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActivationData:@
setActivationData :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setActivationData pkAddPaymentPassRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequest (mkSelector "setActivationData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ephemeralPublicKey@
ephemeralPublicKey :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
ephemeralPublicKey pkAddPaymentPassRequest  =
  sendMsg pkAddPaymentPassRequest (mkSelector "ephemeralPublicKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEphemeralPublicKey:@
setEphemeralPublicKey :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setEphemeralPublicKey pkAddPaymentPassRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequest (mkSelector "setEphemeralPublicKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wrappedKey@
wrappedKey :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
wrappedKey pkAddPaymentPassRequest  =
  sendMsg pkAddPaymentPassRequest (mkSelector "wrappedKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWrappedKey:@
setWrappedKey :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setWrappedKey pkAddPaymentPassRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddPaymentPassRequest (mkSelector "setWrappedKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @encryptedPassData@
encryptedPassDataSelector :: Selector
encryptedPassDataSelector = mkSelector "encryptedPassData"

-- | @Selector@ for @setEncryptedPassData:@
setEncryptedPassDataSelector :: Selector
setEncryptedPassDataSelector = mkSelector "setEncryptedPassData:"

-- | @Selector@ for @activationData@
activationDataSelector :: Selector
activationDataSelector = mkSelector "activationData"

-- | @Selector@ for @setActivationData:@
setActivationDataSelector :: Selector
setActivationDataSelector = mkSelector "setActivationData:"

-- | @Selector@ for @ephemeralPublicKey@
ephemeralPublicKeySelector :: Selector
ephemeralPublicKeySelector = mkSelector "ephemeralPublicKey"

-- | @Selector@ for @setEphemeralPublicKey:@
setEphemeralPublicKeySelector :: Selector
setEphemeralPublicKeySelector = mkSelector "setEphemeralPublicKey:"

-- | @Selector@ for @wrappedKey@
wrappedKeySelector :: Selector
wrappedKeySelector = mkSelector "wrappedKey"

-- | @Selector@ for @setWrappedKey:@
setWrappedKeySelector :: Selector
setWrappedKeySelector = mkSelector "setWrappedKey:"

