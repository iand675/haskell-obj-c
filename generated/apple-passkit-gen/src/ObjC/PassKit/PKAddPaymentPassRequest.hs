{-# LANGUAGE DataKinds #-}
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
  , activationDataSelector
  , encryptedPassDataSelector
  , ephemeralPublicKeySelector
  , initSelector
  , setActivationDataSelector
  , setEncryptedPassDataSelector
  , setEphemeralPublicKeySelector
  , setWrappedKeySelector
  , wrappedKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id PKAddPaymentPassRequest)
init_ pkAddPaymentPassRequest =
  sendOwnedMessage pkAddPaymentPassRequest initSelector

-- | @- encryptedPassData@
encryptedPassData :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
encryptedPassData pkAddPaymentPassRequest =
  sendMessage pkAddPaymentPassRequest encryptedPassDataSelector

-- | @- setEncryptedPassData:@
setEncryptedPassData :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setEncryptedPassData pkAddPaymentPassRequest value =
  sendMessage pkAddPaymentPassRequest setEncryptedPassDataSelector (toNSData value)

-- | @- activationData@
activationData :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
activationData pkAddPaymentPassRequest =
  sendMessage pkAddPaymentPassRequest activationDataSelector

-- | @- setActivationData:@
setActivationData :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setActivationData pkAddPaymentPassRequest value =
  sendMessage pkAddPaymentPassRequest setActivationDataSelector (toNSData value)

-- | @- ephemeralPublicKey@
ephemeralPublicKey :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
ephemeralPublicKey pkAddPaymentPassRequest =
  sendMessage pkAddPaymentPassRequest ephemeralPublicKeySelector

-- | @- setEphemeralPublicKey:@
setEphemeralPublicKey :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setEphemeralPublicKey pkAddPaymentPassRequest value =
  sendMessage pkAddPaymentPassRequest setEphemeralPublicKeySelector (toNSData value)

-- | @- wrappedKey@
wrappedKey :: IsPKAddPaymentPassRequest pkAddPaymentPassRequest => pkAddPaymentPassRequest -> IO (Id NSData)
wrappedKey pkAddPaymentPassRequest =
  sendMessage pkAddPaymentPassRequest wrappedKeySelector

-- | @- setWrappedKey:@
setWrappedKey :: (IsPKAddPaymentPassRequest pkAddPaymentPassRequest, IsNSData value) => pkAddPaymentPassRequest -> value -> IO ()
setWrappedKey pkAddPaymentPassRequest value =
  sendMessage pkAddPaymentPassRequest setWrappedKeySelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKAddPaymentPassRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @encryptedPassData@
encryptedPassDataSelector :: Selector '[] (Id NSData)
encryptedPassDataSelector = mkSelector "encryptedPassData"

-- | @Selector@ for @setEncryptedPassData:@
setEncryptedPassDataSelector :: Selector '[Id NSData] ()
setEncryptedPassDataSelector = mkSelector "setEncryptedPassData:"

-- | @Selector@ for @activationData@
activationDataSelector :: Selector '[] (Id NSData)
activationDataSelector = mkSelector "activationData"

-- | @Selector@ for @setActivationData:@
setActivationDataSelector :: Selector '[Id NSData] ()
setActivationDataSelector = mkSelector "setActivationData:"

-- | @Selector@ for @ephemeralPublicKey@
ephemeralPublicKeySelector :: Selector '[] (Id NSData)
ephemeralPublicKeySelector = mkSelector "ephemeralPublicKey"

-- | @Selector@ for @setEphemeralPublicKey:@
setEphemeralPublicKeySelector :: Selector '[Id NSData] ()
setEphemeralPublicKeySelector = mkSelector "setEphemeralPublicKey:"

-- | @Selector@ for @wrappedKey@
wrappedKeySelector :: Selector '[] (Id NSData)
wrappedKeySelector = mkSelector "wrappedKey"

-- | @Selector@ for @setWrappedKey:@
setWrappedKeySelector :: Selector '[Id NSData] ()
setWrappedKeySelector = mkSelector "setWrappedKey:"

