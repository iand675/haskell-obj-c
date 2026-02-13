{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenKeyExchangeParameters Encapsulates parameters needed for performing specific Key Exchange operation types.
--
-- Generated bindings for @TKTokenKeyExchangeParameters@.
module ObjC.CryptoTokenKit.TKTokenKeyExchangeParameters
  ( TKTokenKeyExchangeParameters
  , IsTKTokenKeyExchangeParameters(..)
  , requestedSize
  , sharedInfo
  , requestedSizeSelector
  , sharedInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requested output size of key exchange result.  Should be ignored if output size is not configurable for specified key exchange algorithm.
--
-- ObjC selector: @- requestedSize@
requestedSize :: IsTKTokenKeyExchangeParameters tkTokenKeyExchangeParameters => tkTokenKeyExchangeParameters -> IO CLong
requestedSize tkTokenKeyExchangeParameters =
  sendMessage tkTokenKeyExchangeParameters requestedSizeSelector

-- | Additional shared information input, typically used for key derivation (KDF) step of key exchange algorithm.  Should be ignored if shared info is not used for specified key exchange algorithm.
--
-- ObjC selector: @- sharedInfo@
sharedInfo :: IsTKTokenKeyExchangeParameters tkTokenKeyExchangeParameters => tkTokenKeyExchangeParameters -> IO (Id NSData)
sharedInfo tkTokenKeyExchangeParameters =
  sendMessage tkTokenKeyExchangeParameters sharedInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestedSize@
requestedSizeSelector :: Selector '[] CLong
requestedSizeSelector = mkSelector "requestedSize"

-- | @Selector@ for @sharedInfo@
sharedInfoSelector :: Selector '[] (Id NSData)
sharedInfoSelector = mkSelector "sharedInfo"

