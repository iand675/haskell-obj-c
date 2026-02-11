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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requested output size of key exchange result.  Should be ignored if output size is not configurable for specified key exchange algorithm.
--
-- ObjC selector: @- requestedSize@
requestedSize :: IsTKTokenKeyExchangeParameters tkTokenKeyExchangeParameters => tkTokenKeyExchangeParameters -> IO CLong
requestedSize tkTokenKeyExchangeParameters  =
  sendMsg tkTokenKeyExchangeParameters (mkSelector "requestedSize") retCLong []

-- | Additional shared information input, typically used for key derivation (KDF) step of key exchange algorithm.  Should be ignored if shared info is not used for specified key exchange algorithm.
--
-- ObjC selector: @- sharedInfo@
sharedInfo :: IsTKTokenKeyExchangeParameters tkTokenKeyExchangeParameters => tkTokenKeyExchangeParameters -> IO (Id NSData)
sharedInfo tkTokenKeyExchangeParameters  =
  sendMsg tkTokenKeyExchangeParameters (mkSelector "sharedInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestedSize@
requestedSizeSelector :: Selector
requestedSizeSelector = mkSelector "requestedSize"

-- | @Selector@ for @sharedInfo@
sharedInfoSelector :: Selector
sharedInfoSelector = mkSelector "sharedInfo"

