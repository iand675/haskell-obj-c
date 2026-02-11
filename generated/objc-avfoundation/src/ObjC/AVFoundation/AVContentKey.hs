{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVContentKey@.
module ObjC.AVFoundation.AVContentKey
  ( AVContentKey
  , IsAVContentKey(..)
  , revoke
  , contentKeySpecifier
  , externalContentProtectionStatus
  , revokeSelector
  , contentKeySpecifierSelector
  , externalContentProtectionStatusSelector

  -- * Enum types
  , AVExternalContentProtectionStatus(AVExternalContentProtectionStatus)
  , pattern AVExternalContentProtectionStatusPending
  , pattern AVExternalContentProtectionStatusSufficient
  , pattern AVExternalContentProtectionStatusInsufficient

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Revokes the decryption context of the content key, and removes it from its associated AVContentKeySession.
--
-- Once revoked, the AVContentKey is no longer eligible to be used with any media. If the key is required again, or if the key is requested to be loaded by the application, a new AVContentKeyRequest will be dispatched to the delegate. If there is media playback occurring which is dependent on the content key it will fail and may result in an error being generated with the playback halting.
--
-- ObjC selector: @- revoke@
revoke :: IsAVContentKey avContentKey => avContentKey -> IO ()
revoke avContentKey  =
  sendMsg avContentKey (mkSelector "revoke") retVoid []

-- | Specifies the content key.
--
-- ObjC selector: @- contentKeySpecifier@
contentKeySpecifier :: IsAVContentKey avContentKey => avContentKey -> IO (Id AVContentKeySpecifier)
contentKeySpecifier avContentKey  =
  sendMsg avContentKey (mkSelector "contentKeySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The external protection status for the AVContentKey based on all attached displays.
--
-- This property is not key-value observable, instead the contentKeySession:externalProtectionStatusDidChangeForContentKey: delegate method should be used.
--
-- ObjC selector: @- externalContentProtectionStatus@
externalContentProtectionStatus :: IsAVContentKey avContentKey => avContentKey -> IO AVExternalContentProtectionStatus
externalContentProtectionStatus avContentKey  =
  fmap (coerce :: CLong -> AVExternalContentProtectionStatus) $ sendMsg avContentKey (mkSelector "externalContentProtectionStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revoke@
revokeSelector :: Selector
revokeSelector = mkSelector "revoke"

-- | @Selector@ for @contentKeySpecifier@
contentKeySpecifierSelector :: Selector
contentKeySpecifierSelector = mkSelector "contentKeySpecifier"

-- | @Selector@ for @externalContentProtectionStatus@
externalContentProtectionStatusSelector :: Selector
externalContentProtectionStatusSelector = mkSelector "externalContentProtectionStatus"

