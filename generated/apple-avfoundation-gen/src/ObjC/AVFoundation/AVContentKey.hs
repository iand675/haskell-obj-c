{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , contentKeySpecifierSelector
  , externalContentProtectionStatusSelector
  , revokeSelector

  -- * Enum types
  , AVExternalContentProtectionStatus(AVExternalContentProtectionStatus)
  , pattern AVExternalContentProtectionStatusPending
  , pattern AVExternalContentProtectionStatusSufficient
  , pattern AVExternalContentProtectionStatusInsufficient

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
revoke avContentKey =
  sendMessage avContentKey revokeSelector

-- | Specifies the content key.
--
-- ObjC selector: @- contentKeySpecifier@
contentKeySpecifier :: IsAVContentKey avContentKey => avContentKey -> IO (Id AVContentKeySpecifier)
contentKeySpecifier avContentKey =
  sendMessage avContentKey contentKeySpecifierSelector

-- | The external protection status for the AVContentKey based on all attached displays.
--
-- This property is not key-value observable, instead the contentKeySession:externalProtectionStatusDidChangeForContentKey: delegate method should be used.
--
-- ObjC selector: @- externalContentProtectionStatus@
externalContentProtectionStatus :: IsAVContentKey avContentKey => avContentKey -> IO AVExternalContentProtectionStatus
externalContentProtectionStatus avContentKey =
  sendMessage avContentKey externalContentProtectionStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revoke@
revokeSelector :: Selector '[] ()
revokeSelector = mkSelector "revoke"

-- | @Selector@ for @contentKeySpecifier@
contentKeySpecifierSelector :: Selector '[] (Id AVContentKeySpecifier)
contentKeySpecifierSelector = mkSelector "contentKeySpecifier"

-- | @Selector@ for @externalContentProtectionStatus@
externalContentProtectionStatusSelector :: Selector '[] AVExternalContentProtectionStatus
externalContentProtectionStatusSelector = mkSelector "externalContentProtectionStatus"

