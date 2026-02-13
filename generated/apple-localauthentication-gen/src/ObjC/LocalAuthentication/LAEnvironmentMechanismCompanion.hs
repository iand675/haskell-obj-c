{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentMechanismCompanion@.
module ObjC.LocalAuthentication.LAEnvironmentMechanismCompanion
  ( LAEnvironmentMechanismCompanion
  , IsLAEnvironmentMechanismCompanion(..)
  , type_
  , stateHash
  , stateHashSelector
  , typeSelector

  -- * Enum types
  , LACompanionType(LACompanionType)
  , pattern LACompanionTypeWatch
  , pattern LACompanionTypeMac
  , pattern LACompanionTypeVision

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Type of the companion.
--
-- ObjC selector: @- type@
type_ :: IsLAEnvironmentMechanismCompanion laEnvironmentMechanismCompanion => laEnvironmentMechanismCompanion -> IO LACompanionType
type_ laEnvironmentMechanismCompanion =
  sendMessage laEnvironmentMechanismCompanion typeSelector

-- | Hash of the current companion pairing as returned by @LAContext.domainState.companion.stateHash(for:)@
--
-- If no companion are paired for this companion type, @stateHash@ property is @nil.@             If at least one companion is paired for this companion type, @stateHash@ is not @nil@ and             it changes whenever the set of paired companions of this type is changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLAEnvironmentMechanismCompanion laEnvironmentMechanismCompanion => laEnvironmentMechanismCompanion -> IO (Id NSData)
stateHash laEnvironmentMechanismCompanion =
  sendMessage laEnvironmentMechanismCompanion stateHashSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] LACompanionType
typeSelector = mkSelector "type"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector '[] (Id NSData)
stateHashSelector = mkSelector "stateHash"

