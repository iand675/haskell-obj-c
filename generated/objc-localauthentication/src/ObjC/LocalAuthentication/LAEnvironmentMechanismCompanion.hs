{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentMechanismCompanion@.
module ObjC.LocalAuthentication.LAEnvironmentMechanismCompanion
  ( LAEnvironmentMechanismCompanion
  , IsLAEnvironmentMechanismCompanion(..)
  , type_
  , stateHash
  , typeSelector
  , stateHashSelector

  -- * Enum types
  , LACompanionType(LACompanionType)
  , pattern LACompanionTypeWatch
  , pattern LACompanionTypeMac
  , pattern LACompanionTypeVision

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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Type of the companion.
--
-- ObjC selector: @- type@
type_ :: IsLAEnvironmentMechanismCompanion laEnvironmentMechanismCompanion => laEnvironmentMechanismCompanion -> IO LACompanionType
type_ laEnvironmentMechanismCompanion  =
  fmap (coerce :: CLong -> LACompanionType) $ sendMsg laEnvironmentMechanismCompanion (mkSelector "type") retCLong []

-- | Hash of the current companion pairing as returned by @LAContext.domainState.companion.stateHash(for:)@
--
-- If no companion are paired for this companion type, @stateHash@ property is @nil.@             If at least one companion is paired for this companion type, @stateHash@ is not @nil@ and             it changes whenever the set of paired companions of this type is changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLAEnvironmentMechanismCompanion laEnvironmentMechanismCompanion => laEnvironmentMechanismCompanion -> IO (Id NSData)
stateHash laEnvironmentMechanismCompanion  =
  sendMsg laEnvironmentMechanismCompanion (mkSelector "stateHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector
stateHashSelector = mkSelector "stateHash"

