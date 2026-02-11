{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INWorkoutLocationTypeResolutionResult@.
module ObjC.Intents.INWorkoutLocationTypeResolutionResult
  ( INWorkoutLocationTypeResolutionResult
  , IsINWorkoutLocationTypeResolutionResult(..)
  , successWithResolvedWorkoutLocationType
  , successWithResolvedValue
  , confirmationRequiredWithWorkoutLocationTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedWorkoutLocationTypeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithWorkoutLocationTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INWorkoutLocationType(INWorkoutLocationType)
  , pattern INWorkoutLocationTypeUnknown
  , pattern INWorkoutLocationTypeOutdoor
  , pattern INWorkoutLocationTypeIndoor

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ successWithResolvedWorkoutLocationType:@
successWithResolvedWorkoutLocationType :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedWorkoutLocationType resolvedWorkoutLocationType =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedWorkoutLocationType:") (retPtr retVoid) [argCLong (coerce resolvedWorkoutLocationType)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithWorkoutLocationTypeToConfirm:@
confirmationRequiredWithWorkoutLocationTypeToConfirm :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithWorkoutLocationTypeToConfirm workoutLocationTypeToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithWorkoutLocationTypeToConfirm:") (retPtr retVoid) [argCLong (coerce workoutLocationTypeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedWorkoutLocationType:@
successWithResolvedWorkoutLocationTypeSelector :: Selector
successWithResolvedWorkoutLocationTypeSelector = mkSelector "successWithResolvedWorkoutLocationType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithWorkoutLocationTypeToConfirm:@
confirmationRequiredWithWorkoutLocationTypeToConfirmSelector :: Selector
confirmationRequiredWithWorkoutLocationTypeToConfirmSelector = mkSelector "confirmationRequiredWithWorkoutLocationTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

