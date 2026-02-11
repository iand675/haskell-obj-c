{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INWorkoutGoalUnitTypeResolutionResult@.
module ObjC.Intents.INWorkoutGoalUnitTypeResolutionResult
  ( INWorkoutGoalUnitTypeResolutionResult
  , IsINWorkoutGoalUnitTypeResolutionResult(..)
  , successWithResolvedWorkoutGoalUnitType
  , successWithResolvedValue
  , confirmationRequiredWithWorkoutGoalUnitTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , successWithResolvedWorkoutGoalUnitTypeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INWorkoutGoalUnitType(INWorkoutGoalUnitType)
  , pattern INWorkoutGoalUnitTypeUnknown
  , pattern INWorkoutGoalUnitTypeInch
  , pattern INWorkoutGoalUnitTypeMeter
  , pattern INWorkoutGoalUnitTypeFoot
  , pattern INWorkoutGoalUnitTypeMile
  , pattern INWorkoutGoalUnitTypeYard
  , pattern INWorkoutGoalUnitTypeSecond
  , pattern INWorkoutGoalUnitTypeMinute
  , pattern INWorkoutGoalUnitTypeHour
  , pattern INWorkoutGoalUnitTypeJoule
  , pattern INWorkoutGoalUnitTypeKiloCalorie

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

-- | @+ successWithResolvedWorkoutGoalUnitType:@
successWithResolvedWorkoutGoalUnitType :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
successWithResolvedWorkoutGoalUnitType resolvedWorkoutGoalUnitType =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedWorkoutGoalUnitType:") (retPtr retVoid) [argCLong (coerce resolvedWorkoutGoalUnitType)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:@
confirmationRequiredWithWorkoutGoalUnitTypeToConfirm :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithWorkoutGoalUnitTypeToConfirm workoutGoalUnitTypeToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:") (retPtr retVoid) [argCLong (coerce workoutGoalUnitTypeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedWorkoutGoalUnitType:@
successWithResolvedWorkoutGoalUnitTypeSelector :: Selector
successWithResolvedWorkoutGoalUnitTypeSelector = mkSelector "successWithResolvedWorkoutGoalUnitType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:@
confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector :: Selector
confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector = mkSelector "confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

