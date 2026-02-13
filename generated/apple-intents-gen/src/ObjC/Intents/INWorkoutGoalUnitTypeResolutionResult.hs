{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithValueToConfirmSelector
  , confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector
  , successWithResolvedValueSelector
  , successWithResolvedWorkoutGoalUnitTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' successWithResolvedWorkoutGoalUnitTypeSelector resolvedWorkoutGoalUnitType

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:@
confirmationRequiredWithWorkoutGoalUnitTypeToConfirm :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithWorkoutGoalUnitTypeToConfirm workoutGoalUnitTypeToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector workoutGoalUnitTypeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INWorkoutGoalUnitType -> IO (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutGoalUnitTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedWorkoutGoalUnitType:@
successWithResolvedWorkoutGoalUnitTypeSelector :: Selector '[INWorkoutGoalUnitType] (Id INWorkoutGoalUnitTypeResolutionResult)
successWithResolvedWorkoutGoalUnitTypeSelector = mkSelector "successWithResolvedWorkoutGoalUnitType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INWorkoutGoalUnitType] (Id INWorkoutGoalUnitTypeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:@
confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector :: Selector '[INWorkoutGoalUnitType] (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithWorkoutGoalUnitTypeToConfirmSelector = mkSelector "confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INWorkoutGoalUnitType] (Id INWorkoutGoalUnitTypeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

