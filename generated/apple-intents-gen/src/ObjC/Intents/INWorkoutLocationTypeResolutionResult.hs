{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , confirmationRequiredWithValueToConfirmSelector
  , confirmationRequiredWithWorkoutLocationTypeToConfirmSelector
  , successWithResolvedValueSelector
  , successWithResolvedWorkoutLocationTypeSelector

  -- * Enum types
  , INWorkoutLocationType(INWorkoutLocationType)
  , pattern INWorkoutLocationTypeUnknown
  , pattern INWorkoutLocationTypeOutdoor
  , pattern INWorkoutLocationTypeIndoor

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

-- | @+ successWithResolvedWorkoutLocationType:@
successWithResolvedWorkoutLocationType :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedWorkoutLocationType resolvedWorkoutLocationType =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMessage cls' successWithResolvedWorkoutLocationTypeSelector resolvedWorkoutLocationType

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithWorkoutLocationTypeToConfirm:@
confirmationRequiredWithWorkoutLocationTypeToConfirm :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithWorkoutLocationTypeToConfirm workoutLocationTypeToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithWorkoutLocationTypeToConfirmSelector workoutLocationTypeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INWorkoutLocationType -> IO (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INWorkoutLocationTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedWorkoutLocationType:@
successWithResolvedWorkoutLocationTypeSelector :: Selector '[INWorkoutLocationType] (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedWorkoutLocationTypeSelector = mkSelector "successWithResolvedWorkoutLocationType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INWorkoutLocationType] (Id INWorkoutLocationTypeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithWorkoutLocationTypeToConfirm:@
confirmationRequiredWithWorkoutLocationTypeToConfirmSelector :: Selector '[INWorkoutLocationType] (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithWorkoutLocationTypeToConfirmSelector = mkSelector "confirmationRequiredWithWorkoutLocationTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INWorkoutLocationType] (Id INWorkoutLocationTypeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

