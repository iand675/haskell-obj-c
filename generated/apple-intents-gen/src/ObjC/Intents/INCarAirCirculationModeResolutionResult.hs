{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarAirCirculationModeResolutionResult@.
module ObjC.Intents.INCarAirCirculationModeResolutionResult
  ( INCarAirCirculationModeResolutionResult
  , IsINCarAirCirculationModeResolutionResult(..)
  , successWithResolvedCarAirCirculationMode
  , successWithResolvedValue
  , confirmationRequiredWithCarAirCirculationModeToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithCarAirCirculationModeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedCarAirCirculationModeSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INCarAirCirculationMode(INCarAirCirculationMode)
  , pattern INCarAirCirculationModeUnknown
  , pattern INCarAirCirculationModeFreshAir
  , pattern INCarAirCirculationModeRecirculateAir

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

-- | @+ successWithResolvedCarAirCirculationMode:@
successWithResolvedCarAirCirculationMode :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
successWithResolvedCarAirCirculationMode resolvedCarAirCirculationMode =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMessage cls' successWithResolvedCarAirCirculationModeSelector resolvedCarAirCirculationMode

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithCarAirCirculationModeToConfirm:@
confirmationRequiredWithCarAirCirculationModeToConfirm :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithCarAirCirculationModeToConfirm carAirCirculationModeToConfirm =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCarAirCirculationModeToConfirmSelector carAirCirculationModeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarAirCirculationMode:@
successWithResolvedCarAirCirculationModeSelector :: Selector '[INCarAirCirculationMode] (Id INCarAirCirculationModeResolutionResult)
successWithResolvedCarAirCirculationModeSelector = mkSelector "successWithResolvedCarAirCirculationMode:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INCarAirCirculationMode] (Id INCarAirCirculationModeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarAirCirculationModeToConfirm:@
confirmationRequiredWithCarAirCirculationModeToConfirmSelector :: Selector '[INCarAirCirculationMode] (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithCarAirCirculationModeToConfirmSelector = mkSelector "confirmationRequiredWithCarAirCirculationModeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INCarAirCirculationMode] (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

