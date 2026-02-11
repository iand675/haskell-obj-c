{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedCarAirCirculationModeSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithCarAirCirculationModeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INCarAirCirculationMode(INCarAirCirculationMode)
  , pattern INCarAirCirculationModeUnknown
  , pattern INCarAirCirculationModeFreshAir
  , pattern INCarAirCirculationModeRecirculateAir

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

-- | @+ successWithResolvedCarAirCirculationMode:@
successWithResolvedCarAirCirculationMode :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
successWithResolvedCarAirCirculationMode resolvedCarAirCirculationMode =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCarAirCirculationMode:") (retPtr retVoid) [argCLong (coerce resolvedCarAirCirculationMode)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCarAirCirculationModeToConfirm:@
confirmationRequiredWithCarAirCirculationModeToConfirm :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithCarAirCirculationModeToConfirm carAirCirculationModeToConfirm =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCarAirCirculationModeToConfirm:") (retPtr retVoid) [argCLong (coerce carAirCirculationModeToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarAirCirculationMode -> IO (Id INCarAirCirculationModeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarAirCirculationModeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarAirCirculationMode:@
successWithResolvedCarAirCirculationModeSelector :: Selector
successWithResolvedCarAirCirculationModeSelector = mkSelector "successWithResolvedCarAirCirculationMode:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarAirCirculationModeToConfirm:@
confirmationRequiredWithCarAirCirculationModeToConfirmSelector :: Selector
confirmationRequiredWithCarAirCirculationModeToConfirmSelector = mkSelector "confirmationRequiredWithCarAirCirculationModeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

