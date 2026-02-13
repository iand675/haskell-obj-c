{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarAudioSourceResolutionResult@.
module ObjC.Intents.INCarAudioSourceResolutionResult
  ( INCarAudioSourceResolutionResult
  , IsINCarAudioSourceResolutionResult(..)
  , successWithResolvedCarAudioSource
  , successWithResolvedValue
  , confirmationRequiredWithCarAudioSourceToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithCarAudioSourceToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedCarAudioSourceSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INCarAudioSource(INCarAudioSource)
  , pattern INCarAudioSourceUnknown
  , pattern INCarAudioSourceCarPlay
  , pattern INCarAudioSourceiPod
  , pattern INCarAudioSourceRadio
  , pattern INCarAudioSourceBluetooth
  , pattern INCarAudioSourceAUX
  , pattern INCarAudioSourceUSB
  , pattern INCarAudioSourceMemoryCard
  , pattern INCarAudioSourceOpticalDrive
  , pattern INCarAudioSourceHardDrive

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

-- | @+ successWithResolvedCarAudioSource:@
successWithResolvedCarAudioSource :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
successWithResolvedCarAudioSource resolvedCarAudioSource =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMessage cls' successWithResolvedCarAudioSourceSelector resolvedCarAudioSource

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithCarAudioSourceToConfirm:@
confirmationRequiredWithCarAudioSourceToConfirm :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithCarAudioSourceToConfirm carAudioSourceToConfirm =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCarAudioSourceToConfirmSelector carAudioSourceToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarAudioSource:@
successWithResolvedCarAudioSourceSelector :: Selector '[INCarAudioSource] (Id INCarAudioSourceResolutionResult)
successWithResolvedCarAudioSourceSelector = mkSelector "successWithResolvedCarAudioSource:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INCarAudioSource] (Id INCarAudioSourceResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarAudioSourceToConfirm:@
confirmationRequiredWithCarAudioSourceToConfirmSelector :: Selector '[INCarAudioSource] (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithCarAudioSourceToConfirmSelector = mkSelector "confirmationRequiredWithCarAudioSourceToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INCarAudioSource] (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

