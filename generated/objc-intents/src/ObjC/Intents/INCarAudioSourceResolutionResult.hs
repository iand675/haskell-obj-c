{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedCarAudioSourceSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithCarAudioSourceToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

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

-- | @+ successWithResolvedCarAudioSource:@
successWithResolvedCarAudioSource :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
successWithResolvedCarAudioSource resolvedCarAudioSource =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCarAudioSource:") (retPtr retVoid) [argCLong (coerce resolvedCarAudioSource)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCarAudioSourceToConfirm:@
confirmationRequiredWithCarAudioSourceToConfirm :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithCarAudioSourceToConfirm carAudioSourceToConfirm =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCarAudioSourceToConfirm:") (retPtr retVoid) [argCLong (coerce carAudioSourceToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarAudioSource -> IO (Id INCarAudioSourceResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarAudioSourceResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarAudioSource:@
successWithResolvedCarAudioSourceSelector :: Selector
successWithResolvedCarAudioSourceSelector = mkSelector "successWithResolvedCarAudioSource:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarAudioSourceToConfirm:@
confirmationRequiredWithCarAudioSourceToConfirmSelector :: Selector
confirmationRequiredWithCarAudioSourceToConfirmSelector = mkSelector "confirmationRequiredWithCarAudioSourceToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

