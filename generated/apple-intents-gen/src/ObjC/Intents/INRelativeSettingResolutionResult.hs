{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRelativeSettingResolutionResult@.
module ObjC.Intents.INRelativeSettingResolutionResult
  ( INRelativeSettingResolutionResult
  , IsINRelativeSettingResolutionResult(..)
  , successWithResolvedRelativeSetting
  , successWithResolvedValue
  , confirmationRequiredWithRelativeSettingToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithRelativeSettingToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedRelativeSettingSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INRelativeSetting(INRelativeSetting)
  , pattern INRelativeSettingUnknown
  , pattern INRelativeSettingLowest
  , pattern INRelativeSettingLower
  , pattern INRelativeSettingHigher
  , pattern INRelativeSettingHighest

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

-- | @+ successWithResolvedRelativeSetting:@
successWithResolvedRelativeSetting :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
successWithResolvedRelativeSetting resolvedRelativeSetting =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMessage cls' successWithResolvedRelativeSettingSelector resolvedRelativeSetting

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithRelativeSettingToConfirm:@
confirmationRequiredWithRelativeSettingToConfirm :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
confirmationRequiredWithRelativeSettingToConfirm relativeSettingToConfirm =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMessage cls' confirmationRequiredWithRelativeSettingToConfirmSelector relativeSettingToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRelativeSetting:@
successWithResolvedRelativeSettingSelector :: Selector '[INRelativeSetting] (Id INRelativeSettingResolutionResult)
successWithResolvedRelativeSettingSelector = mkSelector "successWithResolvedRelativeSetting:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INRelativeSetting] (Id INRelativeSettingResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRelativeSettingToConfirm:@
confirmationRequiredWithRelativeSettingToConfirmSelector :: Selector '[INRelativeSetting] (Id INRelativeSettingResolutionResult)
confirmationRequiredWithRelativeSettingToConfirmSelector = mkSelector "confirmationRequiredWithRelativeSettingToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INRelativeSetting] (Id INRelativeSettingResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

