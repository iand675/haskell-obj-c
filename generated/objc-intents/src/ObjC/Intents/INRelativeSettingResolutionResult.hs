{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedRelativeSettingSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithRelativeSettingToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INRelativeSetting(INRelativeSetting)
  , pattern INRelativeSettingUnknown
  , pattern INRelativeSettingLowest
  , pattern INRelativeSettingLower
  , pattern INRelativeSettingHigher
  , pattern INRelativeSettingHighest

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

-- | @+ successWithResolvedRelativeSetting:@
successWithResolvedRelativeSetting :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
successWithResolvedRelativeSetting resolvedRelativeSetting =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedRelativeSetting:") (retPtr retVoid) [argCLong (coerce resolvedRelativeSetting)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithRelativeSettingToConfirm:@
confirmationRequiredWithRelativeSettingToConfirm :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
confirmationRequiredWithRelativeSettingToConfirm relativeSettingToConfirm =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithRelativeSettingToConfirm:") (retPtr retVoid) [argCLong (coerce relativeSettingToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRelativeSetting -> IO (Id INRelativeSettingResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRelativeSettingResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRelativeSetting:@
successWithResolvedRelativeSettingSelector :: Selector
successWithResolvedRelativeSettingSelector = mkSelector "successWithResolvedRelativeSetting:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRelativeSettingToConfirm:@
confirmationRequiredWithRelativeSettingToConfirmSelector :: Selector
confirmationRequiredWithRelativeSettingToConfirmSelector = mkSelector "confirmationRequiredWithRelativeSettingToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

