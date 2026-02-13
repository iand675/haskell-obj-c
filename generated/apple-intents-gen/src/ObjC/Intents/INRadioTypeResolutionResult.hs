{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRadioTypeResolutionResult@.
module ObjC.Intents.INRadioTypeResolutionResult
  ( INRadioTypeResolutionResult
  , IsINRadioTypeResolutionResult(..)
  , successWithResolvedRadioType
  , successWithResolvedValue
  , confirmationRequiredWithRadioTypeToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithRadioTypeToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedRadioTypeSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INRadioType(INRadioType)
  , pattern INRadioTypeUnknown
  , pattern INRadioTypeAM
  , pattern INRadioTypeFM
  , pattern INRadioTypeHD
  , pattern INRadioTypeSatellite
  , pattern INRadioTypeDAB

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

-- | @+ successWithResolvedRadioType:@
successWithResolvedRadioType :: INRadioType -> IO (Id INRadioTypeResolutionResult)
successWithResolvedRadioType resolvedRadioType =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMessage cls' successWithResolvedRadioTypeSelector resolvedRadioType

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRadioType -> IO (Id INRadioTypeResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithRadioTypeToConfirm:@
confirmationRequiredWithRadioTypeToConfirm :: INRadioType -> IO (Id INRadioTypeResolutionResult)
confirmationRequiredWithRadioTypeToConfirm radioTypeToConfirm =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithRadioTypeToConfirmSelector radioTypeToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRadioType -> IO (Id INRadioTypeResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRadioTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRadioType:@
successWithResolvedRadioTypeSelector :: Selector '[INRadioType] (Id INRadioTypeResolutionResult)
successWithResolvedRadioTypeSelector = mkSelector "successWithResolvedRadioType:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INRadioType] (Id INRadioTypeResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRadioTypeToConfirm:@
confirmationRequiredWithRadioTypeToConfirmSelector :: Selector '[INRadioType] (Id INRadioTypeResolutionResult)
confirmationRequiredWithRadioTypeToConfirmSelector = mkSelector "confirmationRequiredWithRadioTypeToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INRadioType] (Id INRadioTypeResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

