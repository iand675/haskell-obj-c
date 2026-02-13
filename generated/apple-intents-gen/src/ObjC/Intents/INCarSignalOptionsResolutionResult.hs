{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCarSignalOptionsResolutionResult@.
module ObjC.Intents.INCarSignalOptionsResolutionResult
  ( INCarSignalOptionsResolutionResult
  , IsINCarSignalOptionsResolutionResult(..)
  , successWithResolvedCarSignalOptions
  , successWithResolvedValue
  , confirmationRequiredWithCarSignalOptionsToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithCarSignalOptionsToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedCarSignalOptionsSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

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

-- | @+ successWithResolvedCarSignalOptions:@
successWithResolvedCarSignalOptions :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
successWithResolvedCarSignalOptions resolvedCarSignalOptions =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedCarSignalOptionsSelector resolvedCarSignalOptions

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithCarSignalOptionsToConfirm:@
confirmationRequiredWithCarSignalOptionsToConfirm :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithCarSignalOptionsToConfirm carSignalOptionsToConfirm =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCarSignalOptionsToConfirmSelector carSignalOptionsToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INCarSignalOptions -> IO (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INCarSignalOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCarSignalOptions:@
successWithResolvedCarSignalOptionsSelector :: Selector '[INCarSignalOptions] (Id INCarSignalOptionsResolutionResult)
successWithResolvedCarSignalOptionsSelector = mkSelector "successWithResolvedCarSignalOptions:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INCarSignalOptions] (Id INCarSignalOptionsResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithCarSignalOptionsToConfirm:@
confirmationRequiredWithCarSignalOptionsToConfirmSelector :: Selector '[INCarSignalOptions] (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithCarSignalOptionsToConfirmSelector = mkSelector "confirmationRequiredWithCarSignalOptionsToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INCarSignalOptions] (Id INCarSignalOptionsResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

