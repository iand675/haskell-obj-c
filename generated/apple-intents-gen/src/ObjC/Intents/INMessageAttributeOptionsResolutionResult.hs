{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessageAttributeOptionsResolutionResult@.
module ObjC.Intents.INMessageAttributeOptionsResolutionResult
  ( INMessageAttributeOptionsResolutionResult
  , IsINMessageAttributeOptionsResolutionResult(..)
  , successWithResolvedMessageAttributeOptions
  , successWithResolvedValue
  , confirmationRequiredWithMessageAttributeOptionsToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithMessageAttributeOptionsToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedMessageAttributeOptionsSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INMessageAttributeOptions(INMessageAttributeOptions)
  , pattern INMessageAttributeOptionRead
  , pattern INMessageAttributeOptionUnread
  , pattern INMessageAttributeOptionFlagged
  , pattern INMessageAttributeOptionUnflagged
  , pattern INMessageAttributeOptionPlayed

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

-- | @+ successWithResolvedMessageAttributeOptions:@
successWithResolvedMessageAttributeOptions :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedMessageAttributeOptions resolvedMessageAttributeOptions =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedMessageAttributeOptionsSelector resolvedMessageAttributeOptions

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithMessageAttributeOptionsToConfirm:@
confirmationRequiredWithMessageAttributeOptionsToConfirm :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithMessageAttributeOptionsToConfirm messageAttributeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithMessageAttributeOptionsToConfirmSelector messageAttributeOptionsToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INMessageAttributeOptions -> IO (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INMessageAttributeOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMessageAttributeOptions:@
successWithResolvedMessageAttributeOptionsSelector :: Selector '[INMessageAttributeOptions] (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedMessageAttributeOptionsSelector = mkSelector "successWithResolvedMessageAttributeOptions:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INMessageAttributeOptions] (Id INMessageAttributeOptionsResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithMessageAttributeOptionsToConfirm:@
confirmationRequiredWithMessageAttributeOptionsToConfirmSelector :: Selector '[INMessageAttributeOptions] (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithMessageAttributeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithMessageAttributeOptionsToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INMessageAttributeOptions] (Id INMessageAttributeOptionsResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

