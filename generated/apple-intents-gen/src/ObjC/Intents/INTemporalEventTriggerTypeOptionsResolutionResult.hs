{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemporalEventTriggerTypeOptionsResolutionResult@.
module ObjC.Intents.INTemporalEventTriggerTypeOptionsResolutionResult
  ( INTemporalEventTriggerTypeOptionsResolutionResult
  , IsINTemporalEventTriggerTypeOptionsResolutionResult(..)
  , successWithResolvedTemporalEventTriggerTypeOptions
  , confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm
  , confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector
  , successWithResolvedTemporalEventTriggerTypeOptionsSelector

  -- * Enum types
  , INTemporalEventTriggerTypeOptions(INTemporalEventTriggerTypeOptions)
  , pattern INTemporalEventTriggerTypeOptionNotScheduled
  , pattern INTemporalEventTriggerTypeOptionScheduledNonRecurring
  , pattern INTemporalEventTriggerTypeOptionScheduledRecurring

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

-- | @+ successWithResolvedTemporalEventTriggerTypeOptions:@
successWithResolvedTemporalEventTriggerTypeOptions :: INTemporalEventTriggerTypeOptions -> IO (Id INTemporalEventTriggerTypeOptionsResolutionResult)
successWithResolvedTemporalEventTriggerTypeOptions resolvedTemporalEventTriggerTypeOptions =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerTypeOptionsResolutionResult"
    sendClassMessage cls' successWithResolvedTemporalEventTriggerTypeOptionsSelector resolvedTemporalEventTriggerTypeOptions

-- | @+ confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:@
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm :: INTemporalEventTriggerTypeOptions -> IO (Id INTemporalEventTriggerTypeOptionsResolutionResult)
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm temporalEventTriggerTypeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerTypeOptionsResolutionResult"
    sendClassMessage cls' confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector temporalEventTriggerTypeOptionsToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemporalEventTriggerTypeOptions:@
successWithResolvedTemporalEventTriggerTypeOptionsSelector :: Selector '[INTemporalEventTriggerTypeOptions] (Id INTemporalEventTriggerTypeOptionsResolutionResult)
successWithResolvedTemporalEventTriggerTypeOptionsSelector = mkSelector "successWithResolvedTemporalEventTriggerTypeOptions:"

-- | @Selector@ for @confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:@
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector :: Selector '[INTemporalEventTriggerTypeOptions] (Id INTemporalEventTriggerTypeOptionsResolutionResult)
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:"

