{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTemporalEventTriggerTypeOptionsResolutionResult@.
module ObjC.Intents.INTemporalEventTriggerTypeOptionsResolutionResult
  ( INTemporalEventTriggerTypeOptionsResolutionResult
  , IsINTemporalEventTriggerTypeOptionsResolutionResult(..)
  , successWithResolvedTemporalEventTriggerTypeOptions
  , confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm
  , successWithResolvedTemporalEventTriggerTypeOptionsSelector
  , confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector

  -- * Enum types
  , INTemporalEventTriggerTypeOptions(INTemporalEventTriggerTypeOptions)
  , pattern INTemporalEventTriggerTypeOptionNotScheduled
  , pattern INTemporalEventTriggerTypeOptionScheduledNonRecurring
  , pattern INTemporalEventTriggerTypeOptionScheduledRecurring

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

-- | @+ successWithResolvedTemporalEventTriggerTypeOptions:@
successWithResolvedTemporalEventTriggerTypeOptions :: INTemporalEventTriggerTypeOptions -> IO (Id INTemporalEventTriggerTypeOptionsResolutionResult)
successWithResolvedTemporalEventTriggerTypeOptions resolvedTemporalEventTriggerTypeOptions =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerTypeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedTemporalEventTriggerTypeOptions:") (retPtr retVoid) [argCULong (coerce resolvedTemporalEventTriggerTypeOptions)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:@
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm :: INTemporalEventTriggerTypeOptions -> IO (Id INTemporalEventTriggerTypeOptionsResolutionResult)
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm temporalEventTriggerTypeOptionsToConfirm =
  do
    cls' <- getRequiredClass "INTemporalEventTriggerTypeOptionsResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:") (retPtr retVoid) [argCULong (coerce temporalEventTriggerTypeOptionsToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedTemporalEventTriggerTypeOptions:@
successWithResolvedTemporalEventTriggerTypeOptionsSelector :: Selector
successWithResolvedTemporalEventTriggerTypeOptionsSelector = mkSelector "successWithResolvedTemporalEventTriggerTypeOptions:"

-- | @Selector@ for @confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:@
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector :: Selector
confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirmSelector = mkSelector "confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm:"

