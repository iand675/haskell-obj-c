{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetTaskAttributeTemporalEventTriggerResolutionResult@.
module ObjC.Intents.INSetTaskAttributeTemporalEventTriggerResolutionResult
  ( INSetTaskAttributeTemporalEventTriggerResolutionResult
  , IsINSetTaskAttributeTemporalEventTriggerResolutionResult(..)
  , unsupportedForReason
  , initWithTemporalEventTriggerResolutionResult
  , initWithTemporalEventTriggerResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSetTaskAttributeTemporalEventTriggerUnsupportedReason(INSetTaskAttributeTemporalEventTriggerUnsupportedReason)
  , pattern INSetTaskAttributeTemporalEventTriggerUnsupportedReasonTimeInPast
  , pattern INSetTaskAttributeTemporalEventTriggerUnsupportedReasonInvalidRecurrence

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INSetTaskAttributeTemporalEventTriggerUnsupportedReason -> IO (Id INSetTaskAttributeTemporalEventTriggerResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSetTaskAttributeTemporalEventTriggerResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResult :: (IsINSetTaskAttributeTemporalEventTriggerResolutionResult inSetTaskAttributeTemporalEventTriggerResolutionResult, IsINTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult) => inSetTaskAttributeTemporalEventTriggerResolutionResult -> temporalEventTriggerResolutionResult -> IO (Id INSetTaskAttributeTemporalEventTriggerResolutionResult)
initWithTemporalEventTriggerResolutionResult inSetTaskAttributeTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult =
  sendOwnedMessage inSetTaskAttributeTemporalEventTriggerResolutionResult initWithTemporalEventTriggerResolutionResultSelector (toINTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSetTaskAttributeTemporalEventTriggerUnsupportedReason] (Id INSetTaskAttributeTemporalEventTriggerResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResultSelector :: Selector '[Id INTemporalEventTriggerResolutionResult] (Id INSetTaskAttributeTemporalEventTriggerResolutionResult)
initWithTemporalEventTriggerResolutionResultSelector = mkSelector "initWithTemporalEventTriggerResolutionResult:"

