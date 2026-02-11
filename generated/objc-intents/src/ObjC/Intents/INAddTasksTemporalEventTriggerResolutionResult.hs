{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddTasksTemporalEventTriggerResolutionResult@.
module ObjC.Intents.INAddTasksTemporalEventTriggerResolutionResult
  ( INAddTasksTemporalEventTriggerResolutionResult
  , IsINAddTasksTemporalEventTriggerResolutionResult(..)
  , unsupportedForReason
  , initWithTemporalEventTriggerResolutionResult
  , unsupportedForReasonSelector
  , initWithTemporalEventTriggerResolutionResultSelector

  -- * Enum types
  , INAddTasksTemporalEventTriggerUnsupportedReason(INAddTasksTemporalEventTriggerUnsupportedReason)
  , pattern INAddTasksTemporalEventTriggerUnsupportedReasonTimeInPast
  , pattern INAddTasksTemporalEventTriggerUnsupportedReasonInvalidRecurrence

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INAddTasksTemporalEventTriggerUnsupportedReason -> IO (Id INAddTasksTemporalEventTriggerResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddTasksTemporalEventTriggerResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResult :: (IsINAddTasksTemporalEventTriggerResolutionResult inAddTasksTemporalEventTriggerResolutionResult, IsINTemporalEventTriggerResolutionResult temporalEventTriggerResolutionResult) => inAddTasksTemporalEventTriggerResolutionResult -> temporalEventTriggerResolutionResult -> IO (Id INAddTasksTemporalEventTriggerResolutionResult)
initWithTemporalEventTriggerResolutionResult inAddTasksTemporalEventTriggerResolutionResult  temporalEventTriggerResolutionResult =
withObjCPtr temporalEventTriggerResolutionResult $ \raw_temporalEventTriggerResolutionResult ->
    sendMsg inAddTasksTemporalEventTriggerResolutionResult (mkSelector "initWithTemporalEventTriggerResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_temporalEventTriggerResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTemporalEventTriggerResolutionResult:@
initWithTemporalEventTriggerResolutionResultSelector :: Selector
initWithTemporalEventTriggerResolutionResultSelector = mkSelector "initWithTemporalEventTriggerResolutionResult:"

