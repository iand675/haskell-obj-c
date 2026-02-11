{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksTaskListResolutionResult@.
module ObjC.Intents.INDeleteTasksTaskListResolutionResult
  ( INDeleteTasksTaskListResolutionResult
  , IsINDeleteTasksTaskListResolutionResult(..)
  , unsupportedForReason
  , initWithTaskListResolutionResult
  , unsupportedForReasonSelector
  , initWithTaskListResolutionResultSelector

  -- * Enum types
  , INDeleteTasksTaskListUnsupportedReason(INDeleteTasksTaskListUnsupportedReason)
  , pattern INDeleteTasksTaskListUnsupportedReasonNoTaskListFound

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
unsupportedForReason :: INDeleteTasksTaskListUnsupportedReason -> IO (Id INDeleteTasksTaskListResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INDeleteTasksTaskListResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithTaskListResolutionResult:@
initWithTaskListResolutionResult :: (IsINDeleteTasksTaskListResolutionResult inDeleteTasksTaskListResolutionResult, IsINTaskListResolutionResult taskListResolutionResult) => inDeleteTasksTaskListResolutionResult -> taskListResolutionResult -> IO (Id INDeleteTasksTaskListResolutionResult)
initWithTaskListResolutionResult inDeleteTasksTaskListResolutionResult  taskListResolutionResult =
withObjCPtr taskListResolutionResult $ \raw_taskListResolutionResult ->
    sendMsg inDeleteTasksTaskListResolutionResult (mkSelector "initWithTaskListResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_taskListResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskListResolutionResult:@
initWithTaskListResolutionResultSelector :: Selector
initWithTaskListResolutionResultSelector = mkSelector "initWithTaskListResolutionResult:"

