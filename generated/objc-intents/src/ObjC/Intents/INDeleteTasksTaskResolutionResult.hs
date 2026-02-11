{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksTaskResolutionResult@.
module ObjC.Intents.INDeleteTasksTaskResolutionResult
  ( INDeleteTasksTaskResolutionResult
  , IsINDeleteTasksTaskResolutionResult(..)
  , unsupportedForReason
  , initWithTaskResolutionResult
  , unsupportedForReasonSelector
  , initWithTaskResolutionResultSelector

  -- * Enum types
  , INDeleteTasksTaskUnsupportedReason(INDeleteTasksTaskUnsupportedReason)
  , pattern INDeleteTasksTaskUnsupportedReasonNoTasksFound
  , pattern INDeleteTasksTaskUnsupportedReasonNoTasksInApp

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
unsupportedForReason :: INDeleteTasksTaskUnsupportedReason -> IO (Id INDeleteTasksTaskResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INDeleteTasksTaskResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithTaskResolutionResult:@
initWithTaskResolutionResult :: (IsINDeleteTasksTaskResolutionResult inDeleteTasksTaskResolutionResult, IsINTaskResolutionResult taskResolutionResult) => inDeleteTasksTaskResolutionResult -> taskResolutionResult -> IO (Id INDeleteTasksTaskResolutionResult)
initWithTaskResolutionResult inDeleteTasksTaskResolutionResult  taskResolutionResult =
withObjCPtr taskResolutionResult $ \raw_taskResolutionResult ->
    sendMsg inDeleteTasksTaskResolutionResult (mkSelector "initWithTaskResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_taskResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskResolutionResult:@
initWithTaskResolutionResultSelector :: Selector
initWithTaskResolutionResultSelector = mkSelector "initWithTaskResolutionResult:"

