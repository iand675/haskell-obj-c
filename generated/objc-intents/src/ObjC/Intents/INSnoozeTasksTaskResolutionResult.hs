{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSnoozeTasksTaskResolutionResult@.
module ObjC.Intents.INSnoozeTasksTaskResolutionResult
  ( INSnoozeTasksTaskResolutionResult
  , IsINSnoozeTasksTaskResolutionResult(..)
  , unsupportedForReason
  , initWithTaskResolutionResult
  , unsupportedForReasonSelector
  , initWithTaskResolutionResultSelector

  -- * Enum types
  , INSnoozeTasksTaskUnsupportedReason(INSnoozeTasksTaskUnsupportedReason)
  , pattern INSnoozeTasksTaskUnsupportedReasonNoTasksFound

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
unsupportedForReason :: INSnoozeTasksTaskUnsupportedReason -> IO (Id INSnoozeTasksTaskResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSnoozeTasksTaskResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithTaskResolutionResult:@
initWithTaskResolutionResult :: (IsINSnoozeTasksTaskResolutionResult inSnoozeTasksTaskResolutionResult, IsINTaskResolutionResult taskResolutionResult) => inSnoozeTasksTaskResolutionResult -> taskResolutionResult -> IO (Id INSnoozeTasksTaskResolutionResult)
initWithTaskResolutionResult inSnoozeTasksTaskResolutionResult  taskResolutionResult =
withObjCPtr taskResolutionResult $ \raw_taskResolutionResult ->
    sendMsg inSnoozeTasksTaskResolutionResult (mkSelector "initWithTaskResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_taskResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithTaskResolutionResult:@
initWithTaskResolutionResultSelector :: Selector
initWithTaskResolutionResultSelector = mkSelector "initWithTaskResolutionResult:"

