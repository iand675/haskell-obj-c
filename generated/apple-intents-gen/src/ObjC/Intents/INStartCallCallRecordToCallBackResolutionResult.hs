{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallCallRecordToCallBackResolutionResult@.
module ObjC.Intents.INStartCallCallRecordToCallBackResolutionResult
  ( INStartCallCallRecordToCallBackResolutionResult
  , IsINStartCallCallRecordToCallBackResolutionResult(..)
  , unsupportedForReason
  , initWithCallRecordResolutionResult
  , initWithCallRecordResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INStartCallCallRecordToCallBackUnsupportedReason(INStartCallCallRecordToCallBackUnsupportedReason)
  , pattern INStartCallCallRecordToCallBackUnsupportedReasonNoMatchingCall

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
unsupportedForReason :: INStartCallCallRecordToCallBackUnsupportedReason -> IO (Id INStartCallCallRecordToCallBackResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallCallRecordToCallBackResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithCallRecordResolutionResult:@
initWithCallRecordResolutionResult :: (IsINStartCallCallRecordToCallBackResolutionResult inStartCallCallRecordToCallBackResolutionResult, IsINCallRecordResolutionResult callRecordResolutionResult) => inStartCallCallRecordToCallBackResolutionResult -> callRecordResolutionResult -> IO (Id INStartCallCallRecordToCallBackResolutionResult)
initWithCallRecordResolutionResult inStartCallCallRecordToCallBackResolutionResult callRecordResolutionResult =
  sendOwnedMessage inStartCallCallRecordToCallBackResolutionResult initWithCallRecordResolutionResultSelector (toINCallRecordResolutionResult callRecordResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INStartCallCallRecordToCallBackUnsupportedReason] (Id INStartCallCallRecordToCallBackResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCallRecordResolutionResult:@
initWithCallRecordResolutionResultSelector :: Selector '[Id INCallRecordResolutionResult] (Id INStartCallCallRecordToCallBackResolutionResult)
initWithCallRecordResolutionResultSelector = mkSelector "initWithCallRecordResolutionResult:"

