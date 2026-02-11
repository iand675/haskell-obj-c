{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallCallRecordToCallBackResolutionResult@.
module ObjC.Intents.INStartCallCallRecordToCallBackResolutionResult
  ( INStartCallCallRecordToCallBackResolutionResult
  , IsINStartCallCallRecordToCallBackResolutionResult(..)
  , unsupportedForReason
  , initWithCallRecordResolutionResult
  , unsupportedForReasonSelector
  , initWithCallRecordResolutionResultSelector

  -- * Enum types
  , INStartCallCallRecordToCallBackUnsupportedReason(INStartCallCallRecordToCallBackUnsupportedReason)
  , pattern INStartCallCallRecordToCallBackUnsupportedReasonNoMatchingCall

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
unsupportedForReason :: INStartCallCallRecordToCallBackUnsupportedReason -> IO (Id INStartCallCallRecordToCallBackResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallCallRecordToCallBackResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithCallRecordResolutionResult:@
initWithCallRecordResolutionResult :: (IsINStartCallCallRecordToCallBackResolutionResult inStartCallCallRecordToCallBackResolutionResult, IsINCallRecordResolutionResult callRecordResolutionResult) => inStartCallCallRecordToCallBackResolutionResult -> callRecordResolutionResult -> IO (Id INStartCallCallRecordToCallBackResolutionResult)
initWithCallRecordResolutionResult inStartCallCallRecordToCallBackResolutionResult  callRecordResolutionResult =
withObjCPtr callRecordResolutionResult $ \raw_callRecordResolutionResult ->
    sendMsg inStartCallCallRecordToCallBackResolutionResult (mkSelector "initWithCallRecordResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_callRecordResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCallRecordResolutionResult:@
initWithCallRecordResolutionResultSelector :: Selector
initWithCallRecordResolutionResultSelector = mkSelector "initWithCallRecordResolutionResult:"

