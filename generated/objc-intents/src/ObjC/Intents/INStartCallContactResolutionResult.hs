{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallContactResolutionResult@.
module ObjC.Intents.INStartCallContactResolutionResult
  ( INStartCallContactResolutionResult
  , IsINStartCallContactResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , unsupportedForReasonSelector
  , initWithPersonResolutionResultSelector

  -- * Enum types
  , INStartCallContactUnsupportedReason(INStartCallContactUnsupportedReason)
  , pattern INStartCallContactUnsupportedReasonNoContactFound
  , pattern INStartCallContactUnsupportedReasonMultipleContactsUnsupported
  , pattern INStartCallContactUnsupportedReasonNoHandleForLabel
  , pattern INStartCallContactUnsupportedReasonInvalidHandle
  , pattern INStartCallContactUnsupportedReasonUnsupportedMmiUssd
  , pattern INStartCallContactUnsupportedReasonNoCallHistoryForRedial
  , pattern INStartCallContactUnsupportedReasonNoUsableHandleForRedial
  , pattern INStartCallContactUnsupportedReasonRequiringInAppAuthentication

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
unsupportedForReason :: INStartCallContactUnsupportedReason -> IO (Id INStartCallContactResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallContactResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINStartCallContactResolutionResult inStartCallContactResolutionResult, IsINPersonResolutionResult personResolutionResult) => inStartCallContactResolutionResult -> personResolutionResult -> IO (Id INStartCallContactResolutionResult)
initWithPersonResolutionResult inStartCallContactResolutionResult  personResolutionResult =
withObjCPtr personResolutionResult $ \raw_personResolutionResult ->
    sendMsg inStartCallContactResolutionResult (mkSelector "initWithPersonResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_personResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

