{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallContactResolutionResult@.
module ObjC.Intents.INStartCallContactResolutionResult
  ( INStartCallContactResolutionResult
  , IsINStartCallContactResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , initWithPersonResolutionResultSelector
  , unsupportedForReasonSelector

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
unsupportedForReason :: INStartCallContactUnsupportedReason -> IO (Id INStartCallContactResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallContactResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINStartCallContactResolutionResult inStartCallContactResolutionResult, IsINPersonResolutionResult personResolutionResult) => inStartCallContactResolutionResult -> personResolutionResult -> IO (Id INStartCallContactResolutionResult)
initWithPersonResolutionResult inStartCallContactResolutionResult personResolutionResult =
  sendOwnedMessage inStartCallContactResolutionResult initWithPersonResolutionResultSelector (toINPersonResolutionResult personResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INStartCallContactUnsupportedReason] (Id INStartCallContactResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector '[Id INPersonResolutionResult] (Id INStartCallContactResolutionResult)
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

