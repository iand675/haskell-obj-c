{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageRecipientResolutionResult@.
module ObjC.Intents.INSendMessageRecipientResolutionResult
  ( INSendMessageRecipientResolutionResult
  , IsINSendMessageRecipientResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , initWithPersonResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSendMessageRecipientUnsupportedReason(INSendMessageRecipientUnsupportedReason)
  , pattern INSendMessageRecipientUnsupportedReasonNoAccount
  , pattern INSendMessageRecipientUnsupportedReasonOffline
  , pattern INSendMessageRecipientUnsupportedReasonMessagingServiceNotEnabledForRecipient
  , pattern INSendMessageRecipientUnsupportedReasonNoValidHandle
  , pattern INSendMessageRecipientUnsupportedReasonRequestedHandleInvalid
  , pattern INSendMessageRecipientUnsupportedReasonNoHandleForLabel
  , pattern INSendMessageRecipientUnsupportedReasonRequiringInAppAuthentication

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
unsupportedForReason :: INSendMessageRecipientUnsupportedReason -> IO (Id INSendMessageRecipientResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendMessageRecipientResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINSendMessageRecipientResolutionResult inSendMessageRecipientResolutionResult, IsINPersonResolutionResult personResolutionResult) => inSendMessageRecipientResolutionResult -> personResolutionResult -> IO (Id INSendMessageRecipientResolutionResult)
initWithPersonResolutionResult inSendMessageRecipientResolutionResult personResolutionResult =
  sendOwnedMessage inSendMessageRecipientResolutionResult initWithPersonResolutionResultSelector (toINPersonResolutionResult personResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSendMessageRecipientUnsupportedReason] (Id INSendMessageRecipientResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector '[Id INPersonResolutionResult] (Id INSendMessageRecipientResolutionResult)
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

