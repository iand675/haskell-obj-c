{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageRecipientResolutionResult@.
module ObjC.Intents.INSendMessageRecipientResolutionResult
  ( INSendMessageRecipientResolutionResult
  , IsINSendMessageRecipientResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , unsupportedForReasonSelector
  , initWithPersonResolutionResultSelector

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
unsupportedForReason :: INSendMessageRecipientUnsupportedReason -> IO (Id INSendMessageRecipientResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendMessageRecipientResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINSendMessageRecipientResolutionResult inSendMessageRecipientResolutionResult, IsINPersonResolutionResult personResolutionResult) => inSendMessageRecipientResolutionResult -> personResolutionResult -> IO (Id INSendMessageRecipientResolutionResult)
initWithPersonResolutionResult inSendMessageRecipientResolutionResult  personResolutionResult =
withObjCPtr personResolutionResult $ \raw_personResolutionResult ->
    sendMsg inSendMessageRecipientResolutionResult (mkSelector "initWithPersonResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_personResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

