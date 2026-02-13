{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentPayeeResolutionResult@.
module ObjC.Intents.INSendPaymentPayeeResolutionResult
  ( INSendPaymentPayeeResolutionResult
  , IsINSendPaymentPayeeResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , initWithPersonResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INSendPaymentPayeeUnsupportedReason(INSendPaymentPayeeUnsupportedReason)
  , pattern INSendPaymentPayeeUnsupportedReasonCredentialsUnverified
  , pattern INSendPaymentPayeeUnsupportedReasonInsufficientFunds
  , pattern INSendPaymentPayeeUnsupportedReasonNoAccount
  , pattern INSendPaymentPayeeUnsupportedReasonNoValidHandle

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
unsupportedForReason :: INSendPaymentPayeeUnsupportedReason -> IO (Id INSendPaymentPayeeResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendPaymentPayeeResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINSendPaymentPayeeResolutionResult inSendPaymentPayeeResolutionResult, IsINPersonResolutionResult personResolutionResult) => inSendPaymentPayeeResolutionResult -> personResolutionResult -> IO (Id INSendPaymentPayeeResolutionResult)
initWithPersonResolutionResult inSendPaymentPayeeResolutionResult personResolutionResult =
  sendOwnedMessage inSendPaymentPayeeResolutionResult initWithPersonResolutionResultSelector (toINPersonResolutionResult personResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INSendPaymentPayeeUnsupportedReason] (Id INSendPaymentPayeeResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector '[Id INPersonResolutionResult] (Id INSendPaymentPayeeResolutionResult)
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

