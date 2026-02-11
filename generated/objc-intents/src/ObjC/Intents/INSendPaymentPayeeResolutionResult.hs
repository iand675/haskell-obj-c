{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentPayeeResolutionResult@.
module ObjC.Intents.INSendPaymentPayeeResolutionResult
  ( INSendPaymentPayeeResolutionResult
  , IsINSendPaymentPayeeResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , unsupportedForReasonSelector
  , initWithPersonResolutionResultSelector

  -- * Enum types
  , INSendPaymentPayeeUnsupportedReason(INSendPaymentPayeeUnsupportedReason)
  , pattern INSendPaymentPayeeUnsupportedReasonCredentialsUnverified
  , pattern INSendPaymentPayeeUnsupportedReasonInsufficientFunds
  , pattern INSendPaymentPayeeUnsupportedReasonNoAccount
  , pattern INSendPaymentPayeeUnsupportedReasonNoValidHandle

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
unsupportedForReason :: INSendPaymentPayeeUnsupportedReason -> IO (Id INSendPaymentPayeeResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INSendPaymentPayeeResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINSendPaymentPayeeResolutionResult inSendPaymentPayeeResolutionResult, IsINPersonResolutionResult personResolutionResult) => inSendPaymentPayeeResolutionResult -> personResolutionResult -> IO (Id INSendPaymentPayeeResolutionResult)
initWithPersonResolutionResult inSendPaymentPayeeResolutionResult  personResolutionResult =
withObjCPtr personResolutionResult $ \raw_personResolutionResult ->
    sendMsg inSendPaymentPayeeResolutionResult (mkSelector "initWithPersonResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_personResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

