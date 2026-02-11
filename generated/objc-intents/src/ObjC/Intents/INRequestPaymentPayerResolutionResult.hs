{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentPayerResolutionResult@.
module ObjC.Intents.INRequestPaymentPayerResolutionResult
  ( INRequestPaymentPayerResolutionResult
  , IsINRequestPaymentPayerResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , unsupportedForReasonSelector
  , initWithPersonResolutionResultSelector

  -- * Enum types
  , INRequestPaymentPayerUnsupportedReason(INRequestPaymentPayerUnsupportedReason)
  , pattern INRequestPaymentPayerUnsupportedReasonCredentialsUnverified
  , pattern INRequestPaymentPayerUnsupportedReasonNoAccount
  , pattern INRequestPaymentPayerUnsupportedReasonNoValidHandle

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
unsupportedForReason :: INRequestPaymentPayerUnsupportedReason -> IO (Id INRequestPaymentPayerResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INRequestPaymentPayerResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINRequestPaymentPayerResolutionResult inRequestPaymentPayerResolutionResult, IsINPersonResolutionResult personResolutionResult) => inRequestPaymentPayerResolutionResult -> personResolutionResult -> IO (Id INRequestPaymentPayerResolutionResult)
initWithPersonResolutionResult inRequestPaymentPayerResolutionResult  personResolutionResult =
withObjCPtr personResolutionResult $ \raw_personResolutionResult ->
    sendMsg inRequestPaymentPayerResolutionResult (mkSelector "initWithPersonResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_personResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

