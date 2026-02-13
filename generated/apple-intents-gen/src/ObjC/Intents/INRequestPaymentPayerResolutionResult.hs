{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentPayerResolutionResult@.
module ObjC.Intents.INRequestPaymentPayerResolutionResult
  ( INRequestPaymentPayerResolutionResult
  , IsINRequestPaymentPayerResolutionResult(..)
  , unsupportedForReason
  , initWithPersonResolutionResult
  , initWithPersonResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INRequestPaymentPayerUnsupportedReason(INRequestPaymentPayerUnsupportedReason)
  , pattern INRequestPaymentPayerUnsupportedReasonCredentialsUnverified
  , pattern INRequestPaymentPayerUnsupportedReasonNoAccount
  , pattern INRequestPaymentPayerUnsupportedReasonNoValidHandle

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
unsupportedForReason :: INRequestPaymentPayerUnsupportedReason -> IO (Id INRequestPaymentPayerResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INRequestPaymentPayerResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithPersonResolutionResult:@
initWithPersonResolutionResult :: (IsINRequestPaymentPayerResolutionResult inRequestPaymentPayerResolutionResult, IsINPersonResolutionResult personResolutionResult) => inRequestPaymentPayerResolutionResult -> personResolutionResult -> IO (Id INRequestPaymentPayerResolutionResult)
initWithPersonResolutionResult inRequestPaymentPayerResolutionResult personResolutionResult =
  sendOwnedMessage inRequestPaymentPayerResolutionResult initWithPersonResolutionResultSelector (toINPersonResolutionResult personResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INRequestPaymentPayerUnsupportedReason] (Id INRequestPaymentPayerResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithPersonResolutionResult:@
initWithPersonResolutionResultSelector :: Selector '[Id INPersonResolutionResult] (Id INRequestPaymentPayerResolutionResult)
initWithPersonResolutionResultSelector = mkSelector "initWithPersonResolutionResult:"

