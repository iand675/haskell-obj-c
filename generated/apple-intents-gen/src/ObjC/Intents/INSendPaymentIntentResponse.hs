{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentIntentResponse@.
module ObjC.Intents.INSendPaymentIntentResponse
  ( INSendPaymentIntentResponse
  , IsINSendPaymentIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , paymentRecord
  , setPaymentRecord
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , paymentRecordSelector
  , setPaymentRecordSelector

  -- * Enum types
  , INSendPaymentIntentResponseCode(INSendPaymentIntentResponseCode)
  , pattern INSendPaymentIntentResponseCodeUnspecified
  , pattern INSendPaymentIntentResponseCodeReady
  , pattern INSendPaymentIntentResponseCodeInProgress
  , pattern INSendPaymentIntentResponseCodeSuccess
  , pattern INSendPaymentIntentResponseCodeFailure
  , pattern INSendPaymentIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSendPaymentIntentResponseCodeFailureCredentialsUnverified
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported
  , pattern INSendPaymentIntentResponseCodeFailureInsufficientFunds
  , pattern INSendPaymentIntentResponseCodeFailureNoBankAccount
  , pattern INSendPaymentIntentResponseCodeFailureNotEligible
  , pattern INSendPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired

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

-- | @- init@
init_ :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO RawId
init_ inSendPaymentIntentResponse =
  sendOwnedMessage inSendPaymentIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendPaymentIntentResponse inSendPaymentIntentResponse, IsNSUserActivity userActivity) => inSendPaymentIntentResponse -> INSendPaymentIntentResponseCode -> userActivity -> IO (Id INSendPaymentIntentResponse)
initWithCode_userActivity inSendPaymentIntentResponse code userActivity =
  sendOwnedMessage inSendPaymentIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO INSendPaymentIntentResponseCode
code inSendPaymentIntentResponse =
  sendMessage inSendPaymentIntentResponse codeSelector

-- | @- paymentRecord@
paymentRecord :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO (Id INPaymentRecord)
paymentRecord inSendPaymentIntentResponse =
  sendMessage inSendPaymentIntentResponse paymentRecordSelector

-- | @- setPaymentRecord:@
setPaymentRecord :: (IsINSendPaymentIntentResponse inSendPaymentIntentResponse, IsINPaymentRecord value) => inSendPaymentIntentResponse -> value -> IO ()
setPaymentRecord inSendPaymentIntentResponse value =
  sendMessage inSendPaymentIntentResponse setPaymentRecordSelector (toINPaymentRecord value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSendPaymentIntentResponseCode, Id NSUserActivity] (Id INSendPaymentIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSendPaymentIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @paymentRecord@
paymentRecordSelector :: Selector '[] (Id INPaymentRecord)
paymentRecordSelector = mkSelector "paymentRecord"

-- | @Selector@ for @setPaymentRecord:@
setPaymentRecordSelector :: Selector '[Id INPaymentRecord] ()
setPaymentRecordSelector = mkSelector "setPaymentRecord:"

