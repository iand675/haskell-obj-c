{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentIntentResponse@.
module ObjC.Intents.INRequestPaymentIntentResponse
  ( INRequestPaymentIntentResponse
  , IsINRequestPaymentIntentResponse(..)
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
  , INRequestPaymentIntentResponseCode(INRequestPaymentIntentResponseCode)
  , pattern INRequestPaymentIntentResponseCodeUnspecified
  , pattern INRequestPaymentIntentResponseCodeReady
  , pattern INRequestPaymentIntentResponseCodeInProgress
  , pattern INRequestPaymentIntentResponseCodeSuccess
  , pattern INRequestPaymentIntentResponseCodeFailure
  , pattern INRequestPaymentIntentResponseCodeFailureRequiringAppLaunch
  , pattern INRequestPaymentIntentResponseCodeFailureCredentialsUnverified
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported
  , pattern INRequestPaymentIntentResponseCodeFailureNoBankAccount
  , pattern INRequestPaymentIntentResponseCodeFailureNotEligible
  , pattern INRequestPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired

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
init_ :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO RawId
init_ inRequestPaymentIntentResponse =
  sendOwnedMessage inRequestPaymentIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse, IsNSUserActivity userActivity) => inRequestPaymentIntentResponse -> INRequestPaymentIntentResponseCode -> userActivity -> IO (Id INRequestPaymentIntentResponse)
initWithCode_userActivity inRequestPaymentIntentResponse code userActivity =
  sendOwnedMessage inRequestPaymentIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO INRequestPaymentIntentResponseCode
code inRequestPaymentIntentResponse =
  sendMessage inRequestPaymentIntentResponse codeSelector

-- | @- paymentRecord@
paymentRecord :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO (Id INPaymentRecord)
paymentRecord inRequestPaymentIntentResponse =
  sendMessage inRequestPaymentIntentResponse paymentRecordSelector

-- | @- setPaymentRecord:@
setPaymentRecord :: (IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse, IsINPaymentRecord value) => inRequestPaymentIntentResponse -> value -> IO ()
setPaymentRecord inRequestPaymentIntentResponse value =
  sendMessage inRequestPaymentIntentResponse setPaymentRecordSelector (toINPaymentRecord value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INRequestPaymentIntentResponseCode, Id NSUserActivity] (Id INRequestPaymentIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INRequestPaymentIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @paymentRecord@
paymentRecordSelector :: Selector '[] (Id INPaymentRecord)
paymentRecordSelector = mkSelector "paymentRecord"

-- | @Selector@ for @setPaymentRecord:@
setPaymentRecordSelector :: Selector '[Id INPaymentRecord] ()
setPaymentRecordSelector = mkSelector "setPaymentRecord:"

