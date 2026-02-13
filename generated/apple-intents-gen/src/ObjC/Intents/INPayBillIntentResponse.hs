{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPayBillIntentResponse@.
module ObjC.Intents.INPayBillIntentResponse
  ( INPayBillIntentResponse
  , IsINPayBillIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , fromAccount
  , setFromAccount
  , billDetails
  , setBillDetails
  , transactionAmount
  , setTransactionAmount
  , transactionScheduledDate
  , setTransactionScheduledDate
  , transactionNote
  , setTransactionNote
  , billDetailsSelector
  , codeSelector
  , fromAccountSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setBillDetailsSelector
  , setFromAccountSelector
  , setTransactionAmountSelector
  , setTransactionNoteSelector
  , setTransactionScheduledDateSelector
  , transactionAmountSelector
  , transactionNoteSelector
  , transactionScheduledDateSelector

  -- * Enum types
  , INPayBillIntentResponseCode(INPayBillIntentResponseCode)
  , pattern INPayBillIntentResponseCodeUnspecified
  , pattern INPayBillIntentResponseCodeReady
  , pattern INPayBillIntentResponseCodeInProgress
  , pattern INPayBillIntentResponseCodeSuccess
  , pattern INPayBillIntentResponseCodeFailure
  , pattern INPayBillIntentResponseCodeFailureRequiringAppLaunch
  , pattern INPayBillIntentResponseCodeFailureCredentialsUnverified
  , pattern INPayBillIntentResponseCodeFailureInsufficientFunds

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
init_ :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO RawId
init_ inPayBillIntentResponse =
  sendOwnedMessage inPayBillIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsNSUserActivity userActivity) => inPayBillIntentResponse -> INPayBillIntentResponseCode -> userActivity -> IO (Id INPayBillIntentResponse)
initWithCode_userActivity inPayBillIntentResponse code userActivity =
  sendOwnedMessage inPayBillIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO INPayBillIntentResponseCode
code inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse codeSelector

-- | @- fromAccount@
fromAccount :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INPaymentAccount)
fromAccount inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse fromAccountSelector

-- | @- setFromAccount:@
setFromAccount :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINPaymentAccount value) => inPayBillIntentResponse -> value -> IO ()
setFromAccount inPayBillIntentResponse value =
  sendMessage inPayBillIntentResponse setFromAccountSelector (toINPaymentAccount value)

-- | @- billDetails@
billDetails :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INBillDetails)
billDetails inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse billDetailsSelector

-- | @- setBillDetails:@
setBillDetails :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINBillDetails value) => inPayBillIntentResponse -> value -> IO ()
setBillDetails inPayBillIntentResponse value =
  sendMessage inPayBillIntentResponse setBillDetailsSelector (toINBillDetails value)

-- | @- transactionAmount@
transactionAmount :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INPaymentAmount)
transactionAmount inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse transactionAmountSelector

-- | @- setTransactionAmount:@
setTransactionAmount :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINPaymentAmount value) => inPayBillIntentResponse -> value -> IO ()
setTransactionAmount inPayBillIntentResponse value =
  sendMessage inPayBillIntentResponse setTransactionAmountSelector (toINPaymentAmount value)

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INDateComponentsRange)
transactionScheduledDate inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse transactionScheduledDateSelector

-- | @- setTransactionScheduledDate:@
setTransactionScheduledDate :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINDateComponentsRange value) => inPayBillIntentResponse -> value -> IO ()
setTransactionScheduledDate inPayBillIntentResponse value =
  sendMessage inPayBillIntentResponse setTransactionScheduledDateSelector (toINDateComponentsRange value)

-- | @- transactionNote@
transactionNote :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id NSString)
transactionNote inPayBillIntentResponse =
  sendMessage inPayBillIntentResponse transactionNoteSelector

-- | @- setTransactionNote:@
setTransactionNote :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsNSString value) => inPayBillIntentResponse -> value -> IO ()
setTransactionNote inPayBillIntentResponse value =
  sendMessage inPayBillIntentResponse setTransactionNoteSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INPayBillIntentResponseCode, Id NSUserActivity] (Id INPayBillIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INPayBillIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector '[] (Id INPaymentAccount)
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @setFromAccount:@
setFromAccountSelector :: Selector '[Id INPaymentAccount] ()
setFromAccountSelector = mkSelector "setFromAccount:"

-- | @Selector@ for @billDetails@
billDetailsSelector :: Selector '[] (Id INBillDetails)
billDetailsSelector = mkSelector "billDetails"

-- | @Selector@ for @setBillDetails:@
setBillDetailsSelector :: Selector '[Id INBillDetails] ()
setBillDetailsSelector = mkSelector "setBillDetails:"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector '[] (Id INPaymentAmount)
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @setTransactionAmount:@
setTransactionAmountSelector :: Selector '[Id INPaymentAmount] ()
setTransactionAmountSelector = mkSelector "setTransactionAmount:"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector '[] (Id INDateComponentsRange)
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @setTransactionScheduledDate:@
setTransactionScheduledDateSelector :: Selector '[Id INDateComponentsRange] ()
setTransactionScheduledDateSelector = mkSelector "setTransactionScheduledDate:"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector '[] (Id NSString)
transactionNoteSelector = mkSelector "transactionNote"

-- | @Selector@ for @setTransactionNote:@
setTransactionNoteSelector :: Selector '[Id NSString] ()
setTransactionNoteSelector = mkSelector "setTransactionNote:"

