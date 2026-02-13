{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTransferMoneyIntentResponse@.
module ObjC.Intents.INTransferMoneyIntentResponse
  ( INTransferMoneyIntentResponse
  , IsINTransferMoneyIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , fromAccount
  , setFromAccount
  , toAccount
  , setToAccount
  , transactionAmount
  , setTransactionAmount
  , transactionScheduledDate
  , setTransactionScheduledDate
  , transactionNote
  , setTransactionNote
  , transferFee
  , setTransferFee
  , codeSelector
  , fromAccountSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setFromAccountSelector
  , setToAccountSelector
  , setTransactionAmountSelector
  , setTransactionNoteSelector
  , setTransactionScheduledDateSelector
  , setTransferFeeSelector
  , toAccountSelector
  , transactionAmountSelector
  , transactionNoteSelector
  , transactionScheduledDateSelector
  , transferFeeSelector

  -- * Enum types
  , INTransferMoneyIntentResponseCode(INTransferMoneyIntentResponseCode)
  , pattern INTransferMoneyIntentResponseCodeUnspecified
  , pattern INTransferMoneyIntentResponseCodeReady
  , pattern INTransferMoneyIntentResponseCodeInProgress
  , pattern INTransferMoneyIntentResponseCodeSuccess
  , pattern INTransferMoneyIntentResponseCodeFailure
  , pattern INTransferMoneyIntentResponseCodeFailureRequiringAppLaunch
  , pattern INTransferMoneyIntentResponseCodeFailureCredentialsUnverified
  , pattern INTransferMoneyIntentResponseCodeFailureInsufficientFunds

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
init_ :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO RawId
init_ inTransferMoneyIntentResponse =
  sendOwnedMessage inTransferMoneyIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsNSUserActivity userActivity) => inTransferMoneyIntentResponse -> INTransferMoneyIntentResponseCode -> userActivity -> IO (Id INTransferMoneyIntentResponse)
initWithCode_userActivity inTransferMoneyIntentResponse code userActivity =
  sendOwnedMessage inTransferMoneyIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO INTransferMoneyIntentResponseCode
code inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse codeSelector

-- | @- fromAccount@
fromAccount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAccount)
fromAccount inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse fromAccountSelector

-- | @- setFromAccount:@
setFromAccount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAccount value) => inTransferMoneyIntentResponse -> value -> IO ()
setFromAccount inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setFromAccountSelector (toINPaymentAccount value)

-- | @- toAccount@
toAccount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAccount)
toAccount inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse toAccountSelector

-- | @- setToAccount:@
setToAccount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAccount value) => inTransferMoneyIntentResponse -> value -> IO ()
setToAccount inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setToAccountSelector (toINPaymentAccount value)

-- | @- transactionAmount@
transactionAmount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAmount)
transactionAmount inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse transactionAmountSelector

-- | @- setTransactionAmount:@
setTransactionAmount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAmount value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionAmount inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setTransactionAmountSelector (toINPaymentAmount value)

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INDateComponentsRange)
transactionScheduledDate inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse transactionScheduledDateSelector

-- | @- setTransactionScheduledDate:@
setTransactionScheduledDate :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINDateComponentsRange value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionScheduledDate inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setTransactionScheduledDateSelector (toINDateComponentsRange value)

-- | @- transactionNote@
transactionNote :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id NSString)
transactionNote inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse transactionNoteSelector

-- | @- setTransactionNote:@
setTransactionNote :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsNSString value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionNote inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setTransactionNoteSelector (toNSString value)

-- | @- transferFee@
transferFee :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INCurrencyAmount)
transferFee inTransferMoneyIntentResponse =
  sendMessage inTransferMoneyIntentResponse transferFeeSelector

-- | @- setTransferFee:@
setTransferFee :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINCurrencyAmount value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransferFee inTransferMoneyIntentResponse value =
  sendMessage inTransferMoneyIntentResponse setTransferFeeSelector (toINCurrencyAmount value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INTransferMoneyIntentResponseCode, Id NSUserActivity] (Id INTransferMoneyIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INTransferMoneyIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector '[] (Id INPaymentAccount)
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @setFromAccount:@
setFromAccountSelector :: Selector '[Id INPaymentAccount] ()
setFromAccountSelector = mkSelector "setFromAccount:"

-- | @Selector@ for @toAccount@
toAccountSelector :: Selector '[] (Id INPaymentAccount)
toAccountSelector = mkSelector "toAccount"

-- | @Selector@ for @setToAccount:@
setToAccountSelector :: Selector '[Id INPaymentAccount] ()
setToAccountSelector = mkSelector "setToAccount:"

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

-- | @Selector@ for @transferFee@
transferFeeSelector :: Selector '[] (Id INCurrencyAmount)
transferFeeSelector = mkSelector "transferFee"

-- | @Selector@ for @setTransferFee:@
setTransferFeeSelector :: Selector '[Id INCurrencyAmount] ()
setTransferFeeSelector = mkSelector "setTransferFee:"

