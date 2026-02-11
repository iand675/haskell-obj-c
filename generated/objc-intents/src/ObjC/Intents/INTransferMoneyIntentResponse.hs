{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , fromAccountSelector
  , setFromAccountSelector
  , toAccountSelector
  , setToAccountSelector
  , transactionAmountSelector
  , setTransactionAmountSelector
  , transactionScheduledDateSelector
  , setTransactionScheduledDateSelector
  , transactionNoteSelector
  , setTransactionNoteSelector
  , transferFeeSelector
  , setTransferFeeSelector

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

-- | @- init@
init_ :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO RawId
init_ inTransferMoneyIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inTransferMoneyIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsNSUserActivity userActivity) => inTransferMoneyIntentResponse -> INTransferMoneyIntentResponseCode -> userActivity -> IO (Id INTransferMoneyIntentResponse)
initWithCode_userActivity inTransferMoneyIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO INTransferMoneyIntentResponseCode
code inTransferMoneyIntentResponse  =
  fmap (coerce :: CLong -> INTransferMoneyIntentResponseCode) $ sendMsg inTransferMoneyIntentResponse (mkSelector "code") retCLong []

-- | @- fromAccount@
fromAccount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAccount)
fromAccount inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "fromAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFromAccount:@
setFromAccount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAccount value) => inTransferMoneyIntentResponse -> value -> IO ()
setFromAccount inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setFromAccount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- toAccount@
toAccount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAccount)
toAccount inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "toAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToAccount:@
setToAccount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAccount value) => inTransferMoneyIntentResponse -> value -> IO ()
setToAccount inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setToAccount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionAmount@
transactionAmount :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INPaymentAmount)
transactionAmount inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "transactionAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionAmount:@
setTransactionAmount :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINPaymentAmount value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionAmount inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setTransactionAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INDateComponentsRange)
transactionScheduledDate inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "transactionScheduledDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionScheduledDate:@
setTransactionScheduledDate :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINDateComponentsRange value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionScheduledDate inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setTransactionScheduledDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionNote@
transactionNote :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id NSString)
transactionNote inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "transactionNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionNote:@
setTransactionNote :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsNSString value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransactionNote inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setTransactionNote:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transferFee@
transferFee :: IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse => inTransferMoneyIntentResponse -> IO (Id INCurrencyAmount)
transferFee inTransferMoneyIntentResponse  =
  sendMsg inTransferMoneyIntentResponse (mkSelector "transferFee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransferFee:@
setTransferFee :: (IsINTransferMoneyIntentResponse inTransferMoneyIntentResponse, IsINCurrencyAmount value) => inTransferMoneyIntentResponse -> value -> IO ()
setTransferFee inTransferMoneyIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inTransferMoneyIntentResponse (mkSelector "setTransferFee:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @setFromAccount:@
setFromAccountSelector :: Selector
setFromAccountSelector = mkSelector "setFromAccount:"

-- | @Selector@ for @toAccount@
toAccountSelector :: Selector
toAccountSelector = mkSelector "toAccount"

-- | @Selector@ for @setToAccount:@
setToAccountSelector :: Selector
setToAccountSelector = mkSelector "setToAccount:"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @setTransactionAmount:@
setTransactionAmountSelector :: Selector
setTransactionAmountSelector = mkSelector "setTransactionAmount:"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @setTransactionScheduledDate:@
setTransactionScheduledDateSelector :: Selector
setTransactionScheduledDateSelector = mkSelector "setTransactionScheduledDate:"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector
transactionNoteSelector = mkSelector "transactionNote"

-- | @Selector@ for @setTransactionNote:@
setTransactionNoteSelector :: Selector
setTransactionNoteSelector = mkSelector "setTransactionNote:"

-- | @Selector@ for @transferFee@
transferFeeSelector :: Selector
transferFeeSelector = mkSelector "transferFee"

-- | @Selector@ for @setTransferFee:@
setTransferFeeSelector :: Selector
setTransferFeeSelector = mkSelector "setTransferFee:"

