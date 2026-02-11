{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , fromAccountSelector
  , setFromAccountSelector
  , billDetailsSelector
  , setBillDetailsSelector
  , transactionAmountSelector
  , setTransactionAmountSelector
  , transactionScheduledDateSelector
  , setTransactionScheduledDateSelector
  , transactionNoteSelector
  , setTransactionNoteSelector

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
init_ :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO RawId
init_ inPayBillIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inPayBillIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsNSUserActivity userActivity) => inPayBillIntentResponse -> INPayBillIntentResponseCode -> userActivity -> IO (Id INPayBillIntentResponse)
initWithCode_userActivity inPayBillIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inPayBillIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO INPayBillIntentResponseCode
code inPayBillIntentResponse  =
  fmap (coerce :: CLong -> INPayBillIntentResponseCode) $ sendMsg inPayBillIntentResponse (mkSelector "code") retCLong []

-- | @- fromAccount@
fromAccount :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INPaymentAccount)
fromAccount inPayBillIntentResponse  =
  sendMsg inPayBillIntentResponse (mkSelector "fromAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFromAccount:@
setFromAccount :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINPaymentAccount value) => inPayBillIntentResponse -> value -> IO ()
setFromAccount inPayBillIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPayBillIntentResponse (mkSelector "setFromAccount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billDetails@
billDetails :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INBillDetails)
billDetails inPayBillIntentResponse  =
  sendMsg inPayBillIntentResponse (mkSelector "billDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillDetails:@
setBillDetails :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINBillDetails value) => inPayBillIntentResponse -> value -> IO ()
setBillDetails inPayBillIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPayBillIntentResponse (mkSelector "setBillDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionAmount@
transactionAmount :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INPaymentAmount)
transactionAmount inPayBillIntentResponse  =
  sendMsg inPayBillIntentResponse (mkSelector "transactionAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionAmount:@
setTransactionAmount :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINPaymentAmount value) => inPayBillIntentResponse -> value -> IO ()
setTransactionAmount inPayBillIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPayBillIntentResponse (mkSelector "setTransactionAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id INDateComponentsRange)
transactionScheduledDate inPayBillIntentResponse  =
  sendMsg inPayBillIntentResponse (mkSelector "transactionScheduledDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionScheduledDate:@
setTransactionScheduledDate :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsINDateComponentsRange value) => inPayBillIntentResponse -> value -> IO ()
setTransactionScheduledDate inPayBillIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPayBillIntentResponse (mkSelector "setTransactionScheduledDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transactionNote@
transactionNote :: IsINPayBillIntentResponse inPayBillIntentResponse => inPayBillIntentResponse -> IO (Id NSString)
transactionNote inPayBillIntentResponse  =
  sendMsg inPayBillIntentResponse (mkSelector "transactionNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionNote:@
setTransactionNote :: (IsINPayBillIntentResponse inPayBillIntentResponse, IsNSString value) => inPayBillIntentResponse -> value -> IO ()
setTransactionNote inPayBillIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inPayBillIntentResponse (mkSelector "setTransactionNote:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @billDetails@
billDetailsSelector :: Selector
billDetailsSelector = mkSelector "billDetails"

-- | @Selector@ for @setBillDetails:@
setBillDetailsSelector :: Selector
setBillDetailsSelector = mkSelector "setBillDetails:"

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

