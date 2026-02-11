{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTransferMoneyIntent@.
module ObjC.Intents.INTransferMoneyIntent
  ( INTransferMoneyIntent
  , IsINTransferMoneyIntent(..)
  , initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNote
  , fromAccount
  , toAccount
  , transactionAmount
  , transactionScheduledDate
  , transactionNote
  , initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector
  , fromAccountSelector
  , toAccountSelector
  , transactionAmountSelector
  , transactionScheduledDateSelector
  , transactionNoteSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:@
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNote :: (IsINTransferMoneyIntent inTransferMoneyIntent, IsINPaymentAccount fromAccount, IsINPaymentAccount toAccount, IsINPaymentAmount transactionAmount, IsINDateComponentsRange transactionScheduledDate, IsNSString transactionNote) => inTransferMoneyIntent -> fromAccount -> toAccount -> transactionAmount -> transactionScheduledDate -> transactionNote -> IO (Id INTransferMoneyIntent)
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNote inTransferMoneyIntent  fromAccount toAccount transactionAmount transactionScheduledDate transactionNote =
withObjCPtr fromAccount $ \raw_fromAccount ->
  withObjCPtr toAccount $ \raw_toAccount ->
    withObjCPtr transactionAmount $ \raw_transactionAmount ->
      withObjCPtr transactionScheduledDate $ \raw_transactionScheduledDate ->
        withObjCPtr transactionNote $ \raw_transactionNote ->
            sendMsg inTransferMoneyIntent (mkSelector "initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:") (retPtr retVoid) [argPtr (castPtr raw_fromAccount :: Ptr ()), argPtr (castPtr raw_toAccount :: Ptr ()), argPtr (castPtr raw_transactionAmount :: Ptr ()), argPtr (castPtr raw_transactionScheduledDate :: Ptr ()), argPtr (castPtr raw_transactionNote :: Ptr ())] >>= ownedObject . castPtr

-- | @- fromAccount@
fromAccount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAccount)
fromAccount inTransferMoneyIntent  =
  sendMsg inTransferMoneyIntent (mkSelector "fromAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toAccount@
toAccount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAccount)
toAccount inTransferMoneyIntent  =
  sendMsg inTransferMoneyIntent (mkSelector "toAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionAmount@
transactionAmount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAmount)
transactionAmount inTransferMoneyIntent  =
  sendMsg inTransferMoneyIntent (mkSelector "transactionAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INDateComponentsRange)
transactionScheduledDate inTransferMoneyIntent  =
  sendMsg inTransferMoneyIntent (mkSelector "transactionScheduledDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionNote@
transactionNote :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id NSString)
transactionNote inTransferMoneyIntent  =
  sendMsg inTransferMoneyIntent (mkSelector "transactionNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:@
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector :: Selector
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector = mkSelector "initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @toAccount@
toAccountSelector :: Selector
toAccountSelector = mkSelector "toAccount"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector
transactionNoteSelector = mkSelector "transactionNote"

