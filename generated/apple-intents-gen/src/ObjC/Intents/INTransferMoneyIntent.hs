{-# LANGUAGE DataKinds #-}
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
  , fromAccountSelector
  , initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector
  , toAccountSelector
  , transactionAmountSelector
  , transactionNoteSelector
  , transactionScheduledDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:@
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNote :: (IsINTransferMoneyIntent inTransferMoneyIntent, IsINPaymentAccount fromAccount, IsINPaymentAccount toAccount, IsINPaymentAmount transactionAmount, IsINDateComponentsRange transactionScheduledDate, IsNSString transactionNote) => inTransferMoneyIntent -> fromAccount -> toAccount -> transactionAmount -> transactionScheduledDate -> transactionNote -> IO (Id INTransferMoneyIntent)
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNote inTransferMoneyIntent fromAccount toAccount transactionAmount transactionScheduledDate transactionNote =
  sendOwnedMessage inTransferMoneyIntent initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector (toINPaymentAccount fromAccount) (toINPaymentAccount toAccount) (toINPaymentAmount transactionAmount) (toINDateComponentsRange transactionScheduledDate) (toNSString transactionNote)

-- | @- fromAccount@
fromAccount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAccount)
fromAccount inTransferMoneyIntent =
  sendMessage inTransferMoneyIntent fromAccountSelector

-- | @- toAccount@
toAccount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAccount)
toAccount inTransferMoneyIntent =
  sendMessage inTransferMoneyIntent toAccountSelector

-- | @- transactionAmount@
transactionAmount :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INPaymentAmount)
transactionAmount inTransferMoneyIntent =
  sendMessage inTransferMoneyIntent transactionAmountSelector

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id INDateComponentsRange)
transactionScheduledDate inTransferMoneyIntent =
  sendMessage inTransferMoneyIntent transactionScheduledDateSelector

-- | @- transactionNote@
transactionNote :: IsINTransferMoneyIntent inTransferMoneyIntent => inTransferMoneyIntent -> IO (Id NSString)
transactionNote inTransferMoneyIntent =
  sendMessage inTransferMoneyIntent transactionNoteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:@
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector :: Selector '[Id INPaymentAccount, Id INPaymentAccount, Id INPaymentAmount, Id INDateComponentsRange, Id NSString] (Id INTransferMoneyIntent)
initWithFromAccount_toAccount_transactionAmount_transactionScheduledDate_transactionNoteSelector = mkSelector "initWithFromAccount:toAccount:transactionAmount:transactionScheduledDate:transactionNote:"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector '[] (Id INPaymentAccount)
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @toAccount@
toAccountSelector :: Selector '[] (Id INPaymentAccount)
toAccountSelector = mkSelector "toAccount"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector '[] (Id INPaymentAmount)
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector '[] (Id INDateComponentsRange)
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector '[] (Id NSString)
transactionNoteSelector = mkSelector "transactionNote"

