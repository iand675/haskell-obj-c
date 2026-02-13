{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPayBillIntent@.
module ObjC.Intents.INPayBillIntent
  ( INPayBillIntent
  , IsINPayBillIntent(..)
  , initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDate
  , billPayee
  , fromAccount
  , transactionAmount
  , transactionScheduledDate
  , transactionNote
  , billType
  , dueDate
  , billPayeeSelector
  , billTypeSelector
  , dueDateSelector
  , fromAccountSelector
  , initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector
  , transactionAmountSelector
  , transactionNoteSelector
  , transactionScheduledDateSelector

  -- * Enum types
  , INBillType(INBillType)
  , pattern INBillTypeUnknown
  , pattern INBillTypeAutoInsurance
  , pattern INBillTypeCable
  , pattern INBillTypeCarLease
  , pattern INBillTypeCarLoan
  , pattern INBillTypeCreditCard
  , pattern INBillTypeElectricity
  , pattern INBillTypeGas
  , pattern INBillTypeGarbageAndRecycling
  , pattern INBillTypeHealthInsurance
  , pattern INBillTypeHomeInsurance
  , pattern INBillTypeInternet
  , pattern INBillTypeLifeInsurance
  , pattern INBillTypeMortgage
  , pattern INBillTypeMusicStreaming
  , pattern INBillTypePhone
  , pattern INBillTypeRent
  , pattern INBillTypeSewer
  , pattern INBillTypeStudentLoan
  , pattern INBillTypeTrafficTicket
  , pattern INBillTypeTuition
  , pattern INBillTypeUtilities
  , pattern INBillTypeWater

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

-- | @- initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:@
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDate :: (IsINPayBillIntent inPayBillIntent, IsINBillPayee billPayee, IsINPaymentAccount fromAccount, IsINPaymentAmount transactionAmount, IsINDateComponentsRange transactionScheduledDate, IsNSString transactionNote, IsINDateComponentsRange dueDate) => inPayBillIntent -> billPayee -> fromAccount -> transactionAmount -> transactionScheduledDate -> transactionNote -> INBillType -> dueDate -> IO (Id INPayBillIntent)
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDate inPayBillIntent billPayee fromAccount transactionAmount transactionScheduledDate transactionNote billType dueDate =
  sendOwnedMessage inPayBillIntent initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector (toINBillPayee billPayee) (toINPaymentAccount fromAccount) (toINPaymentAmount transactionAmount) (toINDateComponentsRange transactionScheduledDate) (toNSString transactionNote) billType (toINDateComponentsRange dueDate)

-- | @- billPayee@
billPayee :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INBillPayee)
billPayee inPayBillIntent =
  sendMessage inPayBillIntent billPayeeSelector

-- | @- fromAccount@
fromAccount :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INPaymentAccount)
fromAccount inPayBillIntent =
  sendMessage inPayBillIntent fromAccountSelector

-- | @- transactionAmount@
transactionAmount :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INPaymentAmount)
transactionAmount inPayBillIntent =
  sendMessage inPayBillIntent transactionAmountSelector

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INDateComponentsRange)
transactionScheduledDate inPayBillIntent =
  sendMessage inPayBillIntent transactionScheduledDateSelector

-- | @- transactionNote@
transactionNote :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id NSString)
transactionNote inPayBillIntent =
  sendMessage inPayBillIntent transactionNoteSelector

-- | @- billType@
billType :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO INBillType
billType inPayBillIntent =
  sendMessage inPayBillIntent billTypeSelector

-- | @- dueDate@
dueDate :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INDateComponentsRange)
dueDate inPayBillIntent =
  sendMessage inPayBillIntent dueDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:@
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector :: Selector '[Id INBillPayee, Id INPaymentAccount, Id INPaymentAmount, Id INDateComponentsRange, Id NSString, INBillType, Id INDateComponentsRange] (Id INPayBillIntent)
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector = mkSelector "initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector '[] (Id INBillPayee)
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector '[] (Id INPaymentAccount)
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector '[] (Id INPaymentAmount)
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector '[] (Id INDateComponentsRange)
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector '[] (Id NSString)
transactionNoteSelector = mkSelector "transactionNote"

-- | @Selector@ for @billType@
billTypeSelector :: Selector '[] INBillType
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector '[] (Id INDateComponentsRange)
dueDateSelector = mkSelector "dueDate"

