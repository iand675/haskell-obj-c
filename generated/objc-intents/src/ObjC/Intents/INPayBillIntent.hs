{-# LANGUAGE PatternSynonyms #-}
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
  , initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector
  , billPayeeSelector
  , fromAccountSelector
  , transactionAmountSelector
  , transactionScheduledDateSelector
  , transactionNoteSelector
  , billTypeSelector
  , dueDateSelector

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

-- | @- initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:@
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDate :: (IsINPayBillIntent inPayBillIntent, IsINBillPayee billPayee, IsINPaymentAccount fromAccount, IsINPaymentAmount transactionAmount, IsINDateComponentsRange transactionScheduledDate, IsNSString transactionNote, IsINDateComponentsRange dueDate) => inPayBillIntent -> billPayee -> fromAccount -> transactionAmount -> transactionScheduledDate -> transactionNote -> INBillType -> dueDate -> IO (Id INPayBillIntent)
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDate inPayBillIntent  billPayee fromAccount transactionAmount transactionScheduledDate transactionNote billType dueDate =
withObjCPtr billPayee $ \raw_billPayee ->
  withObjCPtr fromAccount $ \raw_fromAccount ->
    withObjCPtr transactionAmount $ \raw_transactionAmount ->
      withObjCPtr transactionScheduledDate $ \raw_transactionScheduledDate ->
        withObjCPtr transactionNote $ \raw_transactionNote ->
          withObjCPtr dueDate $ \raw_dueDate ->
              sendMsg inPayBillIntent (mkSelector "initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:") (retPtr retVoid) [argPtr (castPtr raw_billPayee :: Ptr ()), argPtr (castPtr raw_fromAccount :: Ptr ()), argPtr (castPtr raw_transactionAmount :: Ptr ()), argPtr (castPtr raw_transactionScheduledDate :: Ptr ()), argPtr (castPtr raw_transactionNote :: Ptr ()), argCLong (coerce billType), argPtr (castPtr raw_dueDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- billPayee@
billPayee :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INBillPayee)
billPayee inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "billPayee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fromAccount@
fromAccount :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INPaymentAccount)
fromAccount inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "fromAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionAmount@
transactionAmount :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INPaymentAmount)
transactionAmount inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "transactionAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionScheduledDate@
transactionScheduledDate :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INDateComponentsRange)
transactionScheduledDate inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "transactionScheduledDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionNote@
transactionNote :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id NSString)
transactionNote inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "transactionNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- billType@
billType :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO INBillType
billType inPayBillIntent  =
  fmap (coerce :: CLong -> INBillType) $ sendMsg inPayBillIntent (mkSelector "billType") retCLong []

-- | @- dueDate@
dueDate :: IsINPayBillIntent inPayBillIntent => inPayBillIntent -> IO (Id INDateComponentsRange)
dueDate inPayBillIntent  =
  sendMsg inPayBillIntent (mkSelector "dueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:@
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector :: Selector
initWithBillPayee_fromAccount_transactionAmount_transactionScheduledDate_transactionNote_billType_dueDateSelector = mkSelector "initWithBillPayee:fromAccount:transactionAmount:transactionScheduledDate:transactionNote:billType:dueDate:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @fromAccount@
fromAccountSelector :: Selector
fromAccountSelector = mkSelector "fromAccount"

-- | @Selector@ for @transactionAmount@
transactionAmountSelector :: Selector
transactionAmountSelector = mkSelector "transactionAmount"

-- | @Selector@ for @transactionScheduledDate@
transactionScheduledDateSelector :: Selector
transactionScheduledDateSelector = mkSelector "transactionScheduledDate"

-- | @Selector@ for @transactionNote@
transactionNoteSelector :: Selector
transactionNoteSelector = mkSelector "transactionNote"

-- | @Selector@ for @billType@
billTypeSelector :: Selector
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector
dueDateSelector = mkSelector "dueDate"

