{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBillDetails@.
module ObjC.Intents.INBillDetails
  ( INBillDetails
  , IsINBillDetails(..)
  , init_
  , initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDate
  , billPayee
  , setBillPayee
  , amountDue
  , setAmountDue
  , minimumDue
  , setMinimumDue
  , lateFee
  , setLateFee
  , dueDate
  , setDueDate
  , paymentDate
  , setPaymentDate
  , billType
  , setBillType
  , paymentStatus
  , setPaymentStatus
  , amountDueSelector
  , billPayeeSelector
  , billTypeSelector
  , dueDateSelector
  , initSelector
  , initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector
  , lateFeeSelector
  , minimumDueSelector
  , paymentDateSelector
  , paymentStatusSelector
  , setAmountDueSelector
  , setBillPayeeSelector
  , setBillTypeSelector
  , setDueDateSelector
  , setLateFeeSelector
  , setMinimumDueSelector
  , setPaymentDateSelector
  , setPaymentStatusSelector

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
  , INPaymentStatus(INPaymentStatus)
  , pattern INPaymentStatusUnknown
  , pattern INPaymentStatusPending
  , pattern INPaymentStatusCompleted
  , pattern INPaymentStatusCanceled
  , pattern INPaymentStatusFailed
  , pattern INPaymentStatusUnpaid

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
init_ :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INBillDetails)
init_ inBillDetails =
  sendOwnedMessage inBillDetails initSelector

-- | @- initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:@
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDate :: (IsINBillDetails inBillDetails, IsINBillPayee billPayee, IsINCurrencyAmount amountDue, IsINCurrencyAmount minimumDue, IsINCurrencyAmount lateFee, IsNSDateComponents dueDate, IsNSDateComponents paymentDate) => inBillDetails -> INBillType -> INPaymentStatus -> billPayee -> amountDue -> minimumDue -> lateFee -> dueDate -> paymentDate -> IO (Id INBillDetails)
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDate inBillDetails billType paymentStatus billPayee amountDue minimumDue lateFee dueDate paymentDate =
  sendOwnedMessage inBillDetails initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector billType paymentStatus (toINBillPayee billPayee) (toINCurrencyAmount amountDue) (toINCurrencyAmount minimumDue) (toINCurrencyAmount lateFee) (toNSDateComponents dueDate) (toNSDateComponents paymentDate)

-- | @- billPayee@
billPayee :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INBillPayee)
billPayee inBillDetails =
  sendMessage inBillDetails billPayeeSelector

-- | @- setBillPayee:@
setBillPayee :: (IsINBillDetails inBillDetails, IsINBillPayee value) => inBillDetails -> value -> IO ()
setBillPayee inBillDetails value =
  sendMessage inBillDetails setBillPayeeSelector (toINBillPayee value)

-- | @- amountDue@
amountDue :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
amountDue inBillDetails =
  sendMessage inBillDetails amountDueSelector

-- | @- setAmountDue:@
setAmountDue :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setAmountDue inBillDetails value =
  sendMessage inBillDetails setAmountDueSelector (toINCurrencyAmount value)

-- | @- minimumDue@
minimumDue :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
minimumDue inBillDetails =
  sendMessage inBillDetails minimumDueSelector

-- | @- setMinimumDue:@
setMinimumDue :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setMinimumDue inBillDetails value =
  sendMessage inBillDetails setMinimumDueSelector (toINCurrencyAmount value)

-- | @- lateFee@
lateFee :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
lateFee inBillDetails =
  sendMessage inBillDetails lateFeeSelector

-- | @- setLateFee:@
setLateFee :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setLateFee inBillDetails value =
  sendMessage inBillDetails setLateFeeSelector (toINCurrencyAmount value)

-- | @- dueDate@
dueDate :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id NSDateComponents)
dueDate inBillDetails =
  sendMessage inBillDetails dueDateSelector

-- | @- setDueDate:@
setDueDate :: (IsINBillDetails inBillDetails, IsNSDateComponents value) => inBillDetails -> value -> IO ()
setDueDate inBillDetails value =
  sendMessage inBillDetails setDueDateSelector (toNSDateComponents value)

-- | @- paymentDate@
paymentDate :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id NSDateComponents)
paymentDate inBillDetails =
  sendMessage inBillDetails paymentDateSelector

-- | @- setPaymentDate:@
setPaymentDate :: (IsINBillDetails inBillDetails, IsNSDateComponents value) => inBillDetails -> value -> IO ()
setPaymentDate inBillDetails value =
  sendMessage inBillDetails setPaymentDateSelector (toNSDateComponents value)

-- | @- billType@
billType :: IsINBillDetails inBillDetails => inBillDetails -> IO INBillType
billType inBillDetails =
  sendMessage inBillDetails billTypeSelector

-- | @- setBillType:@
setBillType :: IsINBillDetails inBillDetails => inBillDetails -> INBillType -> IO ()
setBillType inBillDetails value =
  sendMessage inBillDetails setBillTypeSelector value

-- | @- paymentStatus@
paymentStatus :: IsINBillDetails inBillDetails => inBillDetails -> IO INPaymentStatus
paymentStatus inBillDetails =
  sendMessage inBillDetails paymentStatusSelector

-- | @- setPaymentStatus:@
setPaymentStatus :: IsINBillDetails inBillDetails => inBillDetails -> INPaymentStatus -> IO ()
setPaymentStatus inBillDetails value =
  sendMessage inBillDetails setPaymentStatusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBillDetails)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:@
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector :: Selector '[INBillType, INPaymentStatus, Id INBillPayee, Id INCurrencyAmount, Id INCurrencyAmount, Id INCurrencyAmount, Id NSDateComponents, Id NSDateComponents] (Id INBillDetails)
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector = mkSelector "initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector '[] (Id INBillPayee)
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @setBillPayee:@
setBillPayeeSelector :: Selector '[Id INBillPayee] ()
setBillPayeeSelector = mkSelector "setBillPayee:"

-- | @Selector@ for @amountDue@
amountDueSelector :: Selector '[] (Id INCurrencyAmount)
amountDueSelector = mkSelector "amountDue"

-- | @Selector@ for @setAmountDue:@
setAmountDueSelector :: Selector '[Id INCurrencyAmount] ()
setAmountDueSelector = mkSelector "setAmountDue:"

-- | @Selector@ for @minimumDue@
minimumDueSelector :: Selector '[] (Id INCurrencyAmount)
minimumDueSelector = mkSelector "minimumDue"

-- | @Selector@ for @setMinimumDue:@
setMinimumDueSelector :: Selector '[Id INCurrencyAmount] ()
setMinimumDueSelector = mkSelector "setMinimumDue:"

-- | @Selector@ for @lateFee@
lateFeeSelector :: Selector '[] (Id INCurrencyAmount)
lateFeeSelector = mkSelector "lateFee"

-- | @Selector@ for @setLateFee:@
setLateFeeSelector :: Selector '[Id INCurrencyAmount] ()
setLateFeeSelector = mkSelector "setLateFee:"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector '[] (Id NSDateComponents)
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector '[Id NSDateComponents] ()
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @paymentDate@
paymentDateSelector :: Selector '[] (Id NSDateComponents)
paymentDateSelector = mkSelector "paymentDate"

-- | @Selector@ for @setPaymentDate:@
setPaymentDateSelector :: Selector '[Id NSDateComponents] ()
setPaymentDateSelector = mkSelector "setPaymentDate:"

-- | @Selector@ for @billType@
billTypeSelector :: Selector '[] INBillType
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @setBillType:@
setBillTypeSelector :: Selector '[INBillType] ()
setBillTypeSelector = mkSelector "setBillType:"

-- | @Selector@ for @paymentStatus@
paymentStatusSelector :: Selector '[] INPaymentStatus
paymentStatusSelector = mkSelector "paymentStatus"

-- | @Selector@ for @setPaymentStatus:@
setPaymentStatusSelector :: Selector '[INPaymentStatus] ()
setPaymentStatusSelector = mkSelector "setPaymentStatus:"

