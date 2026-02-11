{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector
  , billPayeeSelector
  , setBillPayeeSelector
  , amountDueSelector
  , setAmountDueSelector
  , minimumDueSelector
  , setMinimumDueSelector
  , lateFeeSelector
  , setLateFeeSelector
  , dueDateSelector
  , setDueDateSelector
  , paymentDateSelector
  , setPaymentDateSelector
  , billTypeSelector
  , setBillTypeSelector
  , paymentStatusSelector
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
init_ :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INBillDetails)
init_ inBillDetails  =
  sendMsg inBillDetails (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:@
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDate :: (IsINBillDetails inBillDetails, IsINBillPayee billPayee, IsINCurrencyAmount amountDue, IsINCurrencyAmount minimumDue, IsINCurrencyAmount lateFee, IsNSDateComponents dueDate, IsNSDateComponents paymentDate) => inBillDetails -> INBillType -> INPaymentStatus -> billPayee -> amountDue -> minimumDue -> lateFee -> dueDate -> paymentDate -> IO (Id INBillDetails)
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDate inBillDetails  billType paymentStatus billPayee amountDue minimumDue lateFee dueDate paymentDate =
withObjCPtr billPayee $ \raw_billPayee ->
  withObjCPtr amountDue $ \raw_amountDue ->
    withObjCPtr minimumDue $ \raw_minimumDue ->
      withObjCPtr lateFee $ \raw_lateFee ->
        withObjCPtr dueDate $ \raw_dueDate ->
          withObjCPtr paymentDate $ \raw_paymentDate ->
              sendMsg inBillDetails (mkSelector "initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:") (retPtr retVoid) [argCLong (coerce billType), argCLong (coerce paymentStatus), argPtr (castPtr raw_billPayee :: Ptr ()), argPtr (castPtr raw_amountDue :: Ptr ()), argPtr (castPtr raw_minimumDue :: Ptr ()), argPtr (castPtr raw_lateFee :: Ptr ()), argPtr (castPtr raw_dueDate :: Ptr ()), argPtr (castPtr raw_paymentDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- billPayee@
billPayee :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INBillPayee)
billPayee inBillDetails  =
  sendMsg inBillDetails (mkSelector "billPayee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillPayee:@
setBillPayee :: (IsINBillDetails inBillDetails, IsINBillPayee value) => inBillDetails -> value -> IO ()
setBillPayee inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setBillPayee:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- amountDue@
amountDue :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
amountDue inBillDetails  =
  sendMsg inBillDetails (mkSelector "amountDue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAmountDue:@
setAmountDue :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setAmountDue inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setAmountDue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumDue@
minimumDue :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
minimumDue inBillDetails  =
  sendMsg inBillDetails (mkSelector "minimumDue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinimumDue:@
setMinimumDue :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setMinimumDue inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setMinimumDue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lateFee@
lateFee :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id INCurrencyAmount)
lateFee inBillDetails  =
  sendMsg inBillDetails (mkSelector "lateFee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLateFee:@
setLateFee :: (IsINBillDetails inBillDetails, IsINCurrencyAmount value) => inBillDetails -> value -> IO ()
setLateFee inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setLateFee:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dueDate@
dueDate :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id NSDateComponents)
dueDate inBillDetails  =
  sendMsg inBillDetails (mkSelector "dueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDueDate:@
setDueDate :: (IsINBillDetails inBillDetails, IsNSDateComponents value) => inBillDetails -> value -> IO ()
setDueDate inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setDueDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paymentDate@
paymentDate :: IsINBillDetails inBillDetails => inBillDetails -> IO (Id NSDateComponents)
paymentDate inBillDetails  =
  sendMsg inBillDetails (mkSelector "paymentDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentDate:@
setPaymentDate :: (IsINBillDetails inBillDetails, IsNSDateComponents value) => inBillDetails -> value -> IO ()
setPaymentDate inBillDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBillDetails (mkSelector "setPaymentDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billType@
billType :: IsINBillDetails inBillDetails => inBillDetails -> IO INBillType
billType inBillDetails  =
  fmap (coerce :: CLong -> INBillType) $ sendMsg inBillDetails (mkSelector "billType") retCLong []

-- | @- setBillType:@
setBillType :: IsINBillDetails inBillDetails => inBillDetails -> INBillType -> IO ()
setBillType inBillDetails  value =
  sendMsg inBillDetails (mkSelector "setBillType:") retVoid [argCLong (coerce value)]

-- | @- paymentStatus@
paymentStatus :: IsINBillDetails inBillDetails => inBillDetails -> IO INPaymentStatus
paymentStatus inBillDetails  =
  fmap (coerce :: CLong -> INPaymentStatus) $ sendMsg inBillDetails (mkSelector "paymentStatus") retCLong []

-- | @- setPaymentStatus:@
setPaymentStatus :: IsINBillDetails inBillDetails => inBillDetails -> INPaymentStatus -> IO ()
setPaymentStatus inBillDetails  value =
  sendMsg inBillDetails (mkSelector "setPaymentStatus:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:@
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector :: Selector
initWithBillType_paymentStatus_billPayee_amountDue_minimumDue_lateFee_dueDate_paymentDateSelector = mkSelector "initWithBillType:paymentStatus:billPayee:amountDue:minimumDue:lateFee:dueDate:paymentDate:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @setBillPayee:@
setBillPayeeSelector :: Selector
setBillPayeeSelector = mkSelector "setBillPayee:"

-- | @Selector@ for @amountDue@
amountDueSelector :: Selector
amountDueSelector = mkSelector "amountDue"

-- | @Selector@ for @setAmountDue:@
setAmountDueSelector :: Selector
setAmountDueSelector = mkSelector "setAmountDue:"

-- | @Selector@ for @minimumDue@
minimumDueSelector :: Selector
minimumDueSelector = mkSelector "minimumDue"

-- | @Selector@ for @setMinimumDue:@
setMinimumDueSelector :: Selector
setMinimumDueSelector = mkSelector "setMinimumDue:"

-- | @Selector@ for @lateFee@
lateFeeSelector :: Selector
lateFeeSelector = mkSelector "lateFee"

-- | @Selector@ for @setLateFee:@
setLateFeeSelector :: Selector
setLateFeeSelector = mkSelector "setLateFee:"

-- | @Selector@ for @dueDate@
dueDateSelector :: Selector
dueDateSelector = mkSelector "dueDate"

-- | @Selector@ for @setDueDate:@
setDueDateSelector :: Selector
setDueDateSelector = mkSelector "setDueDate:"

-- | @Selector@ for @paymentDate@
paymentDateSelector :: Selector
paymentDateSelector = mkSelector "paymentDate"

-- | @Selector@ for @setPaymentDate:@
setPaymentDateSelector :: Selector
setPaymentDateSelector = mkSelector "setPaymentDate:"

-- | @Selector@ for @billType@
billTypeSelector :: Selector
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @setBillType:@
setBillTypeSelector :: Selector
setBillTypeSelector = mkSelector "setBillType:"

-- | @Selector@ for @paymentStatus@
paymentStatusSelector :: Selector
paymentStatusSelector = mkSelector "paymentStatus"

-- | @Selector@ for @setPaymentStatus:@
setPaymentStatusSelector :: Selector
setPaymentStatusSelector = mkSelector "setPaymentStatus:"

