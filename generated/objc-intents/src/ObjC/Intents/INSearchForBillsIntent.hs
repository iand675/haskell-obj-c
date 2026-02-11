{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForBillsIntent@.
module ObjC.Intents.INSearchForBillsIntent
  ( INSearchForBillsIntent
  , IsINSearchForBillsIntent(..)
  , initWithBillPayee_paymentDateRange_billType_status_dueDateRange
  , billPayee
  , paymentDateRange
  , billType
  , status
  , dueDateRange
  , initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector
  , billPayeeSelector
  , paymentDateRangeSelector
  , billTypeSelector
  , statusSelector
  , dueDateRangeSelector

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

-- | @- initWithBillPayee:paymentDateRange:billType:status:dueDateRange:@
initWithBillPayee_paymentDateRange_billType_status_dueDateRange :: (IsINSearchForBillsIntent inSearchForBillsIntent, IsINBillPayee billPayee, IsINDateComponentsRange paymentDateRange, IsINDateComponentsRange dueDateRange) => inSearchForBillsIntent -> billPayee -> paymentDateRange -> INBillType -> INPaymentStatus -> dueDateRange -> IO (Id INSearchForBillsIntent)
initWithBillPayee_paymentDateRange_billType_status_dueDateRange inSearchForBillsIntent  billPayee paymentDateRange billType status dueDateRange =
withObjCPtr billPayee $ \raw_billPayee ->
  withObjCPtr paymentDateRange $ \raw_paymentDateRange ->
    withObjCPtr dueDateRange $ \raw_dueDateRange ->
        sendMsg inSearchForBillsIntent (mkSelector "initWithBillPayee:paymentDateRange:billType:status:dueDateRange:") (retPtr retVoid) [argPtr (castPtr raw_billPayee :: Ptr ()), argPtr (castPtr raw_paymentDateRange :: Ptr ()), argCLong (coerce billType), argCLong (coerce status), argPtr (castPtr raw_dueDateRange :: Ptr ())] >>= ownedObject . castPtr

-- | @- billPayee@
billPayee :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INBillPayee)
billPayee inSearchForBillsIntent  =
  sendMsg inSearchForBillsIntent (mkSelector "billPayee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paymentDateRange@
paymentDateRange :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INDateComponentsRange)
paymentDateRange inSearchForBillsIntent  =
  sendMsg inSearchForBillsIntent (mkSelector "paymentDateRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- billType@
billType :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO INBillType
billType inSearchForBillsIntent  =
  fmap (coerce :: CLong -> INBillType) $ sendMsg inSearchForBillsIntent (mkSelector "billType") retCLong []

-- | @- status@
status :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO INPaymentStatus
status inSearchForBillsIntent  =
  fmap (coerce :: CLong -> INPaymentStatus) $ sendMsg inSearchForBillsIntent (mkSelector "status") retCLong []

-- | @- dueDateRange@
dueDateRange :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INDateComponentsRange)
dueDateRange inSearchForBillsIntent  =
  sendMsg inSearchForBillsIntent (mkSelector "dueDateRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBillPayee:paymentDateRange:billType:status:dueDateRange:@
initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector :: Selector
initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector = mkSelector "initWithBillPayee:paymentDateRange:billType:status:dueDateRange:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @paymentDateRange@
paymentDateRangeSelector :: Selector
paymentDateRangeSelector = mkSelector "paymentDateRange"

-- | @Selector@ for @billType@
billTypeSelector :: Selector
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @dueDateRange@
dueDateRangeSelector :: Selector
dueDateRangeSelector = mkSelector "dueDateRange"

