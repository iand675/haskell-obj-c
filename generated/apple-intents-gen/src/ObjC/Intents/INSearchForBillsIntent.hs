{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , billPayeeSelector
  , billTypeSelector
  , dueDateRangeSelector
  , initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector
  , paymentDateRangeSelector
  , statusSelector

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

-- | @- initWithBillPayee:paymentDateRange:billType:status:dueDateRange:@
initWithBillPayee_paymentDateRange_billType_status_dueDateRange :: (IsINSearchForBillsIntent inSearchForBillsIntent, IsINBillPayee billPayee, IsINDateComponentsRange paymentDateRange, IsINDateComponentsRange dueDateRange) => inSearchForBillsIntent -> billPayee -> paymentDateRange -> INBillType -> INPaymentStatus -> dueDateRange -> IO (Id INSearchForBillsIntent)
initWithBillPayee_paymentDateRange_billType_status_dueDateRange inSearchForBillsIntent billPayee paymentDateRange billType status dueDateRange =
  sendOwnedMessage inSearchForBillsIntent initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector (toINBillPayee billPayee) (toINDateComponentsRange paymentDateRange) billType status (toINDateComponentsRange dueDateRange)

-- | @- billPayee@
billPayee :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INBillPayee)
billPayee inSearchForBillsIntent =
  sendMessage inSearchForBillsIntent billPayeeSelector

-- | @- paymentDateRange@
paymentDateRange :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INDateComponentsRange)
paymentDateRange inSearchForBillsIntent =
  sendMessage inSearchForBillsIntent paymentDateRangeSelector

-- | @- billType@
billType :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO INBillType
billType inSearchForBillsIntent =
  sendMessage inSearchForBillsIntent billTypeSelector

-- | @- status@
status :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO INPaymentStatus
status inSearchForBillsIntent =
  sendMessage inSearchForBillsIntent statusSelector

-- | @- dueDateRange@
dueDateRange :: IsINSearchForBillsIntent inSearchForBillsIntent => inSearchForBillsIntent -> IO (Id INDateComponentsRange)
dueDateRange inSearchForBillsIntent =
  sendMessage inSearchForBillsIntent dueDateRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBillPayee:paymentDateRange:billType:status:dueDateRange:@
initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector :: Selector '[Id INBillPayee, Id INDateComponentsRange, INBillType, INPaymentStatus, Id INDateComponentsRange] (Id INSearchForBillsIntent)
initWithBillPayee_paymentDateRange_billType_status_dueDateRangeSelector = mkSelector "initWithBillPayee:paymentDateRange:billType:status:dueDateRange:"

-- | @Selector@ for @billPayee@
billPayeeSelector :: Selector '[] (Id INBillPayee)
billPayeeSelector = mkSelector "billPayee"

-- | @Selector@ for @paymentDateRange@
paymentDateRangeSelector :: Selector '[] (Id INDateComponentsRange)
paymentDateRangeSelector = mkSelector "paymentDateRange"

-- | @Selector@ for @billType@
billTypeSelector :: Selector '[] INBillType
billTypeSelector = mkSelector "billType"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INPaymentStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @dueDateRange@
dueDateRangeSelector :: Selector '[] (Id INDateComponentsRange)
dueDateRangeSelector = mkSelector "dueDateRange"

