{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentRecord@.
module ObjC.Intents.INPaymentRecord
  ( INPaymentRecord
  , IsINPaymentRecord(..)
  , init_
  , initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmount
  , initWithPayee_payer_currencyAmount_paymentMethod_note_status
  , payee
  , payer
  , currencyAmount
  , note
  , status
  , paymentMethod
  , feeAmount
  , currencyAmountSelector
  , feeAmountSelector
  , initSelector
  , initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector
  , initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector
  , noteSelector
  , payeeSelector
  , payerSelector
  , paymentMethodSelector
  , statusSelector

  -- * Enum types
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
init_ :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPaymentRecord)
init_ inPaymentRecord =
  sendOwnedMessage inPaymentRecord initSelector

-- | @- initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmount :: (IsINPaymentRecord inPaymentRecord, IsINPerson payee, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsINPaymentMethod paymentMethod, IsNSString note, IsINCurrencyAmount feeAmount) => inPaymentRecord -> payee -> payer -> currencyAmount -> paymentMethod -> note -> INPaymentStatus -> feeAmount -> IO (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmount inPaymentRecord payee payer currencyAmount paymentMethod note status feeAmount =
  sendOwnedMessage inPaymentRecord initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector (toINPerson payee) (toINPerson payer) (toINCurrencyAmount currencyAmount) (toINPaymentMethod paymentMethod) (toNSString note) status (toINCurrencyAmount feeAmount)

-- | @- initWithPayee:payer:currencyAmount:paymentMethod:note:status:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status :: (IsINPaymentRecord inPaymentRecord, IsINPerson payee, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsINPaymentMethod paymentMethod, IsNSString note) => inPaymentRecord -> payee -> payer -> currencyAmount -> paymentMethod -> note -> INPaymentStatus -> IO (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_status inPaymentRecord payee payer currencyAmount paymentMethod note status =
  sendOwnedMessage inPaymentRecord initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector (toINPerson payee) (toINPerson payer) (toINCurrencyAmount currencyAmount) (toINPaymentMethod paymentMethod) (toNSString note) status

-- | @- payee@
payee :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPerson)
payee inPaymentRecord =
  sendMessage inPaymentRecord payeeSelector

-- | @- payer@
payer :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPerson)
payer inPaymentRecord =
  sendMessage inPaymentRecord payerSelector

-- | @- currencyAmount@
currencyAmount :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INCurrencyAmount)
currencyAmount inPaymentRecord =
  sendMessage inPaymentRecord currencyAmountSelector

-- | @- note@
note :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id NSString)
note inPaymentRecord =
  sendMessage inPaymentRecord noteSelector

-- | @- status@
status :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO INPaymentStatus
status inPaymentRecord =
  sendMessage inPaymentRecord statusSelector

-- | @- paymentMethod@
paymentMethod :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPaymentMethod)
paymentMethod inPaymentRecord =
  sendMessage inPaymentRecord paymentMethodSelector

-- | @- feeAmount@
feeAmount :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INCurrencyAmount)
feeAmount inPaymentRecord =
  sendMessage inPaymentRecord feeAmountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPaymentRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector :: Selector '[Id INPerson, Id INPerson, Id INCurrencyAmount, Id INPaymentMethod, Id NSString, INPaymentStatus, Id INCurrencyAmount] (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector = mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:"

-- | @Selector@ for @initWithPayee:payer:currencyAmount:paymentMethod:note:status:@
initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector :: Selector '[Id INPerson, Id INPerson, Id INCurrencyAmount, Id INPaymentMethod, Id NSString, INPaymentStatus] (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector = mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:"

-- | @Selector@ for @payee@
payeeSelector :: Selector '[] (Id INPerson)
payeeSelector = mkSelector "payee"

-- | @Selector@ for @payer@
payerSelector :: Selector '[] (Id INPerson)
payerSelector = mkSelector "payer"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector '[] (Id INCurrencyAmount)
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INPaymentStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @paymentMethod@
paymentMethodSelector :: Selector '[] (Id INPaymentMethod)
paymentMethodSelector = mkSelector "paymentMethod"

-- | @Selector@ for @feeAmount@
feeAmountSelector :: Selector '[] (Id INCurrencyAmount)
feeAmountSelector = mkSelector "feeAmount"

