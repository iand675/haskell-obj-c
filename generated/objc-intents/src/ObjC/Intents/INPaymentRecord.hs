{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector
  , initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector
  , payeeSelector
  , payerSelector
  , currencyAmountSelector
  , noteSelector
  , statusSelector
  , paymentMethodSelector
  , feeAmountSelector

  -- * Enum types
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
init_ :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPaymentRecord)
init_ inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmount :: (IsINPaymentRecord inPaymentRecord, IsINPerson payee, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsINPaymentMethod paymentMethod, IsNSString note, IsINCurrencyAmount feeAmount) => inPaymentRecord -> payee -> payer -> currencyAmount -> paymentMethod -> note -> INPaymentStatus -> feeAmount -> IO (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmount inPaymentRecord  payee payer currencyAmount paymentMethod note status feeAmount =
withObjCPtr payee $ \raw_payee ->
  withObjCPtr payer $ \raw_payer ->
    withObjCPtr currencyAmount $ \raw_currencyAmount ->
      withObjCPtr paymentMethod $ \raw_paymentMethod ->
        withObjCPtr note $ \raw_note ->
          withObjCPtr feeAmount $ \raw_feeAmount ->
              sendMsg inPaymentRecord (mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:") (retPtr retVoid) [argPtr (castPtr raw_payee :: Ptr ()), argPtr (castPtr raw_payer :: Ptr ()), argPtr (castPtr raw_currencyAmount :: Ptr ()), argPtr (castPtr raw_paymentMethod :: Ptr ()), argPtr (castPtr raw_note :: Ptr ()), argCLong (coerce status), argPtr (castPtr raw_feeAmount :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPayee:payer:currencyAmount:paymentMethod:note:status:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status :: (IsINPaymentRecord inPaymentRecord, IsINPerson payee, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsINPaymentMethod paymentMethod, IsNSString note) => inPaymentRecord -> payee -> payer -> currencyAmount -> paymentMethod -> note -> INPaymentStatus -> IO (Id INPaymentRecord)
initWithPayee_payer_currencyAmount_paymentMethod_note_status inPaymentRecord  payee payer currencyAmount paymentMethod note status =
withObjCPtr payee $ \raw_payee ->
  withObjCPtr payer $ \raw_payer ->
    withObjCPtr currencyAmount $ \raw_currencyAmount ->
      withObjCPtr paymentMethod $ \raw_paymentMethod ->
        withObjCPtr note $ \raw_note ->
            sendMsg inPaymentRecord (mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:") (retPtr retVoid) [argPtr (castPtr raw_payee :: Ptr ()), argPtr (castPtr raw_payer :: Ptr ()), argPtr (castPtr raw_currencyAmount :: Ptr ()), argPtr (castPtr raw_paymentMethod :: Ptr ()), argPtr (castPtr raw_note :: Ptr ()), argCLong (coerce status)] >>= ownedObject . castPtr

-- | @- payee@
payee :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPerson)
payee inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "payee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- payer@
payer :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPerson)
payer inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "payer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyAmount@
currencyAmount :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INCurrencyAmount)
currencyAmount inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "currencyAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- note@
note :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id NSString)
note inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO INPaymentStatus
status inPaymentRecord  =
  fmap (coerce :: CLong -> INPaymentStatus) $ sendMsg inPaymentRecord (mkSelector "status") retCLong []

-- | @- paymentMethod@
paymentMethod :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INPaymentMethod)
paymentMethod inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "paymentMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- feeAmount@
feeAmount :: IsINPaymentRecord inPaymentRecord => inPaymentRecord -> IO (Id INCurrencyAmount)
feeAmount inPaymentRecord  =
  sendMsg inPaymentRecord (mkSelector "feeAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:@
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector :: Selector
initWithPayee_payer_currencyAmount_paymentMethod_note_status_feeAmountSelector = mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:feeAmount:"

-- | @Selector@ for @initWithPayee:payer:currencyAmount:paymentMethod:note:status:@
initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector :: Selector
initWithPayee_payer_currencyAmount_paymentMethod_note_statusSelector = mkSelector "initWithPayee:payer:currencyAmount:paymentMethod:note:status:"

-- | @Selector@ for @payee@
payeeSelector :: Selector
payeeSelector = mkSelector "payee"

-- | @Selector@ for @payer@
payerSelector :: Selector
payerSelector = mkSelector "payer"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @paymentMethod@
paymentMethodSelector :: Selector
paymentMethodSelector = mkSelector "paymentMethod"

-- | @Selector@ for @feeAmount@
feeAmountSelector :: Selector
feeAmountSelector = mkSelector "feeAmount"

