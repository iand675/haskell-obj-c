{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentSummaryItem@.
module ObjC.PassKit.PKPaymentSummaryItem
  ( PKPaymentSummaryItem
  , IsPKPaymentSummaryItem(..)
  , summaryItemWithLabel_amount
  , summaryItemWithLabel_amount_type
  , label
  , setLabel
  , amount
  , setAmount
  , type_
  , setType
  , summaryItemWithLabel_amountSelector
  , summaryItemWithLabel_amount_typeSelector
  , labelSelector
  , setLabelSelector
  , amountSelector
  , setAmountSelector
  , typeSelector
  , setTypeSelector

  -- * Enum types
  , PKPaymentSummaryItemType(PKPaymentSummaryItemType)
  , pattern PKPaymentSummaryItemTypeFinal
  , pattern PKPaymentSummaryItemTypePending

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ summaryItemWithLabel:amount:@
summaryItemWithLabel_amount :: (IsNSString label, IsNSDecimalNumber amount) => label -> amount -> IO (Id PKPaymentSummaryItem)
summaryItemWithLabel_amount label amount =
  do
    cls' <- getRequiredClass "PKPaymentSummaryItem"
    withObjCPtr label $ \raw_label ->
      withObjCPtr amount $ \raw_amount ->
        sendClassMsg cls' (mkSelector "summaryItemWithLabel:amount:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_amount :: Ptr ())] >>= retainedObject . castPtr

-- | @+ summaryItemWithLabel:amount:type:@
summaryItemWithLabel_amount_type :: (IsNSString label, IsNSDecimalNumber amount) => label -> amount -> PKPaymentSummaryItemType -> IO (Id PKPaymentSummaryItem)
summaryItemWithLabel_amount_type label amount type_ =
  do
    cls' <- getRequiredClass "PKPaymentSummaryItem"
    withObjCPtr label $ \raw_label ->
      withObjCPtr amount $ \raw_amount ->
        sendClassMsg cls' (mkSelector "summaryItemWithLabel:amount:type:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_amount :: Ptr ()), argCULong (coerce type_)] >>= retainedObject . castPtr

-- | @- label@
label :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO (Id NSString)
label pkPaymentSummaryItem  =
  sendMsg pkPaymentSummaryItem (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsPKPaymentSummaryItem pkPaymentSummaryItem, IsNSString value) => pkPaymentSummaryItem -> value -> IO ()
setLabel pkPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentSummaryItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- amount@
amount :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO (Id NSDecimalNumber)
amount pkPaymentSummaryItem  =
  sendMsg pkPaymentSummaryItem (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAmount:@
setAmount :: (IsPKPaymentSummaryItem pkPaymentSummaryItem, IsNSDecimalNumber value) => pkPaymentSummaryItem -> value -> IO ()
setAmount pkPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentSummaryItem (mkSelector "setAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO PKPaymentSummaryItemType
type_ pkPaymentSummaryItem  =
  fmap (coerce :: CULong -> PKPaymentSummaryItemType) $ sendMsg pkPaymentSummaryItem (mkSelector "type") retCULong []

-- | @- setType:@
setType :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> PKPaymentSummaryItemType -> IO ()
setType pkPaymentSummaryItem  value =
  sendMsg pkPaymentSummaryItem (mkSelector "setType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @summaryItemWithLabel:amount:@
summaryItemWithLabel_amountSelector :: Selector
summaryItemWithLabel_amountSelector = mkSelector "summaryItemWithLabel:amount:"

-- | @Selector@ for @summaryItemWithLabel:amount:type:@
summaryItemWithLabel_amount_typeSelector :: Selector
summaryItemWithLabel_amount_typeSelector = mkSelector "summaryItemWithLabel:amount:type:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector
setAmountSelector = mkSelector "setAmount:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

