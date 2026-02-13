{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , amountSelector
  , labelSelector
  , setAmountSelector
  , setLabelSelector
  , setTypeSelector
  , summaryItemWithLabel_amountSelector
  , summaryItemWithLabel_amount_typeSelector
  , typeSelector

  -- * Enum types
  , PKPaymentSummaryItemType(PKPaymentSummaryItemType)
  , pattern PKPaymentSummaryItemTypeFinal
  , pattern PKPaymentSummaryItemTypePending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' summaryItemWithLabel_amountSelector (toNSString label) (toNSDecimalNumber amount)

-- | @+ summaryItemWithLabel:amount:type:@
summaryItemWithLabel_amount_type :: (IsNSString label, IsNSDecimalNumber amount) => label -> amount -> PKPaymentSummaryItemType -> IO (Id PKPaymentSummaryItem)
summaryItemWithLabel_amount_type label amount type_ =
  do
    cls' <- getRequiredClass "PKPaymentSummaryItem"
    sendClassMessage cls' summaryItemWithLabel_amount_typeSelector (toNSString label) (toNSDecimalNumber amount) type_

-- | @- label@
label :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO (Id NSString)
label pkPaymentSummaryItem =
  sendMessage pkPaymentSummaryItem labelSelector

-- | @- setLabel:@
setLabel :: (IsPKPaymentSummaryItem pkPaymentSummaryItem, IsNSString value) => pkPaymentSummaryItem -> value -> IO ()
setLabel pkPaymentSummaryItem value =
  sendMessage pkPaymentSummaryItem setLabelSelector (toNSString value)

-- | @- amount@
amount :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO (Id NSDecimalNumber)
amount pkPaymentSummaryItem =
  sendMessage pkPaymentSummaryItem amountSelector

-- | @- setAmount:@
setAmount :: (IsPKPaymentSummaryItem pkPaymentSummaryItem, IsNSDecimalNumber value) => pkPaymentSummaryItem -> value -> IO ()
setAmount pkPaymentSummaryItem value =
  sendMessage pkPaymentSummaryItem setAmountSelector (toNSDecimalNumber value)

-- | @- type@
type_ :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> IO PKPaymentSummaryItemType
type_ pkPaymentSummaryItem =
  sendMessage pkPaymentSummaryItem typeSelector

-- | @- setType:@
setType :: IsPKPaymentSummaryItem pkPaymentSummaryItem => pkPaymentSummaryItem -> PKPaymentSummaryItemType -> IO ()
setType pkPaymentSummaryItem value =
  sendMessage pkPaymentSummaryItem setTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @summaryItemWithLabel:amount:@
summaryItemWithLabel_amountSelector :: Selector '[Id NSString, Id NSDecimalNumber] (Id PKPaymentSummaryItem)
summaryItemWithLabel_amountSelector = mkSelector "summaryItemWithLabel:amount:"

-- | @Selector@ for @summaryItemWithLabel:amount:type:@
summaryItemWithLabel_amount_typeSelector :: Selector '[Id NSString, Id NSDecimalNumber, PKPaymentSummaryItemType] (Id PKPaymentSummaryItem)
summaryItemWithLabel_amount_typeSelector = mkSelector "summaryItemWithLabel:amount:type:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSDecimalNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector '[Id NSDecimalNumber] ()
setAmountSelector = mkSelector "setAmount:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] PKPaymentSummaryItemType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[PKPaymentSummaryItemType] ()
setTypeSelector = mkSelector "setType:"

