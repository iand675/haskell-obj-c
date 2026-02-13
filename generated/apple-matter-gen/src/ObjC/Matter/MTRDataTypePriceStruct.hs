{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypePriceStruct@.
module ObjC.Matter.MTRDataTypePriceStruct
  ( MTRDataTypePriceStruct
  , IsMTRDataTypePriceStruct(..)
  , amount
  , setAmount
  , currency
  , setCurrency
  , amountSelector
  , currencySelector
  , setAmountSelector
  , setCurrencySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- amount@
amount :: IsMTRDataTypePriceStruct mtrDataTypePriceStruct => mtrDataTypePriceStruct -> IO (Id NSNumber)
amount mtrDataTypePriceStruct =
  sendMessage mtrDataTypePriceStruct amountSelector

-- | @- setAmount:@
setAmount :: (IsMTRDataTypePriceStruct mtrDataTypePriceStruct, IsNSNumber value) => mtrDataTypePriceStruct -> value -> IO ()
setAmount mtrDataTypePriceStruct value =
  sendMessage mtrDataTypePriceStruct setAmountSelector (toNSNumber value)

-- | @- currency@
currency :: IsMTRDataTypePriceStruct mtrDataTypePriceStruct => mtrDataTypePriceStruct -> IO (Id MTRDataTypeCurrencyStruct)
currency mtrDataTypePriceStruct =
  sendMessage mtrDataTypePriceStruct currencySelector

-- | @- setCurrency:@
setCurrency :: (IsMTRDataTypePriceStruct mtrDataTypePriceStruct, IsMTRDataTypeCurrencyStruct value) => mtrDataTypePriceStruct -> value -> IO ()
setCurrency mtrDataTypePriceStruct value =
  sendMessage mtrDataTypePriceStruct setCurrencySelector (toMTRDataTypeCurrencyStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector '[Id NSNumber] ()
setAmountSelector = mkSelector "setAmount:"

-- | @Selector@ for @currency@
currencySelector :: Selector '[] (Id MTRDataTypeCurrencyStruct)
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector '[Id MTRDataTypeCurrencyStruct] ()
setCurrencySelector = mkSelector "setCurrency:"

