{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeCurrencyStruct@.
module ObjC.Matter.MTRDataTypeCurrencyStruct
  ( MTRDataTypeCurrencyStruct
  , IsMTRDataTypeCurrencyStruct(..)
  , currency
  , setCurrency
  , decimalPoints
  , setDecimalPoints
  , currencySelector
  , decimalPointsSelector
  , setCurrencySelector
  , setDecimalPointsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- currency@
currency :: IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct => mtrDataTypeCurrencyStruct -> IO (Id NSNumber)
currency mtrDataTypeCurrencyStruct =
  sendMessage mtrDataTypeCurrencyStruct currencySelector

-- | @- setCurrency:@
setCurrency :: (IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct, IsNSNumber value) => mtrDataTypeCurrencyStruct -> value -> IO ()
setCurrency mtrDataTypeCurrencyStruct value =
  sendMessage mtrDataTypeCurrencyStruct setCurrencySelector (toNSNumber value)

-- | @- decimalPoints@
decimalPoints :: IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct => mtrDataTypeCurrencyStruct -> IO (Id NSNumber)
decimalPoints mtrDataTypeCurrencyStruct =
  sendMessage mtrDataTypeCurrencyStruct decimalPointsSelector

-- | @- setDecimalPoints:@
setDecimalPoints :: (IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct, IsNSNumber value) => mtrDataTypeCurrencyStruct -> value -> IO ()
setDecimalPoints mtrDataTypeCurrencyStruct value =
  sendMessage mtrDataTypeCurrencyStruct setDecimalPointsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currency@
currencySelector :: Selector '[] (Id NSNumber)
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector '[Id NSNumber] ()
setCurrencySelector = mkSelector "setCurrency:"

-- | @Selector@ for @decimalPoints@
decimalPointsSelector :: Selector '[] (Id NSNumber)
decimalPointsSelector = mkSelector "decimalPoints"

-- | @Selector@ for @setDecimalPoints:@
setDecimalPointsSelector :: Selector '[Id NSNumber] ()
setDecimalPointsSelector = mkSelector "setDecimalPoints:"

