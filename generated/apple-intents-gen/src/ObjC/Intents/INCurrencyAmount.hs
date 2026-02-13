{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCurrencyAmount@.
module ObjC.Intents.INCurrencyAmount
  ( INCurrencyAmount
  , IsINCurrencyAmount(..)
  , init_
  , initWithAmount_currencyCode
  , amount
  , currencyCode
  , amountSelector
  , currencyCodeSelector
  , initSelector
  , initWithAmount_currencyCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id INCurrencyAmount)
init_ inCurrencyAmount =
  sendOwnedMessage inCurrencyAmount initSelector

-- | @- initWithAmount:currencyCode:@
initWithAmount_currencyCode :: (IsINCurrencyAmount inCurrencyAmount, IsNSDecimalNumber amount, IsNSString currencyCode) => inCurrencyAmount -> amount -> currencyCode -> IO (Id INCurrencyAmount)
initWithAmount_currencyCode inCurrencyAmount amount currencyCode =
  sendOwnedMessage inCurrencyAmount initWithAmount_currencyCodeSelector (toNSDecimalNumber amount) (toNSString currencyCode)

-- | @- amount@
amount :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id NSDecimalNumber)
amount inCurrencyAmount =
  sendMessage inCurrencyAmount amountSelector

-- | @- currencyCode@
currencyCode :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id NSString)
currencyCode inCurrencyAmount =
  sendMessage inCurrencyAmount currencyCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCurrencyAmount)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmount:currencyCode:@
initWithAmount_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSString] (Id INCurrencyAmount)
initWithAmount_currencyCodeSelector = mkSelector "initWithAmount:currencyCode:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSDecimalNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

