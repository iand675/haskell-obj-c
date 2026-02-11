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
  , initSelector
  , initWithAmount_currencyCodeSelector
  , amountSelector
  , currencyCodeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id INCurrencyAmount)
init_ inCurrencyAmount  =
  sendMsg inCurrencyAmount (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAmount:currencyCode:@
initWithAmount_currencyCode :: (IsINCurrencyAmount inCurrencyAmount, IsNSDecimalNumber amount, IsNSString currencyCode) => inCurrencyAmount -> amount -> currencyCode -> IO (Id INCurrencyAmount)
initWithAmount_currencyCode inCurrencyAmount  amount currencyCode =
withObjCPtr amount $ \raw_amount ->
  withObjCPtr currencyCode $ \raw_currencyCode ->
      sendMsg inCurrencyAmount (mkSelector "initWithAmount:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_amount :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- amount@
amount :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id NSDecimalNumber)
amount inCurrencyAmount  =
  sendMsg inCurrencyAmount (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyCode@
currencyCode :: IsINCurrencyAmount inCurrencyAmount => inCurrencyAmount -> IO (Id NSString)
currencyCode inCurrencyAmount  =
  sendMsg inCurrencyAmount (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmount:currencyCode:@
initWithAmount_currencyCodeSelector :: Selector
initWithAmount_currencyCodeSelector = mkSelector "initWithAmount:currencyCode:"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

