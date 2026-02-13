{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentIntent@.
module ObjC.Intents.INRequestPaymentIntent
  ( INRequestPaymentIntent
  , IsINRequestPaymentIntent(..)
  , initWithPayer_currencyAmount_note
  , payer
  , currencyAmount
  , note
  , currencyAmountSelector
  , initWithPayer_currencyAmount_noteSelector
  , noteSelector
  , payerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPayer:currencyAmount:note:@
initWithPayer_currencyAmount_note :: (IsINRequestPaymentIntent inRequestPaymentIntent, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsNSString note) => inRequestPaymentIntent -> payer -> currencyAmount -> note -> IO (Id INRequestPaymentIntent)
initWithPayer_currencyAmount_note inRequestPaymentIntent payer currencyAmount note =
  sendOwnedMessage inRequestPaymentIntent initWithPayer_currencyAmount_noteSelector (toINPerson payer) (toINCurrencyAmount currencyAmount) (toNSString note)

-- | @- payer@
payer :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id INPerson)
payer inRequestPaymentIntent =
  sendMessage inRequestPaymentIntent payerSelector

-- | @- currencyAmount@
currencyAmount :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id INCurrencyAmount)
currencyAmount inRequestPaymentIntent =
  sendMessage inRequestPaymentIntent currencyAmountSelector

-- | @- note@
note :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id NSString)
note inRequestPaymentIntent =
  sendMessage inRequestPaymentIntent noteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayer:currencyAmount:note:@
initWithPayer_currencyAmount_noteSelector :: Selector '[Id INPerson, Id INCurrencyAmount, Id NSString] (Id INRequestPaymentIntent)
initWithPayer_currencyAmount_noteSelector = mkSelector "initWithPayer:currencyAmount:note:"

-- | @Selector@ for @payer@
payerSelector :: Selector '[] (Id INPerson)
payerSelector = mkSelector "payer"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector '[] (Id INCurrencyAmount)
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

