{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentIntent@.
module ObjC.Intents.INSendPaymentIntent
  ( INSendPaymentIntent
  , IsINSendPaymentIntent(..)
  , initWithPayee_currencyAmount_note
  , payee
  , currencyAmount
  , note
  , currencyAmountSelector
  , initWithPayee_currencyAmount_noteSelector
  , noteSelector
  , payeeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPayee:currencyAmount:note:@
initWithPayee_currencyAmount_note :: (IsINSendPaymentIntent inSendPaymentIntent, IsINPerson payee, IsINCurrencyAmount currencyAmount, IsNSString note) => inSendPaymentIntent -> payee -> currencyAmount -> note -> IO (Id INSendPaymentIntent)
initWithPayee_currencyAmount_note inSendPaymentIntent payee currencyAmount note =
  sendOwnedMessage inSendPaymentIntent initWithPayee_currencyAmount_noteSelector (toINPerson payee) (toINCurrencyAmount currencyAmount) (toNSString note)

-- | @- payee@
payee :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id INPerson)
payee inSendPaymentIntent =
  sendMessage inSendPaymentIntent payeeSelector

-- | @- currencyAmount@
currencyAmount :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id INCurrencyAmount)
currencyAmount inSendPaymentIntent =
  sendMessage inSendPaymentIntent currencyAmountSelector

-- | @- note@
note :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id NSString)
note inSendPaymentIntent =
  sendMessage inSendPaymentIntent noteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayee:currencyAmount:note:@
initWithPayee_currencyAmount_noteSelector :: Selector '[Id INPerson, Id INCurrencyAmount, Id NSString] (Id INSendPaymentIntent)
initWithPayee_currencyAmount_noteSelector = mkSelector "initWithPayee:currencyAmount:note:"

-- | @Selector@ for @payee@
payeeSelector :: Selector '[] (Id INPerson)
payeeSelector = mkSelector "payee"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector '[] (Id INCurrencyAmount)
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

