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
  , initWithPayee_currencyAmount_noteSelector
  , payeeSelector
  , currencyAmountSelector
  , noteSelector


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

-- | @- initWithPayee:currencyAmount:note:@
initWithPayee_currencyAmount_note :: (IsINSendPaymentIntent inSendPaymentIntent, IsINPerson payee, IsINCurrencyAmount currencyAmount, IsNSString note) => inSendPaymentIntent -> payee -> currencyAmount -> note -> IO (Id INSendPaymentIntent)
initWithPayee_currencyAmount_note inSendPaymentIntent  payee currencyAmount note =
withObjCPtr payee $ \raw_payee ->
  withObjCPtr currencyAmount $ \raw_currencyAmount ->
    withObjCPtr note $ \raw_note ->
        sendMsg inSendPaymentIntent (mkSelector "initWithPayee:currencyAmount:note:") (retPtr retVoid) [argPtr (castPtr raw_payee :: Ptr ()), argPtr (castPtr raw_currencyAmount :: Ptr ()), argPtr (castPtr raw_note :: Ptr ())] >>= ownedObject . castPtr

-- | @- payee@
payee :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id INPerson)
payee inSendPaymentIntent  =
  sendMsg inSendPaymentIntent (mkSelector "payee") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyAmount@
currencyAmount :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id INCurrencyAmount)
currencyAmount inSendPaymentIntent  =
  sendMsg inSendPaymentIntent (mkSelector "currencyAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- note@
note :: IsINSendPaymentIntent inSendPaymentIntent => inSendPaymentIntent -> IO (Id NSString)
note inSendPaymentIntent  =
  sendMsg inSendPaymentIntent (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayee:currencyAmount:note:@
initWithPayee_currencyAmount_noteSelector :: Selector
initWithPayee_currencyAmount_noteSelector = mkSelector "initWithPayee:currencyAmount:note:"

-- | @Selector@ for @payee@
payeeSelector :: Selector
payeeSelector = mkSelector "payee"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

