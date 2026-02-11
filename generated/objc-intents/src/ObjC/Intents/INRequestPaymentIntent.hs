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
  , initWithPayer_currencyAmount_noteSelector
  , payerSelector
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

-- | @- initWithPayer:currencyAmount:note:@
initWithPayer_currencyAmount_note :: (IsINRequestPaymentIntent inRequestPaymentIntent, IsINPerson payer, IsINCurrencyAmount currencyAmount, IsNSString note) => inRequestPaymentIntent -> payer -> currencyAmount -> note -> IO (Id INRequestPaymentIntent)
initWithPayer_currencyAmount_note inRequestPaymentIntent  payer currencyAmount note =
withObjCPtr payer $ \raw_payer ->
  withObjCPtr currencyAmount $ \raw_currencyAmount ->
    withObjCPtr note $ \raw_note ->
        sendMsg inRequestPaymentIntent (mkSelector "initWithPayer:currencyAmount:note:") (retPtr retVoid) [argPtr (castPtr raw_payer :: Ptr ()), argPtr (castPtr raw_currencyAmount :: Ptr ()), argPtr (castPtr raw_note :: Ptr ())] >>= ownedObject . castPtr

-- | @- payer@
payer :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id INPerson)
payer inRequestPaymentIntent  =
  sendMsg inRequestPaymentIntent (mkSelector "payer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyAmount@
currencyAmount :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id INCurrencyAmount)
currencyAmount inRequestPaymentIntent  =
  sendMsg inRequestPaymentIntent (mkSelector "currencyAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- note@
note :: IsINRequestPaymentIntent inRequestPaymentIntent => inRequestPaymentIntent -> IO (Id NSString)
note inRequestPaymentIntent  =
  sendMsg inRequestPaymentIntent (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPayer:currencyAmount:note:@
initWithPayer_currencyAmount_noteSelector :: Selector
initWithPayer_currencyAmount_noteSelector = mkSelector "initWithPayer:currencyAmount:note:"

-- | @Selector@ for @payer@
payerSelector :: Selector
payerSelector = mkSelector "payer"

-- | @Selector@ for @currencyAmount@
currencyAmountSelector :: Selector
currencyAmountSelector = mkSelector "currencyAmount"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

