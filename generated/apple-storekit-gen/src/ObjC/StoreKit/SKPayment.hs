{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPayment@.
module ObjC.StoreKit.SKPayment
  ( SKPayment
  , IsSKPayment(..)
  , paymentWithProduct
  , paymentWithProductIdentifier
  , productIdentifier
  , requestData
  , quantity
  , applicationUsername
  , simulatesAskToBuyInSandbox
  , paymentDiscount
  , applicationUsernameSelector
  , paymentDiscountSelector
  , paymentWithProductIdentifierSelector
  , paymentWithProductSelector
  , productIdentifierSelector
  , quantitySelector
  , requestDataSelector
  , simulatesAskToBuyInSandboxSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ paymentWithProduct:@
paymentWithProduct :: IsSKProduct product_ => product_ -> IO (Id SKPayment)
paymentWithProduct product_ =
  do
    cls' <- getRequiredClass "SKPayment"
    sendClassMessage cls' paymentWithProductSelector (toSKProduct product_)

-- | @+ paymentWithProductIdentifier:@
paymentWithProductIdentifier :: IsNSString identifier => identifier -> IO RawId
paymentWithProductIdentifier identifier =
  do
    cls' <- getRequiredClass "SKPayment"
    sendClassMessage cls' paymentWithProductIdentifierSelector (toNSString identifier)

-- | @- productIdentifier@
productIdentifier :: IsSKPayment skPayment => skPayment -> IO (Id NSString)
productIdentifier skPayment =
  sendMessage skPayment productIdentifierSelector

-- | @- requestData@
requestData :: IsSKPayment skPayment => skPayment -> IO (Id NSData)
requestData skPayment =
  sendMessage skPayment requestDataSelector

-- | @- quantity@
quantity :: IsSKPayment skPayment => skPayment -> IO CLong
quantity skPayment =
  sendMessage skPayment quantitySelector

-- | @- applicationUsername@
applicationUsername :: IsSKPayment skPayment => skPayment -> IO (Id NSString)
applicationUsername skPayment =
  sendMessage skPayment applicationUsernameSelector

-- | @- simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandbox :: IsSKPayment skPayment => skPayment -> IO Bool
simulatesAskToBuyInSandbox skPayment =
  sendMessage skPayment simulatesAskToBuyInSandboxSelector

-- | @- paymentDiscount@
paymentDiscount :: IsSKPayment skPayment => skPayment -> IO (Id SKPaymentDiscount)
paymentDiscount skPayment =
  sendMessage skPayment paymentDiscountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paymentWithProduct:@
paymentWithProductSelector :: Selector '[Id SKProduct] (Id SKPayment)
paymentWithProductSelector = mkSelector "paymentWithProduct:"

-- | @Selector@ for @paymentWithProductIdentifier:@
paymentWithProductIdentifierSelector :: Selector '[Id NSString] RawId
paymentWithProductIdentifierSelector = mkSelector "paymentWithProductIdentifier:"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector '[] (Id NSString)
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @requestData@
requestDataSelector :: Selector '[] (Id NSData)
requestDataSelector = mkSelector "requestData"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] CLong
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @applicationUsername@
applicationUsernameSelector :: Selector '[] (Id NSString)
applicationUsernameSelector = mkSelector "applicationUsername"

-- | @Selector@ for @simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandboxSelector :: Selector '[] Bool
simulatesAskToBuyInSandboxSelector = mkSelector "simulatesAskToBuyInSandbox"

-- | @Selector@ for @paymentDiscount@
paymentDiscountSelector :: Selector '[] (Id SKPaymentDiscount)
paymentDiscountSelector = mkSelector "paymentDiscount"

