{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKMutablePayment@.
module ObjC.StoreKit.SKMutablePayment
  ( SKMutablePayment
  , IsSKMutablePayment(..)
  , applicationUsername
  , setApplicationUsername
  , paymentDiscount
  , setPaymentDiscount
  , productIdentifier
  , setProductIdentifier
  , quantity
  , setQuantity
  , requestData
  , setRequestData
  , simulatesAskToBuyInSandbox
  , setSimulatesAskToBuyInSandbox
  , applicationUsernameSelector
  , paymentDiscountSelector
  , productIdentifierSelector
  , quantitySelector
  , requestDataSelector
  , setApplicationUsernameSelector
  , setPaymentDiscountSelector
  , setProductIdentifierSelector
  , setQuantitySelector
  , setRequestDataSelector
  , setSimulatesAskToBuyInSandboxSelector
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

-- | @- applicationUsername@
applicationUsername :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSString)
applicationUsername skMutablePayment =
  sendMessage skMutablePayment applicationUsernameSelector

-- | @- setApplicationUsername:@
setApplicationUsername :: (IsSKMutablePayment skMutablePayment, IsNSString value) => skMutablePayment -> value -> IO ()
setApplicationUsername skMutablePayment value =
  sendMessage skMutablePayment setApplicationUsernameSelector (toNSString value)

-- | @- paymentDiscount@
paymentDiscount :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id SKPaymentDiscount)
paymentDiscount skMutablePayment =
  sendMessage skMutablePayment paymentDiscountSelector

-- | @- setPaymentDiscount:@
setPaymentDiscount :: (IsSKMutablePayment skMutablePayment, IsSKPaymentDiscount value) => skMutablePayment -> value -> IO ()
setPaymentDiscount skMutablePayment value =
  sendMessage skMutablePayment setPaymentDiscountSelector (toSKPaymentDiscount value)

-- | @- productIdentifier@
productIdentifier :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSString)
productIdentifier skMutablePayment =
  sendMessage skMutablePayment productIdentifierSelector

-- | @- setProductIdentifier:@
setProductIdentifier :: (IsSKMutablePayment skMutablePayment, IsNSString value) => skMutablePayment -> value -> IO ()
setProductIdentifier skMutablePayment value =
  sendMessage skMutablePayment setProductIdentifierSelector (toNSString value)

-- | @- quantity@
quantity :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO CLong
quantity skMutablePayment =
  sendMessage skMutablePayment quantitySelector

-- | @- setQuantity:@
setQuantity :: IsSKMutablePayment skMutablePayment => skMutablePayment -> CLong -> IO ()
setQuantity skMutablePayment value =
  sendMessage skMutablePayment setQuantitySelector value

-- | @- requestData@
requestData :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSData)
requestData skMutablePayment =
  sendMessage skMutablePayment requestDataSelector

-- | @- setRequestData:@
setRequestData :: (IsSKMutablePayment skMutablePayment, IsNSData value) => skMutablePayment -> value -> IO ()
setRequestData skMutablePayment value =
  sendMessage skMutablePayment setRequestDataSelector (toNSData value)

-- | @- simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandbox :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO Bool
simulatesAskToBuyInSandbox skMutablePayment =
  sendMessage skMutablePayment simulatesAskToBuyInSandboxSelector

-- | @- setSimulatesAskToBuyInSandbox:@
setSimulatesAskToBuyInSandbox :: IsSKMutablePayment skMutablePayment => skMutablePayment -> Bool -> IO ()
setSimulatesAskToBuyInSandbox skMutablePayment value =
  sendMessage skMutablePayment setSimulatesAskToBuyInSandboxSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @applicationUsername@
applicationUsernameSelector :: Selector '[] (Id NSString)
applicationUsernameSelector = mkSelector "applicationUsername"

-- | @Selector@ for @setApplicationUsername:@
setApplicationUsernameSelector :: Selector '[Id NSString] ()
setApplicationUsernameSelector = mkSelector "setApplicationUsername:"

-- | @Selector@ for @paymentDiscount@
paymentDiscountSelector :: Selector '[] (Id SKPaymentDiscount)
paymentDiscountSelector = mkSelector "paymentDiscount"

-- | @Selector@ for @setPaymentDiscount:@
setPaymentDiscountSelector :: Selector '[Id SKPaymentDiscount] ()
setPaymentDiscountSelector = mkSelector "setPaymentDiscount:"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector '[] (Id NSString)
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @setProductIdentifier:@
setProductIdentifierSelector :: Selector '[Id NSString] ()
setProductIdentifierSelector = mkSelector "setProductIdentifier:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] CLong
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector '[CLong] ()
setQuantitySelector = mkSelector "setQuantity:"

-- | @Selector@ for @requestData@
requestDataSelector :: Selector '[] (Id NSData)
requestDataSelector = mkSelector "requestData"

-- | @Selector@ for @setRequestData:@
setRequestDataSelector :: Selector '[Id NSData] ()
setRequestDataSelector = mkSelector "setRequestData:"

-- | @Selector@ for @simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandboxSelector :: Selector '[] Bool
simulatesAskToBuyInSandboxSelector = mkSelector "simulatesAskToBuyInSandbox"

-- | @Selector@ for @setSimulatesAskToBuyInSandbox:@
setSimulatesAskToBuyInSandboxSelector :: Selector '[Bool] ()
setSimulatesAskToBuyInSandboxSelector = mkSelector "setSimulatesAskToBuyInSandbox:"

