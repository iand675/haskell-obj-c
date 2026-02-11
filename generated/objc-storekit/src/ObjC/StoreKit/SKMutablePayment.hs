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
  , setApplicationUsernameSelector
  , paymentDiscountSelector
  , setPaymentDiscountSelector
  , productIdentifierSelector
  , setProductIdentifierSelector
  , quantitySelector
  , setQuantitySelector
  , requestDataSelector
  , setRequestDataSelector
  , simulatesAskToBuyInSandboxSelector
  , setSimulatesAskToBuyInSandboxSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- applicationUsername@
applicationUsername :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSString)
applicationUsername skMutablePayment  =
  sendMsg skMutablePayment (mkSelector "applicationUsername") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationUsername:@
setApplicationUsername :: (IsSKMutablePayment skMutablePayment, IsNSString value) => skMutablePayment -> value -> IO ()
setApplicationUsername skMutablePayment  value =
withObjCPtr value $ \raw_value ->
    sendMsg skMutablePayment (mkSelector "setApplicationUsername:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paymentDiscount@
paymentDiscount :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id SKPaymentDiscount)
paymentDiscount skMutablePayment  =
  sendMsg skMutablePayment (mkSelector "paymentDiscount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentDiscount:@
setPaymentDiscount :: (IsSKMutablePayment skMutablePayment, IsSKPaymentDiscount value) => skMutablePayment -> value -> IO ()
setPaymentDiscount skMutablePayment  value =
withObjCPtr value $ \raw_value ->
    sendMsg skMutablePayment (mkSelector "setPaymentDiscount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productIdentifier@
productIdentifier :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSString)
productIdentifier skMutablePayment  =
  sendMsg skMutablePayment (mkSelector "productIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifier:@
setProductIdentifier :: (IsSKMutablePayment skMutablePayment, IsNSString value) => skMutablePayment -> value -> IO ()
setProductIdentifier skMutablePayment  value =
withObjCPtr value $ \raw_value ->
    sendMsg skMutablePayment (mkSelector "setProductIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- quantity@
quantity :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO CLong
quantity skMutablePayment  =
  sendMsg skMutablePayment (mkSelector "quantity") retCLong []

-- | @- setQuantity:@
setQuantity :: IsSKMutablePayment skMutablePayment => skMutablePayment -> CLong -> IO ()
setQuantity skMutablePayment  value =
  sendMsg skMutablePayment (mkSelector "setQuantity:") retVoid [argCLong (fromIntegral value)]

-- | @- requestData@
requestData :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO (Id NSData)
requestData skMutablePayment  =
  sendMsg skMutablePayment (mkSelector "requestData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestData:@
setRequestData :: (IsSKMutablePayment skMutablePayment, IsNSData value) => skMutablePayment -> value -> IO ()
setRequestData skMutablePayment  value =
withObjCPtr value $ \raw_value ->
    sendMsg skMutablePayment (mkSelector "setRequestData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandbox :: IsSKMutablePayment skMutablePayment => skMutablePayment -> IO Bool
simulatesAskToBuyInSandbox skMutablePayment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skMutablePayment (mkSelector "simulatesAskToBuyInSandbox") retCULong []

-- | @- setSimulatesAskToBuyInSandbox:@
setSimulatesAskToBuyInSandbox :: IsSKMutablePayment skMutablePayment => skMutablePayment -> Bool -> IO ()
setSimulatesAskToBuyInSandbox skMutablePayment  value =
  sendMsg skMutablePayment (mkSelector "setSimulatesAskToBuyInSandbox:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @applicationUsername@
applicationUsernameSelector :: Selector
applicationUsernameSelector = mkSelector "applicationUsername"

-- | @Selector@ for @setApplicationUsername:@
setApplicationUsernameSelector :: Selector
setApplicationUsernameSelector = mkSelector "setApplicationUsername:"

-- | @Selector@ for @paymentDiscount@
paymentDiscountSelector :: Selector
paymentDiscountSelector = mkSelector "paymentDiscount"

-- | @Selector@ for @setPaymentDiscount:@
setPaymentDiscountSelector :: Selector
setPaymentDiscountSelector = mkSelector "setPaymentDiscount:"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @setProductIdentifier:@
setProductIdentifierSelector :: Selector
setProductIdentifierSelector = mkSelector "setProductIdentifier:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector
setQuantitySelector = mkSelector "setQuantity:"

-- | @Selector@ for @requestData@
requestDataSelector :: Selector
requestDataSelector = mkSelector "requestData"

-- | @Selector@ for @setRequestData:@
setRequestDataSelector :: Selector
setRequestDataSelector = mkSelector "setRequestData:"

-- | @Selector@ for @simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandboxSelector :: Selector
simulatesAskToBuyInSandboxSelector = mkSelector "simulatesAskToBuyInSandbox"

-- | @Selector@ for @setSimulatesAskToBuyInSandbox:@
setSimulatesAskToBuyInSandboxSelector :: Selector
setSimulatesAskToBuyInSandboxSelector = mkSelector "setSimulatesAskToBuyInSandbox:"

