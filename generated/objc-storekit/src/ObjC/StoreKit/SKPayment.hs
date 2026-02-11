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
  , paymentWithProductSelector
  , paymentWithProductIdentifierSelector
  , productIdentifierSelector
  , requestDataSelector
  , quantitySelector
  , applicationUsernameSelector
  , simulatesAskToBuyInSandboxSelector
  , paymentDiscountSelector


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

-- | @+ paymentWithProduct:@
paymentWithProduct :: IsSKProduct product_ => product_ -> IO (Id SKPayment)
paymentWithProduct product_ =
  do
    cls' <- getRequiredClass "SKPayment"
    withObjCPtr product_ $ \raw_product_ ->
      sendClassMsg cls' (mkSelector "paymentWithProduct:") (retPtr retVoid) [argPtr (castPtr raw_product_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentWithProductIdentifier:@
paymentWithProductIdentifier :: IsNSString identifier => identifier -> IO RawId
paymentWithProductIdentifier identifier =
  do
    cls' <- getRequiredClass "SKPayment"
    withObjCPtr identifier $ \raw_identifier ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "paymentWithProductIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- productIdentifier@
productIdentifier :: IsSKPayment skPayment => skPayment -> IO (Id NSString)
productIdentifier skPayment  =
  sendMsg skPayment (mkSelector "productIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestData@
requestData :: IsSKPayment skPayment => skPayment -> IO (Id NSData)
requestData skPayment  =
  sendMsg skPayment (mkSelector "requestData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quantity@
quantity :: IsSKPayment skPayment => skPayment -> IO CLong
quantity skPayment  =
  sendMsg skPayment (mkSelector "quantity") retCLong []

-- | @- applicationUsername@
applicationUsername :: IsSKPayment skPayment => skPayment -> IO (Id NSString)
applicationUsername skPayment  =
  sendMsg skPayment (mkSelector "applicationUsername") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandbox :: IsSKPayment skPayment => skPayment -> IO Bool
simulatesAskToBuyInSandbox skPayment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPayment (mkSelector "simulatesAskToBuyInSandbox") retCULong []

-- | @- paymentDiscount@
paymentDiscount :: IsSKPayment skPayment => skPayment -> IO (Id SKPaymentDiscount)
paymentDiscount skPayment  =
  sendMsg skPayment (mkSelector "paymentDiscount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paymentWithProduct:@
paymentWithProductSelector :: Selector
paymentWithProductSelector = mkSelector "paymentWithProduct:"

-- | @Selector@ for @paymentWithProductIdentifier:@
paymentWithProductIdentifierSelector :: Selector
paymentWithProductIdentifierSelector = mkSelector "paymentWithProductIdentifier:"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @requestData@
requestDataSelector :: Selector
requestDataSelector = mkSelector "requestData"

-- | @Selector@ for @quantity@
quantitySelector :: Selector
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @applicationUsername@
applicationUsernameSelector :: Selector
applicationUsernameSelector = mkSelector "applicationUsername"

-- | @Selector@ for @simulatesAskToBuyInSandbox@
simulatesAskToBuyInSandboxSelector :: Selector
simulatesAskToBuyInSandboxSelector = mkSelector "simulatesAskToBuyInSandbox"

-- | @Selector@ for @paymentDiscount@
paymentDiscountSelector :: Selector
paymentDiscountSelector = mkSelector "paymentDiscount"

