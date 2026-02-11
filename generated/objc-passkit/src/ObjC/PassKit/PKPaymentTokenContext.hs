{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentTokenContext@.
module ObjC.PassKit.PKPaymentTokenContext
  ( PKPaymentTokenContext
  , IsPKPaymentTokenContext(..)
  , init_
  , initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amount
  , merchantIdentifier
  , setMerchantIdentifier
  , externalIdentifier
  , setExternalIdentifier
  , merchantName
  , setMerchantName
  , merchantDomain
  , setMerchantDomain
  , amount
  , setAmount
  , initSelector
  , initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector
  , merchantIdentifierSelector
  , setMerchantIdentifierSelector
  , externalIdentifierSelector
  , setExternalIdentifierSelector
  , merchantNameSelector
  , setMerchantNameSelector
  , merchantDomainSelector
  , setMerchantDomainSelector
  , amountSelector
  , setAmountSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id PKPaymentTokenContext)
init_ pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:@
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amount :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString merchantIdentifier, IsNSString externalIdentifier, IsNSString merchantName, IsNSString merchantDomain, IsNSDecimalNumber amount) => pkPaymentTokenContext -> merchantIdentifier -> externalIdentifier -> merchantName -> merchantDomain -> amount -> IO (Id PKPaymentTokenContext)
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amount pkPaymentTokenContext  merchantIdentifier externalIdentifier merchantName merchantDomain amount =
withObjCPtr merchantIdentifier $ \raw_merchantIdentifier ->
  withObjCPtr externalIdentifier $ \raw_externalIdentifier ->
    withObjCPtr merchantName $ \raw_merchantName ->
      withObjCPtr merchantDomain $ \raw_merchantDomain ->
        withObjCPtr amount $ \raw_amount ->
            sendMsg pkPaymentTokenContext (mkSelector "initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:") (retPtr retVoid) [argPtr (castPtr raw_merchantIdentifier :: Ptr ()), argPtr (castPtr raw_externalIdentifier :: Ptr ()), argPtr (castPtr raw_merchantName :: Ptr ()), argPtr (castPtr raw_merchantDomain :: Ptr ()), argPtr (castPtr raw_amount :: Ptr ())] >>= ownedObject . castPtr

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantIdentifier pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "merchantIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantIdentifier pkPaymentTokenContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentTokenContext (mkSelector "setMerchantIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIdentifier@
externalIdentifier :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
externalIdentifier pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "externalIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIdentifier:@
setExternalIdentifier :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setExternalIdentifier pkPaymentTokenContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentTokenContext (mkSelector "setExternalIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- merchantName@
merchantName :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantName pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "merchantName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMerchantName:@
setMerchantName :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantName pkPaymentTokenContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentTokenContext (mkSelector "setMerchantName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- merchantDomain@
merchantDomain :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantDomain pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "merchantDomain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMerchantDomain:@
setMerchantDomain :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantDomain pkPaymentTokenContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentTokenContext (mkSelector "setMerchantDomain:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- amount@
amount :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSDecimalNumber)
amount pkPaymentTokenContext  =
  sendMsg pkPaymentTokenContext (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAmount:@
setAmount :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSDecimalNumber value) => pkPaymentTokenContext -> value -> IO ()
setAmount pkPaymentTokenContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentTokenContext (mkSelector "setAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:@
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector :: Selector
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector = mkSelector "initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @externalIdentifier@
externalIdentifierSelector :: Selector
externalIdentifierSelector = mkSelector "externalIdentifier"

-- | @Selector@ for @setExternalIdentifier:@
setExternalIdentifierSelector :: Selector
setExternalIdentifierSelector = mkSelector "setExternalIdentifier:"

-- | @Selector@ for @merchantName@
merchantNameSelector :: Selector
merchantNameSelector = mkSelector "merchantName"

-- | @Selector@ for @setMerchantName:@
setMerchantNameSelector :: Selector
setMerchantNameSelector = mkSelector "setMerchantName:"

-- | @Selector@ for @merchantDomain@
merchantDomainSelector :: Selector
merchantDomainSelector = mkSelector "merchantDomain"

-- | @Selector@ for @setMerchantDomain:@
setMerchantDomainSelector :: Selector
setMerchantDomainSelector = mkSelector "setMerchantDomain:"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector
setAmountSelector = mkSelector "setAmount:"

