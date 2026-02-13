{-# LANGUAGE DataKinds #-}
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
  , amountSelector
  , externalIdentifierSelector
  , initSelector
  , initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector
  , merchantDomainSelector
  , merchantIdentifierSelector
  , merchantNameSelector
  , setAmountSelector
  , setExternalIdentifierSelector
  , setMerchantDomainSelector
  , setMerchantIdentifierSelector
  , setMerchantNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id PKPaymentTokenContext)
init_ pkPaymentTokenContext =
  sendOwnedMessage pkPaymentTokenContext initSelector

-- | @- initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:@
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amount :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString merchantIdentifier, IsNSString externalIdentifier, IsNSString merchantName, IsNSString merchantDomain, IsNSDecimalNumber amount) => pkPaymentTokenContext -> merchantIdentifier -> externalIdentifier -> merchantName -> merchantDomain -> amount -> IO (Id PKPaymentTokenContext)
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amount pkPaymentTokenContext merchantIdentifier externalIdentifier merchantName merchantDomain amount =
  sendOwnedMessage pkPaymentTokenContext initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector (toNSString merchantIdentifier) (toNSString externalIdentifier) (toNSString merchantName) (toNSString merchantDomain) (toNSDecimalNumber amount)

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantIdentifier pkPaymentTokenContext =
  sendMessage pkPaymentTokenContext merchantIdentifierSelector

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantIdentifier pkPaymentTokenContext value =
  sendMessage pkPaymentTokenContext setMerchantIdentifierSelector (toNSString value)

-- | @- externalIdentifier@
externalIdentifier :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
externalIdentifier pkPaymentTokenContext =
  sendMessage pkPaymentTokenContext externalIdentifierSelector

-- | @- setExternalIdentifier:@
setExternalIdentifier :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setExternalIdentifier pkPaymentTokenContext value =
  sendMessage pkPaymentTokenContext setExternalIdentifierSelector (toNSString value)

-- | @- merchantName@
merchantName :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantName pkPaymentTokenContext =
  sendMessage pkPaymentTokenContext merchantNameSelector

-- | @- setMerchantName:@
setMerchantName :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantName pkPaymentTokenContext value =
  sendMessage pkPaymentTokenContext setMerchantNameSelector (toNSString value)

-- | @- merchantDomain@
merchantDomain :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSString)
merchantDomain pkPaymentTokenContext =
  sendMessage pkPaymentTokenContext merchantDomainSelector

-- | @- setMerchantDomain:@
setMerchantDomain :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSString value) => pkPaymentTokenContext -> value -> IO ()
setMerchantDomain pkPaymentTokenContext value =
  sendMessage pkPaymentTokenContext setMerchantDomainSelector (toNSString value)

-- | @- amount@
amount :: IsPKPaymentTokenContext pkPaymentTokenContext => pkPaymentTokenContext -> IO (Id NSDecimalNumber)
amount pkPaymentTokenContext =
  sendMessage pkPaymentTokenContext amountSelector

-- | @- setAmount:@
setAmount :: (IsPKPaymentTokenContext pkPaymentTokenContext, IsNSDecimalNumber value) => pkPaymentTokenContext -> value -> IO ()
setAmount pkPaymentTokenContext value =
  sendMessage pkPaymentTokenContext setAmountSelector (toNSDecimalNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKPaymentTokenContext)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:@
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSDecimalNumber] (Id PKPaymentTokenContext)
initWithMerchantIdentifier_externalIdentifier_merchantName_merchantDomain_amountSelector = mkSelector "initWithMerchantIdentifier:externalIdentifier:merchantName:merchantDomain:amount:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector '[] (Id NSString)
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector '[Id NSString] ()
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @externalIdentifier@
externalIdentifierSelector :: Selector '[] (Id NSString)
externalIdentifierSelector = mkSelector "externalIdentifier"

-- | @Selector@ for @setExternalIdentifier:@
setExternalIdentifierSelector :: Selector '[Id NSString] ()
setExternalIdentifierSelector = mkSelector "setExternalIdentifier:"

-- | @Selector@ for @merchantName@
merchantNameSelector :: Selector '[] (Id NSString)
merchantNameSelector = mkSelector "merchantName"

-- | @Selector@ for @setMerchantName:@
setMerchantNameSelector :: Selector '[Id NSString] ()
setMerchantNameSelector = mkSelector "setMerchantName:"

-- | @Selector@ for @merchantDomain@
merchantDomainSelector :: Selector '[] (Id NSString)
merchantDomainSelector = mkSelector "merchantDomain"

-- | @Selector@ for @setMerchantDomain:@
setMerchantDomainSelector :: Selector '[Id NSString] ()
setMerchantDomainSelector = mkSelector "setMerchantDomain:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSDecimalNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector '[Id NSDecimalNumber] ()
setAmountSelector = mkSelector "setAmount:"

