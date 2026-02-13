{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentOrderDetails@.
module ObjC.PassKit.PKPaymentOrderDetails
  ( PKPaymentOrderDetails
  , IsPKPaymentOrderDetails(..)
  , new
  , init_
  , initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationToken
  , orderTypeIdentifier
  , setOrderTypeIdentifier
  , orderIdentifier
  , setOrderIdentifier
  , webServiceURL
  , setWebServiceURL
  , authenticationToken
  , setAuthenticationToken
  , authenticationTokenSelector
  , initSelector
  , initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector
  , newSelector
  , orderIdentifierSelector
  , orderTypeIdentifierSelector
  , setAuthenticationTokenSelector
  , setOrderIdentifierSelector
  , setOrderTypeIdentifierSelector
  , setWebServiceURLSelector
  , webServiceURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PKPaymentOrderDetails)
new  =
  do
    cls' <- getRequiredClass "PKPaymentOrderDetails"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id PKPaymentOrderDetails)
init_ pkPaymentOrderDetails =
  sendOwnedMessage pkPaymentOrderDetails initSelector

-- | @- initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:@
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationToken :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString orderTypeIdentifier, IsNSString orderIdentifier, IsNSURL webServiceURL, IsNSString authenticationToken) => pkPaymentOrderDetails -> orderTypeIdentifier -> orderIdentifier -> webServiceURL -> authenticationToken -> IO (Id PKPaymentOrderDetails)
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationToken pkPaymentOrderDetails orderTypeIdentifier orderIdentifier webServiceURL authenticationToken =
  sendOwnedMessage pkPaymentOrderDetails initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector (toNSString orderTypeIdentifier) (toNSString orderIdentifier) (toNSURL webServiceURL) (toNSString authenticationToken)

-- | @- orderTypeIdentifier@
orderTypeIdentifier :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
orderTypeIdentifier pkPaymentOrderDetails =
  sendMessage pkPaymentOrderDetails orderTypeIdentifierSelector

-- | @- setOrderTypeIdentifier:@
setOrderTypeIdentifier :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setOrderTypeIdentifier pkPaymentOrderDetails value =
  sendMessage pkPaymentOrderDetails setOrderTypeIdentifierSelector (toNSString value)

-- | @- orderIdentifier@
orderIdentifier :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
orderIdentifier pkPaymentOrderDetails =
  sendMessage pkPaymentOrderDetails orderIdentifierSelector

-- | @- setOrderIdentifier:@
setOrderIdentifier :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setOrderIdentifier pkPaymentOrderDetails value =
  sendMessage pkPaymentOrderDetails setOrderIdentifierSelector (toNSString value)

-- | @- webServiceURL@
webServiceURL :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSURL)
webServiceURL pkPaymentOrderDetails =
  sendMessage pkPaymentOrderDetails webServiceURLSelector

-- | @- setWebServiceURL:@
setWebServiceURL :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSURL value) => pkPaymentOrderDetails -> value -> IO ()
setWebServiceURL pkPaymentOrderDetails value =
  sendMessage pkPaymentOrderDetails setWebServiceURLSelector (toNSURL value)

-- | @- authenticationToken@
authenticationToken :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
authenticationToken pkPaymentOrderDetails =
  sendMessage pkPaymentOrderDetails authenticationTokenSelector

-- | @- setAuthenticationToken:@
setAuthenticationToken :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setAuthenticationToken pkPaymentOrderDetails value =
  sendMessage pkPaymentOrderDetails setAuthenticationTokenSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKPaymentOrderDetails)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKPaymentOrderDetails)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:@
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector :: Selector '[Id NSString, Id NSString, Id NSURL, Id NSString] (Id PKPaymentOrderDetails)
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector = mkSelector "initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:"

-- | @Selector@ for @orderTypeIdentifier@
orderTypeIdentifierSelector :: Selector '[] (Id NSString)
orderTypeIdentifierSelector = mkSelector "orderTypeIdentifier"

-- | @Selector@ for @setOrderTypeIdentifier:@
setOrderTypeIdentifierSelector :: Selector '[Id NSString] ()
setOrderTypeIdentifierSelector = mkSelector "setOrderTypeIdentifier:"

-- | @Selector@ for @orderIdentifier@
orderIdentifierSelector :: Selector '[] (Id NSString)
orderIdentifierSelector = mkSelector "orderIdentifier"

-- | @Selector@ for @setOrderIdentifier:@
setOrderIdentifierSelector :: Selector '[Id NSString] ()
setOrderIdentifierSelector = mkSelector "setOrderIdentifier:"

-- | @Selector@ for @webServiceURL@
webServiceURLSelector :: Selector '[] (Id NSURL)
webServiceURLSelector = mkSelector "webServiceURL"

-- | @Selector@ for @setWebServiceURL:@
setWebServiceURLSelector :: Selector '[Id NSURL] ()
setWebServiceURLSelector = mkSelector "setWebServiceURL:"

-- | @Selector@ for @authenticationToken@
authenticationTokenSelector :: Selector '[] (Id NSString)
authenticationTokenSelector = mkSelector "authenticationToken"

-- | @Selector@ for @setAuthenticationToken:@
setAuthenticationTokenSelector :: Selector '[Id NSString] ()
setAuthenticationTokenSelector = mkSelector "setAuthenticationToken:"

