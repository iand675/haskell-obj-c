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
  , newSelector
  , initSelector
  , initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector
  , orderTypeIdentifierSelector
  , setOrderTypeIdentifierSelector
  , orderIdentifierSelector
  , setOrderIdentifierSelector
  , webServiceURLSelector
  , setWebServiceURLSelector
  , authenticationTokenSelector
  , setAuthenticationTokenSelector


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

-- | @+ new@
new :: IO (Id PKPaymentOrderDetails)
new  =
  do
    cls' <- getRequiredClass "PKPaymentOrderDetails"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id PKPaymentOrderDetails)
init_ pkPaymentOrderDetails  =
  sendMsg pkPaymentOrderDetails (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:@
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationToken :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString orderTypeIdentifier, IsNSString orderIdentifier, IsNSURL webServiceURL, IsNSString authenticationToken) => pkPaymentOrderDetails -> orderTypeIdentifier -> orderIdentifier -> webServiceURL -> authenticationToken -> IO (Id PKPaymentOrderDetails)
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationToken pkPaymentOrderDetails  orderTypeIdentifier orderIdentifier webServiceURL authenticationToken =
withObjCPtr orderTypeIdentifier $ \raw_orderTypeIdentifier ->
  withObjCPtr orderIdentifier $ \raw_orderIdentifier ->
    withObjCPtr webServiceURL $ \raw_webServiceURL ->
      withObjCPtr authenticationToken $ \raw_authenticationToken ->
          sendMsg pkPaymentOrderDetails (mkSelector "initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:") (retPtr retVoid) [argPtr (castPtr raw_orderTypeIdentifier :: Ptr ()), argPtr (castPtr raw_orderIdentifier :: Ptr ()), argPtr (castPtr raw_webServiceURL :: Ptr ()), argPtr (castPtr raw_authenticationToken :: Ptr ())] >>= ownedObject . castPtr

-- | @- orderTypeIdentifier@
orderTypeIdentifier :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
orderTypeIdentifier pkPaymentOrderDetails  =
  sendMsg pkPaymentOrderDetails (mkSelector "orderTypeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrderTypeIdentifier:@
setOrderTypeIdentifier :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setOrderTypeIdentifier pkPaymentOrderDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentOrderDetails (mkSelector "setOrderTypeIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- orderIdentifier@
orderIdentifier :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
orderIdentifier pkPaymentOrderDetails  =
  sendMsg pkPaymentOrderDetails (mkSelector "orderIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrderIdentifier:@
setOrderIdentifier :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setOrderIdentifier pkPaymentOrderDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentOrderDetails (mkSelector "setOrderIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- webServiceURL@
webServiceURL :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSURL)
webServiceURL pkPaymentOrderDetails  =
  sendMsg pkPaymentOrderDetails (mkSelector "webServiceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebServiceURL:@
setWebServiceURL :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSURL value) => pkPaymentOrderDetails -> value -> IO ()
setWebServiceURL pkPaymentOrderDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentOrderDetails (mkSelector "setWebServiceURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authenticationToken@
authenticationToken :: IsPKPaymentOrderDetails pkPaymentOrderDetails => pkPaymentOrderDetails -> IO (Id NSString)
authenticationToken pkPaymentOrderDetails  =
  sendMsg pkPaymentOrderDetails (mkSelector "authenticationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthenticationToken:@
setAuthenticationToken :: (IsPKPaymentOrderDetails pkPaymentOrderDetails, IsNSString value) => pkPaymentOrderDetails -> value -> IO ()
setAuthenticationToken pkPaymentOrderDetails  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkPaymentOrderDetails (mkSelector "setAuthenticationToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:@
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector :: Selector
initWithOrderTypeIdentifier_orderIdentifier_webServiceURL_authenticationTokenSelector = mkSelector "initWithOrderTypeIdentifier:orderIdentifier:webServiceURL:authenticationToken:"

-- | @Selector@ for @orderTypeIdentifier@
orderTypeIdentifierSelector :: Selector
orderTypeIdentifierSelector = mkSelector "orderTypeIdentifier"

-- | @Selector@ for @setOrderTypeIdentifier:@
setOrderTypeIdentifierSelector :: Selector
setOrderTypeIdentifierSelector = mkSelector "setOrderTypeIdentifier:"

-- | @Selector@ for @orderIdentifier@
orderIdentifierSelector :: Selector
orderIdentifierSelector = mkSelector "orderIdentifier"

-- | @Selector@ for @setOrderIdentifier:@
setOrderIdentifierSelector :: Selector
setOrderIdentifierSelector = mkSelector "setOrderIdentifier:"

-- | @Selector@ for @webServiceURL@
webServiceURLSelector :: Selector
webServiceURLSelector = mkSelector "webServiceURL"

-- | @Selector@ for @setWebServiceURL:@
setWebServiceURLSelector :: Selector
setWebServiceURLSelector = mkSelector "setWebServiceURL:"

-- | @Selector@ for @authenticationToken@
authenticationTokenSelector :: Selector
authenticationTokenSelector = mkSelector "authenticationToken"

-- | @Selector@ for @setAuthenticationToken:@
setAuthenticationTokenSelector :: Selector
setAuthenticationTokenSelector = mkSelector "setAuthenticationToken:"

