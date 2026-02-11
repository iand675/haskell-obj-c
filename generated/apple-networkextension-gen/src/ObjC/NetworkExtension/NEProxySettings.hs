{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEProxySettings
--
-- The NEProxySettings class declares the programmatic interface for an object that contains proxy settings.
--
-- NEProxySettings is used in the context of a Network Extension configuration to specify the proxy that should be used for network traffic when the Network Extension is active.
--
-- Generated bindings for @NEProxySettings@.
module ObjC.NetworkExtension.NEProxySettings
  ( NEProxySettings
  , IsNEProxySettings(..)
  , autoProxyConfigurationEnabled
  , setAutoProxyConfigurationEnabled
  , proxyAutoConfigurationURL
  , setProxyAutoConfigurationURL
  , proxyAutoConfigurationJavaScript
  , setProxyAutoConfigurationJavaScript
  , httpEnabled
  , setHTTPEnabled
  , httpServer
  , setHTTPServer
  , httpsEnabled
  , setHTTPSEnabled
  , httpsServer
  , setHTTPSServer
  , excludeSimpleHostnames
  , setExcludeSimpleHostnames
  , exceptionList
  , setExceptionList
  , matchDomains
  , setMatchDomains
  , autoProxyConfigurationEnabledSelector
  , setAutoProxyConfigurationEnabledSelector
  , proxyAutoConfigurationURLSelector
  , setProxyAutoConfigurationURLSelector
  , proxyAutoConfigurationJavaScriptSelector
  , setProxyAutoConfigurationJavaScriptSelector
  , httpEnabledSelector
  , setHTTPEnabledSelector
  , httpServerSelector
  , setHTTPServerSelector
  , httpsEnabledSelector
  , setHTTPSEnabledSelector
  , httpsServerSelector
  , setHTTPSServerSelector
  , excludeSimpleHostnamesSelector
  , setExcludeSimpleHostnamesSelector
  , exceptionListSelector
  , setExceptionListSelector
  , matchDomainsSelector
  , setMatchDomainsSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | autoProxyConfigurationEnabled
--
-- A boolean indicating if proxy auto-configuration is enabled.
--
-- ObjC selector: @- autoProxyConfigurationEnabled@
autoProxyConfigurationEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
autoProxyConfigurationEnabled neProxySettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "autoProxyConfigurationEnabled") retCULong []

-- | autoProxyConfigurationEnabled
--
-- A boolean indicating if proxy auto-configuration is enabled.
--
-- ObjC selector: @- setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setAutoProxyConfigurationEnabled neProxySettings  value =
    sendMsg neProxySettings (mkSelector "setAutoProxyConfigurationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | proxyAutoConfigurationURL
--
-- A URL specifying where the PAC script is located.
--
-- ObjC selector: @- proxyAutoConfigurationURL@
proxyAutoConfigurationURL :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSURL)
proxyAutoConfigurationURL neProxySettings  =
    sendMsg neProxySettings (mkSelector "proxyAutoConfigurationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | proxyAutoConfigurationURL
--
-- A URL specifying where the PAC script is located.
--
-- ObjC selector: @- setProxyAutoConfigurationURL:@
setProxyAutoConfigurationURL :: (IsNEProxySettings neProxySettings, IsNSURL value) => neProxySettings -> value -> IO ()
setProxyAutoConfigurationURL neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setProxyAutoConfigurationURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | proxyAutoConfigurationJavaScript
--
-- A string containing the PAC JavaScript source code.
--
-- ObjC selector: @- proxyAutoConfigurationJavaScript@
proxyAutoConfigurationJavaScript :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSString)
proxyAutoConfigurationJavaScript neProxySettings  =
    sendMsg neProxySettings (mkSelector "proxyAutoConfigurationJavaScript") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | proxyAutoConfigurationJavaScript
--
-- A string containing the PAC JavaScript source code.
--
-- ObjC selector: @- setProxyAutoConfigurationJavaScript:@
setProxyAutoConfigurationJavaScript :: (IsNEProxySettings neProxySettings, IsNSString value) => neProxySettings -> value -> IO ()
setProxyAutoConfigurationJavaScript neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setProxyAutoConfigurationJavaScript:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- HTTPEnabled@
httpEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpEnabled neProxySettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "HTTPEnabled") retCULong []

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- setHTTPEnabled:@
setHTTPEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPEnabled neProxySettings  value =
    sendMsg neProxySettings (mkSelector "setHTTPEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | HTTPServer
--
-- A NEProxyServer object containing the HTTP proxy server settings.
--
-- ObjC selector: @- HTTPServer@
httpServer :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NEProxyServer)
httpServer neProxySettings  =
    sendMsg neProxySettings (mkSelector "HTTPServer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTPServer
--
-- A NEProxyServer object containing the HTTP proxy server settings.
--
-- ObjC selector: @- setHTTPServer:@
setHTTPServer :: (IsNEProxySettings neProxySettings, IsNEProxyServer value) => neProxySettings -> value -> IO ()
setHTTPServer neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setHTTPServer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- HTTPSEnabled@
httpsEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpsEnabled neProxySettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "HTTPSEnabled") retCULong []

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- setHTTPSEnabled:@
setHTTPSEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPSEnabled neProxySettings  value =
    sendMsg neProxySettings (mkSelector "setHTTPSEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | HTTPSServer
--
-- A NEProxyServer object containing the HTTPS proxy server settings.
--
-- ObjC selector: @- HTTPSServer@
httpsServer :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NEProxyServer)
httpsServer neProxySettings  =
    sendMsg neProxySettings (mkSelector "HTTPSServer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTPSServer
--
-- A NEProxyServer object containing the HTTPS proxy server settings.
--
-- ObjC selector: @- setHTTPSServer:@
setHTTPSServer :: (IsNEProxySettings neProxySettings, IsNEProxyServer value) => neProxySettings -> value -> IO ()
setHTTPSServer neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setHTTPSServer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- excludeSimpleHostnames@
excludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
excludeSimpleHostnames neProxySettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "excludeSimpleHostnames") retCULong []

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- setExcludeSimpleHostnames:@
setExcludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setExcludeSimpleHostnames neProxySettings  value =
    sendMsg neProxySettings (mkSelector "setExcludeSimpleHostnames:") retVoid [argCULong (if value then 1 else 0)]

-- | exceptionList
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will not be used for the connection.
--
-- ObjC selector: @- exceptionList@
exceptionList :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSArray)
exceptionList neProxySettings  =
    sendMsg neProxySettings (mkSelector "exceptionList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionList
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will not be used for the connection.
--
-- ObjC selector: @- setExceptionList:@
setExceptionList :: (IsNEProxySettings neProxySettings, IsNSArray value) => neProxySettings -> value -> IO ()
setExceptionList neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setExceptionList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchDomains
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will be used for the connection. Otherwise the proxy settings will not be used. If this property is nil then all connections to which the Network Extension applies will use the proxy settings.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSArray)
matchDomains neProxySettings  =
    sendMsg neProxySettings (mkSelector "matchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchDomains
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will be used for the connection. Otherwise the proxy settings will not be used. If this property is nil then all connections to which the Network Extension applies will use the proxy settings.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNEProxySettings neProxySettings, IsNSArray value) => neProxySettings -> value -> IO ()
setMatchDomains neProxySettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neProxySettings (mkSelector "setMatchDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @autoProxyConfigurationEnabled@
autoProxyConfigurationEnabledSelector :: Selector
autoProxyConfigurationEnabledSelector = mkSelector "autoProxyConfigurationEnabled"

-- | @Selector@ for @setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabledSelector :: Selector
setAutoProxyConfigurationEnabledSelector = mkSelector "setAutoProxyConfigurationEnabled:"

-- | @Selector@ for @proxyAutoConfigurationURL@
proxyAutoConfigurationURLSelector :: Selector
proxyAutoConfigurationURLSelector = mkSelector "proxyAutoConfigurationURL"

-- | @Selector@ for @setProxyAutoConfigurationURL:@
setProxyAutoConfigurationURLSelector :: Selector
setProxyAutoConfigurationURLSelector = mkSelector "setProxyAutoConfigurationURL:"

-- | @Selector@ for @proxyAutoConfigurationJavaScript@
proxyAutoConfigurationJavaScriptSelector :: Selector
proxyAutoConfigurationJavaScriptSelector = mkSelector "proxyAutoConfigurationJavaScript"

-- | @Selector@ for @setProxyAutoConfigurationJavaScript:@
setProxyAutoConfigurationJavaScriptSelector :: Selector
setProxyAutoConfigurationJavaScriptSelector = mkSelector "setProxyAutoConfigurationJavaScript:"

-- | @Selector@ for @HTTPEnabled@
httpEnabledSelector :: Selector
httpEnabledSelector = mkSelector "HTTPEnabled"

-- | @Selector@ for @setHTTPEnabled:@
setHTTPEnabledSelector :: Selector
setHTTPEnabledSelector = mkSelector "setHTTPEnabled:"

-- | @Selector@ for @HTTPServer@
httpServerSelector :: Selector
httpServerSelector = mkSelector "HTTPServer"

-- | @Selector@ for @setHTTPServer:@
setHTTPServerSelector :: Selector
setHTTPServerSelector = mkSelector "setHTTPServer:"

-- | @Selector@ for @HTTPSEnabled@
httpsEnabledSelector :: Selector
httpsEnabledSelector = mkSelector "HTTPSEnabled"

-- | @Selector@ for @setHTTPSEnabled:@
setHTTPSEnabledSelector :: Selector
setHTTPSEnabledSelector = mkSelector "setHTTPSEnabled:"

-- | @Selector@ for @HTTPSServer@
httpsServerSelector :: Selector
httpsServerSelector = mkSelector "HTTPSServer"

-- | @Selector@ for @setHTTPSServer:@
setHTTPSServerSelector :: Selector
setHTTPSServerSelector = mkSelector "setHTTPSServer:"

-- | @Selector@ for @excludeSimpleHostnames@
excludeSimpleHostnamesSelector :: Selector
excludeSimpleHostnamesSelector = mkSelector "excludeSimpleHostnames"

-- | @Selector@ for @setExcludeSimpleHostnames:@
setExcludeSimpleHostnamesSelector :: Selector
setExcludeSimpleHostnamesSelector = mkSelector "setExcludeSimpleHostnames:"

-- | @Selector@ for @exceptionList@
exceptionListSelector :: Selector
exceptionListSelector = mkSelector "exceptionList"

-- | @Selector@ for @setExceptionList:@
setExceptionListSelector :: Selector
setExceptionListSelector = mkSelector "setExceptionList:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector
setMatchDomainsSelector = mkSelector "setMatchDomains:"

