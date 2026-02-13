{-# LANGUAGE DataKinds #-}
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
  , exceptionListSelector
  , excludeSimpleHostnamesSelector
  , httpEnabledSelector
  , httpServerSelector
  , httpsEnabledSelector
  , httpsServerSelector
  , matchDomainsSelector
  , proxyAutoConfigurationJavaScriptSelector
  , proxyAutoConfigurationURLSelector
  , setAutoProxyConfigurationEnabledSelector
  , setExceptionListSelector
  , setExcludeSimpleHostnamesSelector
  , setHTTPEnabledSelector
  , setHTTPSEnabledSelector
  , setHTTPSServerSelector
  , setHTTPServerSelector
  , setMatchDomainsSelector
  , setProxyAutoConfigurationJavaScriptSelector
  , setProxyAutoConfigurationURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
autoProxyConfigurationEnabled neProxySettings =
  sendMessage neProxySettings autoProxyConfigurationEnabledSelector

-- | autoProxyConfigurationEnabled
--
-- A boolean indicating if proxy auto-configuration is enabled.
--
-- ObjC selector: @- setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setAutoProxyConfigurationEnabled neProxySettings value =
  sendMessage neProxySettings setAutoProxyConfigurationEnabledSelector value

-- | proxyAutoConfigurationURL
--
-- A URL specifying where the PAC script is located.
--
-- ObjC selector: @- proxyAutoConfigurationURL@
proxyAutoConfigurationURL :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSURL)
proxyAutoConfigurationURL neProxySettings =
  sendMessage neProxySettings proxyAutoConfigurationURLSelector

-- | proxyAutoConfigurationURL
--
-- A URL specifying where the PAC script is located.
--
-- ObjC selector: @- setProxyAutoConfigurationURL:@
setProxyAutoConfigurationURL :: (IsNEProxySettings neProxySettings, IsNSURL value) => neProxySettings -> value -> IO ()
setProxyAutoConfigurationURL neProxySettings value =
  sendMessage neProxySettings setProxyAutoConfigurationURLSelector (toNSURL value)

-- | proxyAutoConfigurationJavaScript
--
-- A string containing the PAC JavaScript source code.
--
-- ObjC selector: @- proxyAutoConfigurationJavaScript@
proxyAutoConfigurationJavaScript :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSString)
proxyAutoConfigurationJavaScript neProxySettings =
  sendMessage neProxySettings proxyAutoConfigurationJavaScriptSelector

-- | proxyAutoConfigurationJavaScript
--
-- A string containing the PAC JavaScript source code.
--
-- ObjC selector: @- setProxyAutoConfigurationJavaScript:@
setProxyAutoConfigurationJavaScript :: (IsNEProxySettings neProxySettings, IsNSString value) => neProxySettings -> value -> IO ()
setProxyAutoConfigurationJavaScript neProxySettings value =
  sendMessage neProxySettings setProxyAutoConfigurationJavaScriptSelector (toNSString value)

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- HTTPEnabled@
httpEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpEnabled neProxySettings =
  sendMessage neProxySettings httpEnabledSelector

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- setHTTPEnabled:@
setHTTPEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPEnabled neProxySettings value =
  sendMessage neProxySettings setHTTPEnabledSelector value

-- | HTTPServer
--
-- A NEProxyServer object containing the HTTP proxy server settings.
--
-- ObjC selector: @- HTTPServer@
httpServer :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NEProxyServer)
httpServer neProxySettings =
  sendMessage neProxySettings httpServerSelector

-- | HTTPServer
--
-- A NEProxyServer object containing the HTTP proxy server settings.
--
-- ObjC selector: @- setHTTPServer:@
setHTTPServer :: (IsNEProxySettings neProxySettings, IsNEProxyServer value) => neProxySettings -> value -> IO ()
setHTTPServer neProxySettings value =
  sendMessage neProxySettings setHTTPServerSelector (toNEProxyServer value)

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- HTTPSEnabled@
httpsEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpsEnabled neProxySettings =
  sendMessage neProxySettings httpsEnabledSelector

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- setHTTPSEnabled:@
setHTTPSEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPSEnabled neProxySettings value =
  sendMessage neProxySettings setHTTPSEnabledSelector value

-- | HTTPSServer
--
-- A NEProxyServer object containing the HTTPS proxy server settings.
--
-- ObjC selector: @- HTTPSServer@
httpsServer :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NEProxyServer)
httpsServer neProxySettings =
  sendMessage neProxySettings httpsServerSelector

-- | HTTPSServer
--
-- A NEProxyServer object containing the HTTPS proxy server settings.
--
-- ObjC selector: @- setHTTPSServer:@
setHTTPSServer :: (IsNEProxySettings neProxySettings, IsNEProxyServer value) => neProxySettings -> value -> IO ()
setHTTPSServer neProxySettings value =
  sendMessage neProxySettings setHTTPSServerSelector (toNEProxyServer value)

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- excludeSimpleHostnames@
excludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
excludeSimpleHostnames neProxySettings =
  sendMessage neProxySettings excludeSimpleHostnamesSelector

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- setExcludeSimpleHostnames:@
setExcludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setExcludeSimpleHostnames neProxySettings value =
  sendMessage neProxySettings setExcludeSimpleHostnamesSelector value

-- | exceptionList
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will not be used for the connection.
--
-- ObjC selector: @- exceptionList@
exceptionList :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSArray)
exceptionList neProxySettings =
  sendMessage neProxySettings exceptionListSelector

-- | exceptionList
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will not be used for the connection.
--
-- ObjC selector: @- setExceptionList:@
setExceptionList :: (IsNEProxySettings neProxySettings, IsNSArray value) => neProxySettings -> value -> IO ()
setExceptionList neProxySettings value =
  sendMessage neProxySettings setExceptionListSelector (toNSArray value)

-- | matchDomains
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will be used for the connection. Otherwise the proxy settings will not be used. If this property is nil then all connections to which the Network Extension applies will use the proxy settings.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEProxySettings neProxySettings => neProxySettings -> IO (Id NSArray)
matchDomains neProxySettings =
  sendMessage neProxySettings matchDomainsSelector

-- | matchDomains
--
-- An array of domain strings. If the destination host name of a connection shares a suffix with one of these strings then the proxy settings will be used for the connection. Otherwise the proxy settings will not be used. If this property is nil then all connections to which the Network Extension applies will use the proxy settings.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNEProxySettings neProxySettings, IsNSArray value) => neProxySettings -> value -> IO ()
setMatchDomains neProxySettings value =
  sendMessage neProxySettings setMatchDomainsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @autoProxyConfigurationEnabled@
autoProxyConfigurationEnabledSelector :: Selector '[] Bool
autoProxyConfigurationEnabledSelector = mkSelector "autoProxyConfigurationEnabled"

-- | @Selector@ for @setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabledSelector :: Selector '[Bool] ()
setAutoProxyConfigurationEnabledSelector = mkSelector "setAutoProxyConfigurationEnabled:"

-- | @Selector@ for @proxyAutoConfigurationURL@
proxyAutoConfigurationURLSelector :: Selector '[] (Id NSURL)
proxyAutoConfigurationURLSelector = mkSelector "proxyAutoConfigurationURL"

-- | @Selector@ for @setProxyAutoConfigurationURL:@
setProxyAutoConfigurationURLSelector :: Selector '[Id NSURL] ()
setProxyAutoConfigurationURLSelector = mkSelector "setProxyAutoConfigurationURL:"

-- | @Selector@ for @proxyAutoConfigurationJavaScript@
proxyAutoConfigurationJavaScriptSelector :: Selector '[] (Id NSString)
proxyAutoConfigurationJavaScriptSelector = mkSelector "proxyAutoConfigurationJavaScript"

-- | @Selector@ for @setProxyAutoConfigurationJavaScript:@
setProxyAutoConfigurationJavaScriptSelector :: Selector '[Id NSString] ()
setProxyAutoConfigurationJavaScriptSelector = mkSelector "setProxyAutoConfigurationJavaScript:"

-- | @Selector@ for @HTTPEnabled@
httpEnabledSelector :: Selector '[] Bool
httpEnabledSelector = mkSelector "HTTPEnabled"

-- | @Selector@ for @setHTTPEnabled:@
setHTTPEnabledSelector :: Selector '[Bool] ()
setHTTPEnabledSelector = mkSelector "setHTTPEnabled:"

-- | @Selector@ for @HTTPServer@
httpServerSelector :: Selector '[] (Id NEProxyServer)
httpServerSelector = mkSelector "HTTPServer"

-- | @Selector@ for @setHTTPServer:@
setHTTPServerSelector :: Selector '[Id NEProxyServer] ()
setHTTPServerSelector = mkSelector "setHTTPServer:"

-- | @Selector@ for @HTTPSEnabled@
httpsEnabledSelector :: Selector '[] Bool
httpsEnabledSelector = mkSelector "HTTPSEnabled"

-- | @Selector@ for @setHTTPSEnabled:@
setHTTPSEnabledSelector :: Selector '[Bool] ()
setHTTPSEnabledSelector = mkSelector "setHTTPSEnabled:"

-- | @Selector@ for @HTTPSServer@
httpsServerSelector :: Selector '[] (Id NEProxyServer)
httpsServerSelector = mkSelector "HTTPSServer"

-- | @Selector@ for @setHTTPSServer:@
setHTTPSServerSelector :: Selector '[Id NEProxyServer] ()
setHTTPSServerSelector = mkSelector "setHTTPSServer:"

-- | @Selector@ for @excludeSimpleHostnames@
excludeSimpleHostnamesSelector :: Selector '[] Bool
excludeSimpleHostnamesSelector = mkSelector "excludeSimpleHostnames"

-- | @Selector@ for @setExcludeSimpleHostnames:@
setExcludeSimpleHostnamesSelector :: Selector '[Bool] ()
setExcludeSimpleHostnamesSelector = mkSelector "setExcludeSimpleHostnames:"

-- | @Selector@ for @exceptionList@
exceptionListSelector :: Selector '[] (Id NSArray)
exceptionListSelector = mkSelector "exceptionList"

-- | @Selector@ for @setExceptionList:@
setExceptionListSelector :: Selector '[Id NSArray] ()
setExceptionListSelector = mkSelector "setExceptionList:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector '[] (Id NSArray)
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector '[Id NSArray] ()
setMatchDomainsSelector = mkSelector "setMatchDomains:"

