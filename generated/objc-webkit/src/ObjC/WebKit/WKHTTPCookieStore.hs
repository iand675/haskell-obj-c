{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKHTTPCookieStore object allows managing the HTTP cookies associated with a particular WKWebsiteDataStore.
--
-- Generated bindings for @WKHTTPCookieStore@.
module ObjC.WebKit.WKHTTPCookieStore
  ( WKHTTPCookieStore
  , IsWKHTTPCookieStore(..)
  , init_
  , setCookie_completionHandler
  , setCookies_completionHandler
  , deleteCookie_completionHandler
  , addObserver
  , removeObserver
  , setCookiePolicy_completionHandler
  , getCookiePolicy
  , initSelector
  , setCookie_completionHandlerSelector
  , setCookies_completionHandlerSelector
  , deleteCookie_completionHandlerSelector
  , addObserverSelector
  , removeObserverSelector
  , setCookiePolicy_completionHandlerSelector
  , getCookiePolicySelector

  -- * Enum types
  , WKCookiePolicy(WKCookiePolicy)
  , pattern WKCookiePolicyAllow
  , pattern WKCookiePolicyDisallow

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsWKHTTPCookieStore wkhttpCookieStore => wkhttpCookieStore -> IO (Id WKHTTPCookieStore)
init_ wkhttpCookieStore  =
  sendMsg wkhttpCookieStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set a cookie.
--
-- @cookie@ — The cookie to set.
--
-- @completionHandler@ — A block to invoke once the cookie has been stored.
--
-- ObjC selector: @- setCookie:completionHandler:@
setCookie_completionHandler :: (IsWKHTTPCookieStore wkhttpCookieStore, IsNSHTTPCookie cookie) => wkhttpCookieStore -> cookie -> Ptr () -> IO ()
setCookie_completionHandler wkhttpCookieStore  cookie completionHandler =
withObjCPtr cookie $ \raw_cookie ->
    sendMsg wkhttpCookieStore (mkSelector "setCookie:completionHandler:") retVoid [argPtr (castPtr raw_cookie :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Set multiple cookies.
--
-- @cookies@ — An array of cookies to set.
--
-- @completionHandler@ — A block to invoke once the cookies have been stored.
--
-- ObjC selector: @- setCookies:completionHandler:@
setCookies_completionHandler :: (IsWKHTTPCookieStore wkhttpCookieStore, IsNSArray cookies) => wkhttpCookieStore -> cookies -> Ptr () -> IO ()
setCookies_completionHandler wkhttpCookieStore  cookies completionHandler =
withObjCPtr cookies $ \raw_cookies ->
    sendMsg wkhttpCookieStore (mkSelector "setCookies:completionHandler:") retVoid [argPtr (castPtr raw_cookies :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Delete the specified cookie.
--
-- @completionHandler@ — A block to invoke once the cookie has been deleted.
--
-- ObjC selector: @- deleteCookie:completionHandler:@
deleteCookie_completionHandler :: (IsWKHTTPCookieStore wkhttpCookieStore, IsNSHTTPCookie cookie) => wkhttpCookieStore -> cookie -> Ptr () -> IO ()
deleteCookie_completionHandler wkhttpCookieStore  cookie completionHandler =
withObjCPtr cookie $ \raw_cookie ->
    sendMsg wkhttpCookieStore (mkSelector "deleteCookie:completionHandler:") retVoid [argPtr (castPtr raw_cookie :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Adds a WKHTTPCookieStoreObserver object with the cookie store.
--
-- @observer@ — The observer object to add.
--
-- The observer is not retained by the receiver. It is your responsibility to unregister the observer before it becomes invalid.
--
-- ObjC selector: @- addObserver:@
addObserver :: IsWKHTTPCookieStore wkhttpCookieStore => wkhttpCookieStore -> RawId -> IO ()
addObserver wkhttpCookieStore  observer =
  sendMsg wkhttpCookieStore (mkSelector "addObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Removes a WKHTTPCookieStoreObserver object from the cookie store.
--
-- @observer@ — The observer to remove.
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsWKHTTPCookieStore wkhttpCookieStore => wkhttpCookieStore -> RawId -> IO ()
removeObserver wkhttpCookieStore  observer =
  sendMsg wkhttpCookieStore (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Set whether cookies are allowed.
--
-- @policy@ — A value indicating whether cookies are allowed. The default value is WKCookiePolicyAllow.
--
-- @completionHandler@ — A block to invoke once the cookie policy has been set.
--
-- ObjC selector: @- setCookiePolicy:completionHandler:@
setCookiePolicy_completionHandler :: IsWKHTTPCookieStore wkhttpCookieStore => wkhttpCookieStore -> WKCookiePolicy -> Ptr () -> IO ()
setCookiePolicy_completionHandler wkhttpCookieStore  policy completionHandler =
  sendMsg wkhttpCookieStore (mkSelector "setCookiePolicy:completionHandler:") retVoid [argCLong (coerce policy), argPtr (castPtr completionHandler :: Ptr ())]

-- | Get whether cookies are allowed.
--
-- @completionHandler@ — A block to invoke with the value of whether cookies are allowed.
--
-- ObjC selector: @- getCookiePolicy:@
getCookiePolicy :: IsWKHTTPCookieStore wkhttpCookieStore => wkhttpCookieStore -> Ptr () -> IO ()
getCookiePolicy wkhttpCookieStore  completionHandler =
  sendMsg wkhttpCookieStore (mkSelector "getCookiePolicy:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setCookie:completionHandler:@
setCookie_completionHandlerSelector :: Selector
setCookie_completionHandlerSelector = mkSelector "setCookie:completionHandler:"

-- | @Selector@ for @setCookies:completionHandler:@
setCookies_completionHandlerSelector :: Selector
setCookies_completionHandlerSelector = mkSelector "setCookies:completionHandler:"

-- | @Selector@ for @deleteCookie:completionHandler:@
deleteCookie_completionHandlerSelector :: Selector
deleteCookie_completionHandlerSelector = mkSelector "deleteCookie:completionHandler:"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @setCookiePolicy:completionHandler:@
setCookiePolicy_completionHandlerSelector :: Selector
setCookiePolicy_completionHandlerSelector = mkSelector "setCookiePolicy:completionHandler:"

-- | @Selector@ for @getCookiePolicy:@
getCookiePolicySelector :: Selector
getCookiePolicySelector = mkSelector "getCookiePolicy:"

