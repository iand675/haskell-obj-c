{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSHTTPCookieStorage
--
-- NSHTTPCookieStorage implements a singleton object (shared    instance) which manages the shared cookie store.  It has methods    to allow clients to set and remove cookies, and get the current    set of cookies.  It also has convenience methods to parse and    generate cookie-related HTTP header fields.
--
-- Generated bindings for @NSHTTPCookieStorage@.
module ObjC.Foundation.NSHTTPCookieStorage
  ( NSHTTPCookieStorage
  , IsNSHTTPCookieStorage(..)
  , sharedCookieStorageForGroupContainerIdentifier
  , setCookie
  , deleteCookie
  , removeCookiesSinceDate
  , cookiesForURL
  , setCookies_forURL_mainDocumentURL
  , sortedCookiesUsingDescriptors
  , storeCookies_forTask
  , sharedHTTPCookieStorage
  , cookies
  , cookieAcceptPolicy
  , setCookieAcceptPolicy
  , sharedCookieStorageForGroupContainerIdentifierSelector
  , setCookieSelector
  , deleteCookieSelector
  , removeCookiesSinceDateSelector
  , cookiesForURLSelector
  , setCookies_forURL_mainDocumentURLSelector
  , sortedCookiesUsingDescriptorsSelector
  , storeCookies_forTaskSelector
  , sharedHTTPCookieStorageSelector
  , cookiesSelector
  , cookieAcceptPolicySelector
  , setCookieAcceptPolicySelector

  -- * Enum types
  , NSHTTPCookieAcceptPolicy(NSHTTPCookieAcceptPolicy)
  , pattern NSHTTPCookieAcceptPolicyAlways
  , pattern NSHTTPCookieAcceptPolicyNever
  , pattern NSHTTPCookieAcceptPolicyOnlyFromMainDocumentDomain

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | sharedCookieStorageForGroupContainerIdentifier:
--
-- Get the cookie storage for the container associated with the specified application group identifier
--
-- @identifier@ — The application group identifier
--
-- Returns: A cookie storage with a persistent store in the application group container
--
-- By default, applications and associated app extensions have different data containers, which means    that the sharedHTTPCookieStorage singleton will refer to different persistent cookie stores in an application and    any app extensions that it contains. This method allows clients to create a persistent cookie storage that can be    shared among all applications and extensions with access to the same application group. Subsequent calls to this    method with the same identifier will return the same cookie storage instance.
--
-- ObjC selector: @+ sharedCookieStorageForGroupContainerIdentifier:@
sharedCookieStorageForGroupContainerIdentifier :: IsNSString identifier => identifier -> IO (Id NSHTTPCookieStorage)
sharedCookieStorageForGroupContainerIdentifier identifier =
  do
    cls' <- getRequiredClass "NSHTTPCookieStorage"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "sharedCookieStorageForGroupContainerIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | setCookie:
--
-- Set a cookie
--
-- The cookie will override an existing cookie with the    same name, domain and path, if any.
--
-- ObjC selector: @- setCookie:@
setCookie :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSHTTPCookie cookie) => nshttpCookieStorage -> cookie -> IO ()
setCookie nshttpCookieStorage  cookie =
withObjCPtr cookie $ \raw_cookie ->
    sendMsg nshttpCookieStorage (mkSelector "setCookie:") retVoid [argPtr (castPtr raw_cookie :: Ptr ())]

-- | deleteCookie:
--
-- Delete the specified cookie
--
-- ObjC selector: @- deleteCookie:@
deleteCookie :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSHTTPCookie cookie) => nshttpCookieStorage -> cookie -> IO ()
deleteCookie nshttpCookieStorage  cookie =
withObjCPtr cookie $ \raw_cookie ->
    sendMsg nshttpCookieStorage (mkSelector "deleteCookie:") retVoid [argPtr (castPtr raw_cookie :: Ptr ())]

-- | removeCookiesSince:
--
-- Delete all cookies from the cookie storage since the provided date.
--
-- ObjC selector: @- removeCookiesSinceDate:@
removeCookiesSinceDate :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSDate date) => nshttpCookieStorage -> date -> IO ()
removeCookiesSinceDate nshttpCookieStorage  date =
withObjCPtr date $ \raw_date ->
    sendMsg nshttpCookieStorage (mkSelector "removeCookiesSinceDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | cookiesForURL:
--
-- Returns an array of cookies to send to the given URL.
--
-- @URL@ — The URL for which to get cookies.
--
-- Returns: an NSArray of NSHTTPCookie objects.
--
-- The cookie manager examines the cookies it stores and    includes those which should be sent to the given URL. You can use    +[NSCookie requestHeaderFieldsWithCookies:] to turn this array    into a set of header fields to add to a request.
--
-- ObjC selector: @- cookiesForURL:@
cookiesForURL :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSURL url) => nshttpCookieStorage -> url -> IO (Id NSArray)
cookiesForURL nshttpCookieStorage  url =
withObjCPtr url $ \raw_url ->
    sendMsg nshttpCookieStorage (mkSelector "cookiesForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | setCookies:forURL:mainDocumentURL:
--
-- Adds an array cookies to the cookie store, following the    cookie accept policy.
--
-- @cookies@ — The cookies to set.
--
-- @URL@ — The URL from which the cookies were sent.
--
-- @mainDocumentURL@ — The main document URL to be used as a base for the "same    domain as main document" policy.
--
-- For mainDocumentURL, the caller should pass the URL for    an appropriate main document, if known. For example, when loading    a web page, the URL of the main html document for the top-level    frame should be passed. To save cookies based on a set of response    headers, you can use +[NSCookie    cookiesWithResponseHeaderFields:forURL:] on a header field    dictionary and then use this method to store the resulting cookies    in accordance with policy settings.
--
-- ObjC selector: @- setCookies:forURL:mainDocumentURL:@
setCookies_forURL_mainDocumentURL :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSArray cookies, IsNSURL url, IsNSURL mainDocumentURL) => nshttpCookieStorage -> cookies -> url -> mainDocumentURL -> IO ()
setCookies_forURL_mainDocumentURL nshttpCookieStorage  cookies url mainDocumentURL =
withObjCPtr cookies $ \raw_cookies ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr mainDocumentURL $ \raw_mainDocumentURL ->
        sendMsg nshttpCookieStorage (mkSelector "setCookies:forURL:mainDocumentURL:") retVoid [argPtr (castPtr raw_cookies :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_mainDocumentURL :: Ptr ())]

-- | sortedCookiesUsingDescriptors:
--
-- Returns an array of all cookies in the store, sorted according to the key value and sorting direction of the NSSortDescriptors specified in the parameter.
--
-- @sortOrder@ — an array of NSSortDescriptors which represent the preferred sort order of the resulting array.
--
-- proper sorting of cookies may require extensive string conversion, which can be avoided by allowing the system to perform the sorting.  This API is to be preferred over the more generic -[NSHTTPCookieStorage cookies] API, if sorting is going to be performed.
--
-- ObjC selector: @- sortedCookiesUsingDescriptors:@
sortedCookiesUsingDescriptors :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSArray sortOrder) => nshttpCookieStorage -> sortOrder -> IO (Id NSArray)
sortedCookiesUsingDescriptors nshttpCookieStorage  sortOrder =
withObjCPtr sortOrder $ \raw_sortOrder ->
    sendMsg nshttpCookieStorage (mkSelector "sortedCookiesUsingDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_sortOrder :: Ptr ())] >>= retainedObject . castPtr

-- | @- storeCookies:forTask:@
storeCookies_forTask :: (IsNSHTTPCookieStorage nshttpCookieStorage, IsNSArray cookies, IsNSURLSessionTask task) => nshttpCookieStorage -> cookies -> task -> IO ()
storeCookies_forTask nshttpCookieStorage  cookies task =
withObjCPtr cookies $ \raw_cookies ->
  withObjCPtr task $ \raw_task ->
      sendMsg nshttpCookieStorage (mkSelector "storeCookies:forTask:") retVoid [argPtr (castPtr raw_cookies :: Ptr ()), argPtr (castPtr raw_task :: Ptr ())]

-- | sharedHTTPCookieStorage
--
-- Get the shared cookie storage in the default location.
--
-- Returns: The shared cookie storage
--
-- Starting in OS X 10.11, each app has its own sharedHTTPCookieStorage singleton,     which will not be shared with other applications.
--
-- ObjC selector: @+ sharedHTTPCookieStorage@
sharedHTTPCookieStorage :: IO (Id NSHTTPCookieStorage)
sharedHTTPCookieStorage  =
  do
    cls' <- getRequiredClass "NSHTTPCookieStorage"
    sendClassMsg cls' (mkSelector "sharedHTTPCookieStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get all the cookies
--
-- Returns: An NSArray of NSHTTPCookies
--
-- ObjC selector: @- cookies@
cookies :: IsNSHTTPCookieStorage nshttpCookieStorage => nshttpCookieStorage -> IO (Id NSArray)
cookies nshttpCookieStorage  =
  sendMsg nshttpCookieStorage (mkSelector "cookies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The cookie accept policy preference of the    receiver.
--
-- ObjC selector: @- cookieAcceptPolicy@
cookieAcceptPolicy :: IsNSHTTPCookieStorage nshttpCookieStorage => nshttpCookieStorage -> IO NSHTTPCookieAcceptPolicy
cookieAcceptPolicy nshttpCookieStorage  =
  fmap (coerce :: CULong -> NSHTTPCookieAcceptPolicy) $ sendMsg nshttpCookieStorage (mkSelector "cookieAcceptPolicy") retCULong []

-- | The cookie accept policy preference of the    receiver.
--
-- ObjC selector: @- setCookieAcceptPolicy:@
setCookieAcceptPolicy :: IsNSHTTPCookieStorage nshttpCookieStorage => nshttpCookieStorage -> NSHTTPCookieAcceptPolicy -> IO ()
setCookieAcceptPolicy nshttpCookieStorage  value =
  sendMsg nshttpCookieStorage (mkSelector "setCookieAcceptPolicy:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCookieStorageForGroupContainerIdentifier:@
sharedCookieStorageForGroupContainerIdentifierSelector :: Selector
sharedCookieStorageForGroupContainerIdentifierSelector = mkSelector "sharedCookieStorageForGroupContainerIdentifier:"

-- | @Selector@ for @setCookie:@
setCookieSelector :: Selector
setCookieSelector = mkSelector "setCookie:"

-- | @Selector@ for @deleteCookie:@
deleteCookieSelector :: Selector
deleteCookieSelector = mkSelector "deleteCookie:"

-- | @Selector@ for @removeCookiesSinceDate:@
removeCookiesSinceDateSelector :: Selector
removeCookiesSinceDateSelector = mkSelector "removeCookiesSinceDate:"

-- | @Selector@ for @cookiesForURL:@
cookiesForURLSelector :: Selector
cookiesForURLSelector = mkSelector "cookiesForURL:"

-- | @Selector@ for @setCookies:forURL:mainDocumentURL:@
setCookies_forURL_mainDocumentURLSelector :: Selector
setCookies_forURL_mainDocumentURLSelector = mkSelector "setCookies:forURL:mainDocumentURL:"

-- | @Selector@ for @sortedCookiesUsingDescriptors:@
sortedCookiesUsingDescriptorsSelector :: Selector
sortedCookiesUsingDescriptorsSelector = mkSelector "sortedCookiesUsingDescriptors:"

-- | @Selector@ for @storeCookies:forTask:@
storeCookies_forTaskSelector :: Selector
storeCookies_forTaskSelector = mkSelector "storeCookies:forTask:"

-- | @Selector@ for @sharedHTTPCookieStorage@
sharedHTTPCookieStorageSelector :: Selector
sharedHTTPCookieStorageSelector = mkSelector "sharedHTTPCookieStorage"

-- | @Selector@ for @cookies@
cookiesSelector :: Selector
cookiesSelector = mkSelector "cookies"

-- | @Selector@ for @cookieAcceptPolicy@
cookieAcceptPolicySelector :: Selector
cookieAcceptPolicySelector = mkSelector "cookieAcceptPolicy"

-- | @Selector@ for @setCookieAcceptPolicy:@
setCookieAcceptPolicySelector :: Selector
setCookieAcceptPolicySelector = mkSelector "setCookieAcceptPolicy:"

