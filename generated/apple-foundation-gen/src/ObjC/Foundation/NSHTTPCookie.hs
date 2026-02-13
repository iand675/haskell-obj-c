{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSHTTPCookie
--
-- NSHTTPCookie represents an http cookie.
--
-- A NSHTTPCookie instance represents a single http cookie. It is    an immutable object initialized from a dictionary that contains    the various cookie attributes. It has accessors to get the various    attributes of a cookie.
--
-- Generated bindings for @NSHTTPCookie@.
module ObjC.Foundation.NSHTTPCookie
  ( NSHTTPCookie
  , IsNSHTTPCookie(..)
  , initWithProperties
  , cookieWithProperties
  , requestHeaderFieldsWithCookies
  , cookiesWithResponseHeaderFields_forURL
  , properties
  , version
  , name
  , value
  , expiresDate
  , sessionOnly
  , domain
  , path
  , secure
  , httpOnly
  , comment
  , commentURL
  , portList
  , sameSitePolicy
  , commentSelector
  , commentURLSelector
  , cookieWithPropertiesSelector
  , cookiesWithResponseHeaderFields_forURLSelector
  , domainSelector
  , expiresDateSelector
  , httpOnlySelector
  , initWithPropertiesSelector
  , nameSelector
  , pathSelector
  , portListSelector
  , propertiesSelector
  , requestHeaderFieldsWithCookiesSelector
  , sameSitePolicySelector
  , secureSelector
  , sessionOnlySelector
  , valueSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | initWithProperties:
--
-- Initialize a NSHTTPCookie object with a dictionary of    parameters
--
-- @properties@ — The dictionary of properties to be used to    initialize this cookie.
--
-- Supported dictionary keys and value types for the    given dictionary are as follows.
--
-- All properties can handle an NSString value, but some can also    handle other types.
--
-- Property key constant        Type of value        Required        Description                NSHTTPCookieComment        NSString        NO        Comment for the cookie. Only valid for version 1 cookies and        later. Default is nil.                NSHTTPCookieCommentURL        NSURL or NSString        NO        Comment URL for the cookie. Only valid for version 1 cookies        and later. Default is nil.                NSHTTPCookieDomain        NSString        Special, a value for either NSHTTPCookieOriginURL or        NSHTTPCookieDomain must be specified.        Domain for the cookie. Inferred from the value for        NSHTTPCookieOriginURL if not provided.                NSHTTPCookieDiscard        NSString        NO        A string stating whether the cookie should be discarded at        the end of the session. String value must be either "TRUE" or        "FALSE". Default is "FALSE", unless this is cookie is version        1 or greater and a value for NSHTTPCookieMaximumAge is not        specified, in which case it is assumed "TRUE".                NSHTTPCookieExpires        NSDate or NSString        NO        Expiration date for the cookie. Used only for version 0        cookies. Ignored for version 1 or greater.                NSHTTPCookieMaximumAge        NSString        NO        A string containing an integer value stating how long in        seconds the cookie should be kept, at most. Only valid for        version 1 cookies and later. Default is "0".                NSHTTPCookieName        NSString        YES        Name of the cookie                NSHTTPCookieOriginURL        NSURL or NSString        Special, a value for either NSHTTPCookieOriginURL or        NSHTTPCookieDomain must be specified.        URL that set this cookie. Used as default for other fields        as noted.                NSHTTPCookiePath        NSString        NO        Path for the cookie. Inferred from the value for        NSHTTPCookieOriginURL if not provided. Default is "/".                NSHTTPCookiePort        NSString        NO        comma-separated integer values specifying the ports for the        cookie. Only valid for version 1 cookies and later. Default is        empty string ("").                NSHTTPCookieSecure        NSString        NO        A string stating whether the cookie should be transmitted        only over secure channels. String value must be either "TRUE"        or "FALSE". Default is "FALSE".                NSHTTPCookieValue        NSString        YES        Value of the cookie                NSHTTPCookieVersion        NSString        NO        Specifies the version of the cookie. Must be either "0" or        "1". Default is "0".                  NSHTTPCookieSetByJavaScript         NSNumber         NO          if the cookie is set via JavaScript.  if the cookie         is not set via JavaScript                 All other keys are ignored.
--
-- Returns: An initialized NSHTTPCookie, or nil if the set of    dictionary keys is invalid, for example because a required key is    missing, or a recognized key maps to an illegal value.
--
-- ObjC selector: @- initWithProperties:@
initWithProperties :: (IsNSHTTPCookie nshttpCookie, IsNSDictionary properties) => nshttpCookie -> properties -> IO (Id NSHTTPCookie)
initWithProperties nshttpCookie properties =
  sendOwnedMessage nshttpCookie initWithPropertiesSelector (toNSDictionary properties)

-- | cookieWithProperties:
--
-- Allocates and initializes an NSHTTPCookie with the given    dictionary.
--
-- See the NSHTTPCookie -initWithProperties:    method for more information on the constraints imposed on the    dictionary, and for descriptions of the supported keys and values.
--
-- @properties@ — The dictionary to use to initialize this cookie.
--
-- Returns: A newly-created and autoreleased NSHTTPCookie instance, or    nil if the set of dictionary keys is invalid, for example because    a required key is missing, or a recognized key maps to an illegal    value.
--
-- ObjC selector: @+ cookieWithProperties:@
cookieWithProperties :: IsNSDictionary properties => properties -> IO (Id NSHTTPCookie)
cookieWithProperties properties =
  do
    cls' <- getRequiredClass "NSHTTPCookie"
    sendClassMessage cls' cookieWithPropertiesSelector (toNSDictionary properties)

-- | requestHeaderFieldsWithCookies:
--
-- Return a dictionary of header fields that can be used to add the    specified cookies to the request.
--
-- @cookies@ — The cookies to turn into request headers.
--
-- Returns: An NSDictionary where the keys are header field names, and the values    are the corresponding header field values.
--
-- ObjC selector: @+ requestHeaderFieldsWithCookies:@
requestHeaderFieldsWithCookies :: IsNSArray cookies => cookies -> IO (Id NSDictionary)
requestHeaderFieldsWithCookies cookies =
  do
    cls' <- getRequiredClass "NSHTTPCookie"
    sendClassMessage cls' requestHeaderFieldsWithCookiesSelector (toNSArray cookies)

-- | cookiesWithResponseHeaderFields:forURL:
--
-- Return an array of cookies parsed from the specified response header fields and URL.
--
-- @headerFields@ — The response header fields to check for cookies.
--
-- @URL@ — The URL that the cookies came from - relevant to how the cookies are interpreted.
--
-- Returns: An NSArray of NSHTTPCookie objects
--
-- This method will ignore irrelevant header fields so    you can pass a dictionary containing data other than cookie data.
--
-- ObjC selector: @+ cookiesWithResponseHeaderFields:forURL:@
cookiesWithResponseHeaderFields_forURL :: (IsNSDictionary headerFields, IsNSURL url) => headerFields -> url -> IO (Id NSArray)
cookiesWithResponseHeaderFields_forURL headerFields url =
  do
    cls' <- getRequiredClass "NSHTTPCookie"
    sendClassMessage cls' cookiesWithResponseHeaderFields_forURLSelector (toNSDictionary headerFields) (toNSURL url)

-- | Returns a dictionary representation of the receiver.
--
-- This method returns a dictionary representation of the    NSHTTPCookie which can be saved and passed to    -initWithProperties: or +cookieWithProperties:    later to reconstitute an equivalent cookie.    See the NSHTTPCookie -initWithProperties: method for    more information on the constraints imposed on the dictionary, and    for descriptions of the supported keys and values.
--
-- Returns: The dictionary representation of the receiver.
--
-- ObjC selector: @- properties@
properties :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSDictionary)
properties nshttpCookie =
  sendMessage nshttpCookie propertiesSelector

-- | Returns the version of the receiver.
--
-- Version 0 maps to "old-style" Netscape cookies.    Version 1 maps to RFC2965 cookies. There may be future versions.
--
-- Returns: the version of the receiver.
--
-- ObjC selector: @- version@
version :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO CULong
version nshttpCookie =
  sendMessage nshttpCookie versionSelector

-- | Returns the name of the receiver.
--
-- Returns: the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
name nshttpCookie =
  sendMessage nshttpCookie nameSelector

-- | Returns the value of the receiver.
--
-- Returns: the value of the receiver.
--
-- ObjC selector: @- value@
value :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
value nshttpCookie =
  sendMessage nshttpCookie valueSelector

-- | Returns the expires date of the receiver.
--
-- Returns: the expires date of the receiver.
--
-- The expires date is the date when the cookie should be    deleted. The result will be nil if there is no specific expires    date. This will be the case only for "session-only" cookies.
--
-- Returns: The expires date of the receiver.
--
-- ObjC selector: @- expiresDate@
expiresDate :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSDate)
expiresDate nshttpCookie =
  sendMessage nshttpCookie expiresDateSelector

-- | Returns whether the receiver is session-only.
--
-- Returns: YES if this receiver should be discarded at the end of the    session (regardless of expiration date), NO if receiver need not    be discarded at the end of the session.
--
-- ObjC selector: @- sessionOnly@
sessionOnly :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO Bool
sessionOnly nshttpCookie =
  sendMessage nshttpCookie sessionOnlySelector

-- | Returns the domain of the receiver.
--
-- This value specifies URL domain to which the cookie    should be sent. A domain with a leading dot means the cookie    should be sent to subdomains as well, assuming certain other    restrictions are valid. See RFC 2965 for more detail.
--
-- Returns: The domain of the receiver.
--
-- ObjC selector: @- domain@
domain :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
domain nshttpCookie =
  sendMessage nshttpCookie domainSelector

-- | Returns the path of the receiver.
--
-- This value specifies the URL path under the cookie's    domain for which this cookie should be sent. The cookie will also    be sent for children of that path, so "/" is the most general.
--
-- Returns: The path of the receiver.
--
-- ObjC selector: @- path@
path :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
path nshttpCookie =
  sendMessage nshttpCookie pathSelector

-- | Returns whether the receiver should be sent only over    secure channels
--
-- Cookies may be marked secure by a server (or by a javascript).    Cookies marked as such must only be sent via an encrypted connection to     trusted servers (i.e. via SSL or TLS), and should not be delivered to any    javascript applications to prevent cross-site scripting vulnerabilities.
--
-- Returns: YES if this cookie should be sent only over secure channels,    NO otherwise.
--
-- ObjC selector: @- secure@
secure :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO Bool
secure nshttpCookie =
  sendMessage nshttpCookie secureSelector

-- | Returns whether the receiver should only be sent to HTTP servers    per RFC 2965
--
-- Cookies may be marked as HTTPOnly by a server (or by a javascript).    Cookies marked as such must only be sent via HTTP Headers in HTTP Requests    for URL's that match both the path and domain of the respective Cookies.    Specifically these cookies should not be delivered to any javascript     applications to prevent cross-site scripting vulnerabilities.
--
-- Returns: YES if this cookie should only be sent via HTTP headers,    NO otherwise.
--
-- ObjC selector: @- HTTPOnly@
httpOnly :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO Bool
httpOnly nshttpCookie =
  sendMessage nshttpCookie httpOnlySelector

-- | Returns the comment of the receiver.
--
-- This value specifies a string which is suitable for    presentation to the user explaining the contents and purpose of this    cookie. It may be nil.
--
-- Returns: The comment of the receiver, or nil if the receiver has no    comment.
--
-- ObjC selector: @- comment@
comment :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
comment nshttpCookie =
  sendMessage nshttpCookie commentSelector

-- | Returns the comment URL of the receiver.
--
-- This value specifies a URL which is suitable for    presentation to the user as a link for further information about    this cookie. It may be nil.
--
-- Returns: The comment URL of the receiver, or nil if the receiver    has no comment URL.
--
-- ObjC selector: @- commentURL@
commentURL :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSURL)
commentURL nshttpCookie =
  sendMessage nshttpCookie commentURLSelector

-- | Returns the list ports to which the receiver should be    sent.
--
-- This value specifies an NSArray of NSNumbers    (containing integers) which specify the only ports to which this    cookie should be sent.
--
-- Returns: The list ports to which the receiver should be sent. The    array may be nil, in which case this cookie can be sent to any    port.
--
-- ObjC selector: @- portList@
portList :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSArray)
portList nshttpCookie =
  sendMessage nshttpCookie portListSelector

-- | Returns the value of the same site attribute on the cookie.
--
-- Cookies can be marked with an attribute Strict or Lax. Cookies marked with "strict" (NSHTTPCookieSameSiteStrict) are not sent along with cross-site requests. Cookies marked with "lax" (NSHTTPCookieSameSiteLax) sent along cross-site requests provided the cross-site requests are top-level-requests (one that changes the url in the address bar). The attribute value is canonicalized and stored. Any value other than the default (strict and lax) will be ignored.
--
-- Returns: strict or lax. The result could also be nil, in which case the cookie will be sent along with all cross-site requests.
--
-- ObjC selector: @- sameSitePolicy@
sameSitePolicy :: IsNSHTTPCookie nshttpCookie => nshttpCookie -> IO (Id NSString)
sameSitePolicy nshttpCookie =
  sendMessage nshttpCookie sameSitePolicySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProperties:@
initWithPropertiesSelector :: Selector '[Id NSDictionary] (Id NSHTTPCookie)
initWithPropertiesSelector = mkSelector "initWithProperties:"

-- | @Selector@ for @cookieWithProperties:@
cookieWithPropertiesSelector :: Selector '[Id NSDictionary] (Id NSHTTPCookie)
cookieWithPropertiesSelector = mkSelector "cookieWithProperties:"

-- | @Selector@ for @requestHeaderFieldsWithCookies:@
requestHeaderFieldsWithCookiesSelector :: Selector '[Id NSArray] (Id NSDictionary)
requestHeaderFieldsWithCookiesSelector = mkSelector "requestHeaderFieldsWithCookies:"

-- | @Selector@ for @cookiesWithResponseHeaderFields:forURL:@
cookiesWithResponseHeaderFields_forURLSelector :: Selector '[Id NSDictionary, Id NSURL] (Id NSArray)
cookiesWithResponseHeaderFields_forURLSelector = mkSelector "cookiesWithResponseHeaderFields:forURL:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSDictionary)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CULong
versionSelector = mkSelector "version"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @expiresDate@
expiresDateSelector :: Selector '[] (Id NSDate)
expiresDateSelector = mkSelector "expiresDate"

-- | @Selector@ for @sessionOnly@
sessionOnlySelector :: Selector '[] Bool
sessionOnlySelector = mkSelector "sessionOnly"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @secure@
secureSelector :: Selector '[] Bool
secureSelector = mkSelector "secure"

-- | @Selector@ for @HTTPOnly@
httpOnlySelector :: Selector '[] Bool
httpOnlySelector = mkSelector "HTTPOnly"

-- | @Selector@ for @comment@
commentSelector :: Selector '[] (Id NSString)
commentSelector = mkSelector "comment"

-- | @Selector@ for @commentURL@
commentURLSelector :: Selector '[] (Id NSURL)
commentURLSelector = mkSelector "commentURL"

-- | @Selector@ for @portList@
portListSelector :: Selector '[] (Id NSArray)
portListSelector = mkSelector "portList"

-- | @Selector@ for @sameSitePolicy@
sameSitePolicySelector :: Selector '[] (Id NSString)
sameSitePolicySelector = mkSelector "sameSitePolicy"

