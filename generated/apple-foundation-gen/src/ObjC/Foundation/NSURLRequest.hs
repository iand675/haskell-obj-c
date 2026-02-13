{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLRequest
--
-- An NSURLRequest object represents a URL load request in a    manner independent of protocol and URL scheme.
--
-- NSURLRequest encapsulates two basic data elements about    a URL load request:        The URL to load.    The policy to use when consulting the URL content cache made    available by the implementation.        In addition, NSURLRequest is designed to be extended to support    protocol-specific data by adding categories to access a property    object provided in an interface targeted at protocol implementors.        Protocol implementors should direct their attention to the    NSURLRequestExtensibility category on NSURLRequest for more    information on how to provide extensions on NSURLRequest to    support protocol-specific request information.    Clients of this API who wish to create NSURLRequest objects to    load URL content should consult the protocol-specific NSURLRequest    categories that are available. The NSHTTPURLRequest category on    NSURLRequest is an example.            Objects of this class are used to create NSURLConnection instances,    which can are used to perform the load of a URL, or as input to the    NSURLConnection class method which performs synchronous loads.
--
-- Generated bindings for @NSURLRequest@.
module ObjC.Foundation.NSURLRequest
  ( NSURLRequest
  , IsNSURLRequest(..)
  , requestWithURL
  , requestWithURL_cachePolicy_timeoutInterval
  , initWithURL
  , initWithURL_cachePolicy_timeoutInterval
  , valueForHTTPHeaderField
  , supportsSecureCoding
  , url
  , cachePolicy
  , timeoutInterval
  , mainDocumentURL
  , networkServiceType
  , allowsCellularAccess
  , allowsExpensiveNetworkAccess
  , allowsConstrainedNetworkAccess
  , allowsUltraConstrainedNetworkAccess
  , assumesHTTP3Capable
  , attribution
  , requiresDNSSECValidation
  , allowsPersistentDNS
  , cookiePartitionIdentifier
  , httpMethod
  , allHTTPHeaderFields
  , httpBody
  , httpBodyStream
  , httpShouldHandleCookies
  , httpShouldUsePipelining
  , allHTTPHeaderFieldsSelector
  , allowsCellularAccessSelector
  , allowsConstrainedNetworkAccessSelector
  , allowsExpensiveNetworkAccessSelector
  , allowsPersistentDNSSelector
  , allowsUltraConstrainedNetworkAccessSelector
  , assumesHTTP3CapableSelector
  , attributionSelector
  , cachePolicySelector
  , cookiePartitionIdentifierSelector
  , httpBodySelector
  , httpBodyStreamSelector
  , httpMethodSelector
  , httpShouldHandleCookiesSelector
  , httpShouldUsePipeliningSelector
  , initWithURLSelector
  , initWithURL_cachePolicy_timeoutIntervalSelector
  , mainDocumentURLSelector
  , networkServiceTypeSelector
  , requestWithURLSelector
  , requestWithURL_cachePolicy_timeoutIntervalSelector
  , requiresDNSSECValidationSelector
  , supportsSecureCodingSelector
  , timeoutIntervalSelector
  , urlSelector
  , valueForHTTPHeaderFieldSelector

  -- * Enum types
  , NSURLRequestAttribution(NSURLRequestAttribution)
  , pattern NSURLRequestAttributionDeveloper
  , pattern NSURLRequestAttributionUser
  , NSURLRequestCachePolicy(NSURLRequestCachePolicy)
  , pattern NSURLRequestUseProtocolCachePolicy
  , pattern NSURLRequestReloadIgnoringLocalCacheData
  , pattern NSURLRequestReloadIgnoringLocalAndRemoteCacheData
  , pattern NSURLRequestReloadIgnoringCacheData
  , pattern NSURLRequestReturnCacheDataElseLoad
  , pattern NSURLRequestReturnCacheDataDontLoad
  , pattern NSURLRequestReloadRevalidatingCacheData
  , NSURLRequestNetworkServiceType(NSURLRequestNetworkServiceType)
  , pattern NSURLNetworkServiceTypeDefault
  , pattern NSURLNetworkServiceTypeVoIP
  , pattern NSURLNetworkServiceTypeVideo
  , pattern NSURLNetworkServiceTypeBackground
  , pattern NSURLNetworkServiceTypeVoice
  , pattern NSURLNetworkServiceTypeResponsiveData
  , pattern NSURLNetworkServiceTypeAVStreaming
  , pattern NSURLNetworkServiceTypeResponsiveAV
  , pattern NSURLNetworkServiceTypeCallSignaling

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | requestWithURL:
--
-- Allocates and initializes an NSURLRequest with the given URL.
--
-- Default values are used for cache policy (NSURLRequestUseProtocolCachePolicy) and timeout interval (60 seconds).
--
-- @URL@ — The URL for the request.
--
-- Returns: A newly-created and autoreleased NSURLRequest instance.
--
-- ObjC selector: @+ requestWithURL:@
requestWithURL :: IsNSURL url => url -> IO (Id NSURLRequest)
requestWithURL url =
  do
    cls' <- getRequiredClass "NSURLRequest"
    sendClassMessage cls' requestWithURLSelector (toNSURL url)

-- | requestWithURL:cachePolicy:timeoutInterval:
--
-- Allocates and initializes a NSURLRequest with the given URL and cache policy.
--
-- @URL@ — The URL for the request.
--
-- @cachePolicy@ — The cache policy for the request.
--
-- @timeoutInterval@ — The timeout interval for the request. See the commentary for the timeoutInterval for more information on timeout intervals.
--
-- Returns: A newly-created and autoreleased NSURLRequest instance.
--
-- ObjC selector: @+ requestWithURL:cachePolicy:timeoutInterval:@
requestWithURL_cachePolicy_timeoutInterval :: IsNSURL url => url -> NSURLRequestCachePolicy -> CDouble -> IO (Id NSURLRequest)
requestWithURL_cachePolicy_timeoutInterval url cachePolicy timeoutInterval =
  do
    cls' <- getRequiredClass "NSURLRequest"
    sendClassMessage cls' requestWithURL_cachePolicy_timeoutIntervalSelector (toNSURL url) cachePolicy timeoutInterval

-- | initWithURL:
--
-- Initializes an NSURLRequest with the given URL.
--
-- Default values are used for cache policy (NSURLRequestUseProtocolCachePolicy) and timeout interval (60 seconds).
--
-- @URL@ — The URL for the request.
--
-- Returns: An initialized NSURLRequest.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsNSURLRequest nsurlRequest, IsNSURL url) => nsurlRequest -> url -> IO (Id NSURLRequest)
initWithURL nsurlRequest url =
  sendOwnedMessage nsurlRequest initWithURLSelector (toNSURL url)

-- | initWithURL:
--
-- Initializes an NSURLRequest with the given URL and cache policy.
--
-- This is the designated initializer for the NSURLRequest class.
--
-- @URL@ — The URL for the request.
--
-- @cachePolicy@ — The cache policy for the request.
--
-- @timeoutInterval@ — The timeout interval for the request. See the commentary for the timeoutInterval for more information on timeout intervals.
--
-- Returns: An initialized NSURLRequest.
--
-- ObjC selector: @- initWithURL:cachePolicy:timeoutInterval:@
initWithURL_cachePolicy_timeoutInterval :: (IsNSURLRequest nsurlRequest, IsNSURL url) => nsurlRequest -> url -> NSURLRequestCachePolicy -> CDouble -> IO (Id NSURLRequest)
initWithURL_cachePolicy_timeoutInterval nsurlRequest url cachePolicy timeoutInterval =
  sendOwnedMessage nsurlRequest initWithURL_cachePolicy_timeoutIntervalSelector (toNSURL url) cachePolicy timeoutInterval

-- | valueForHTTPHeaderField:
--
-- Returns the value which corresponds to the given header    field. Note that, in keeping with the HTTP RFC, HTTP header field    names are case-insensitive.
--
-- @field@ — the header field name to use for the lookup    (case-insensitive).
--
-- Returns: the value associated with the given header field, or nil if    there is no value associated with the given header field.
--
-- ObjC selector: @- valueForHTTPHeaderField:@
valueForHTTPHeaderField :: (IsNSURLRequest nsurlRequest, IsNSString field) => nsurlRequest -> field -> IO (Id NSString)
valueForHTTPHeaderField nsurlRequest field =
  sendMessage nsurlRequest valueForHTTPHeaderFieldSelector (toNSString field)

-- | supportsSecureCoding
--
-- Indicates that NSURLRequest implements the NSSecureCoding protocol.
--
-- Returns: A BOOL value set to YES.
--
-- ObjC selector: @+ supportsSecureCoding@
supportsSecureCoding :: IO Bool
supportsSecureCoding  =
  do
    cls' <- getRequiredClass "NSURLRequest"
    sendClassMessage cls' supportsSecureCodingSelector

-- | Returns the URL of the receiver.
--
-- Returns: The URL of the receiver.
--
-- ObjC selector: @- URL@
url :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSURL)
url nsurlRequest =
  sendMessage nsurlRequest urlSelector

-- | Returns the cache policy of the receiver.
--
-- Returns: The cache policy of the receiver.
--
-- ObjC selector: @- cachePolicy@
cachePolicy :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO NSURLRequestCachePolicy
cachePolicy nsurlRequest =
  sendMessage nsurlRequest cachePolicySelector

-- | Returns the timeout interval of the receiver.
--
-- The timeout interval specifies the limit on the idle interval allotted to a request in the process of loading. The "idle interval" is defined as the period of time that has passed since the last instance of load activity occurred for a request that is in the process of loading. Hence, when an instance of load activity occurs (e.g. bytes are received from the network for a request), the idle interval for a request is reset to 0. If the idle interval ever becomes greater than or equal to the timeout interval, the request is considered to have timed out. This timeout interval is measured in seconds.
--
-- Returns: The timeout interval of the receiver.
--
-- ObjC selector: @- timeoutInterval@
timeoutInterval :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO CDouble
timeoutInterval nsurlRequest =
  sendMessage nsurlRequest timeoutIntervalSelector

-- | The main document URL associated with this load.
--
-- This URL is used for the cookie "same domain as main document" policy, and attributing the request as a sub-resource of a user-specified URL. There may also be other future uses. See setMainDocumentURL:
--
-- Returns: The main document URL.
--
-- ObjC selector: @- mainDocumentURL@
mainDocumentURL :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSURL)
mainDocumentURL nsurlRequest =
  sendMessage nsurlRequest mainDocumentURLSelector

-- | Returns the NSURLRequestNetworkServiceType associated with this request.
--
-- This will return NSURLNetworkServiceTypeDefault for requests that have not explicitly set a networkServiceType (using the setNetworkServiceType method).
--
-- Returns: The NSURLRequestNetworkServiceType associated with this request.
--
-- ObjC selector: @- networkServiceType@
networkServiceType :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO NSURLRequestNetworkServiceType
networkServiceType nsurlRequest =
  sendMessage nsurlRequest networkServiceTypeSelector

-- | returns whether a connection created with this request is allowed to use the built in cellular radios (if present).
--
-- Returns: YES if the receiver is allowed to use the built in cellular radios to satisfy the request, NO otherwise.
--
-- ObjC selector: @- allowsCellularAccess@
allowsCellularAccess :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
allowsCellularAccess nsurlRequest =
  sendMessage nsurlRequest allowsCellularAccessSelector

-- | returns whether a connection created with this request is allowed to use network interfaces which have been marked as expensive.
--
-- Returns: YES if the receiver is allowed to use an interface marked as expensive to satisfy the request, NO otherwise.
--
-- ObjC selector: @- allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccess :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
allowsExpensiveNetworkAccess nsurlRequest =
  sendMessage nsurlRequest allowsExpensiveNetworkAccessSelector

-- | returns whether a connection created with this request is allowed to use network interfaces which have been marked as constrained.
--
-- Returns: YES if the receiver is allowed to use an interface marked as constrained to satisfy the request, NO otherwise.
--
-- ObjC selector: @- allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccess :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
allowsConstrainedNetworkAccess nsurlRequest =
  sendMessage nsurlRequest allowsConstrainedNetworkAccessSelector

-- | returns whether a connection created with this request is allowed to use network interfaces which have been marked as ultra constrained.
--
-- Returns: YES if the receiver is allowed to use an interface marked as ultra constrained to satisfy the request, NO otherwise.
--
-- ObjC selector: @- allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccess :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
allowsUltraConstrainedNetworkAccess nsurlRequest =
  sendMessage nsurlRequest allowsUltraConstrainedNetworkAccessSelector

-- | returns whether we assume that server supports HTTP/3. Enables QUIC racing without HTTP/3 service discovery.
--
-- Returns: YES if server endpoint is known to support HTTP/3. Defaults to NO. The default may be YES in a future OS update.
--
-- ObjC selector: @- assumesHTTP3Capable@
assumesHTTP3Capable :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
assumesHTTP3Capable nsurlRequest =
  sendMessage nsurlRequest assumesHTTP3CapableSelector

-- | Returns the NSURLRequestAttribution associated with this request.
--
-- This will return NSURLRequestAttributionDeveloper for requests that have not explicitly set an attribution.
--
-- Returns: The NSURLRequestAttribution associated with this request.
--
-- ObjC selector: @- attribution@
attribution :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO NSURLRequestAttribution
attribution nsurlRequest =
  sendMessage nsurlRequest attributionSelector

-- | sets whether a request is required to do DNSSEC validation during DNS lookup.
--
-- YES, if the DNS lookup for this request should require DNSSEC validation, No otherwise. Defaults to NO.
--
-- ObjC selector: @- requiresDNSSECValidation@
requiresDNSSECValidation :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
requiresDNSSECValidation nsurlRequest =
  sendMessage nsurlRequest requiresDNSSECValidationSelector

-- | Allows storing and usage of DNS answers, potentially beyond TTL expiry, in a persistent per-process cache. This should only be set for hostnames whose resolutions are not expected to change across networks.
--
-- YES, if the DNS lookup for this request is allowed to use a persistent per-process cache, NO otherwise. Defaults to NO.
--
-- ObjC selector: @- allowsPersistentDNS@
allowsPersistentDNS :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
allowsPersistentDNS nsurlRequest =
  sendMessage nsurlRequest allowsPersistentDNSSelector

-- | @- cookiePartitionIdentifier@
cookiePartitionIdentifier :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSString)
cookiePartitionIdentifier nsurlRequest =
  sendMessage nsurlRequest cookiePartitionIdentifierSelector

-- | Returns the HTTP request method of the receiver.
--
-- Returns: the HTTP request method of the receiver.
--
-- ObjC selector: @- HTTPMethod@
httpMethod :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSString)
httpMethod nsurlRequest =
  sendMessage nsurlRequest httpMethodSelector

-- | Returns a dictionary containing all the HTTP header fields    of the receiver.
--
-- Returns: a dictionary containing all the HTTP header fields of the    receiver.
--
-- ObjC selector: @- allHTTPHeaderFields@
allHTTPHeaderFields :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSDictionary)
allHTTPHeaderFields nsurlRequest =
  sendMessage nsurlRequest allHTTPHeaderFieldsSelector

-- | Returns the request body data of the receiver.
--
-- This data is sent as the message body of the request, as    in done in an HTTP POST request.
--
-- Returns: The request body data of the receiver.
--
-- ObjC selector: @- HTTPBody@
httpBody :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSData)
httpBody nsurlRequest =
  sendMessage nsurlRequest httpBodySelector

-- | Returns the request body stream of the receiver    if any has been set
--
-- The stream is returned for examination only; it is    not safe for the caller to manipulate the stream in any way.  Also    note that the HTTPBodyStream and HTTPBody are mutually exclusive - only    one can be set on a given request.  Also note that the body stream is    preserved across copies, but is LOST when the request is coded via the     NSCoding protocol
--
-- Returns: The request body stream of the receiver.
--
-- ObjC selector: @- HTTPBodyStream@
httpBodyStream :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO (Id NSInputStream)
httpBodyStream nsurlRequest =
  sendMessage nsurlRequest httpBodyStreamSelector

-- | Determine whether default cookie handling will happen for     this request.
--
-- NOTE: This value is not used prior to 10.3
--
-- Returns: YES if cookies will be sent with and set for this request;     otherwise NO.
--
-- ObjC selector: @- HTTPShouldHandleCookies@
httpShouldHandleCookies :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
httpShouldHandleCookies nsurlRequest =
  sendMessage nsurlRequest httpShouldHandleCookiesSelector

-- | Reports whether the receiver is not expected to wait for the previous response before transmitting.
--
-- Returns: YES if the receiver should transmit before the previous response is received.  NO if the receiver should wait for the previous response before transmitting.
--
-- ObjC selector: @- HTTPShouldUsePipelining@
httpShouldUsePipelining :: IsNSURLRequest nsurlRequest => nsurlRequest -> IO Bool
httpShouldUsePipelining nsurlRequest =
  sendMessage nsurlRequest httpShouldUsePipeliningSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestWithURL:@
requestWithURLSelector :: Selector '[Id NSURL] (Id NSURLRequest)
requestWithURLSelector = mkSelector "requestWithURL:"

-- | @Selector@ for @requestWithURL:cachePolicy:timeoutInterval:@
requestWithURL_cachePolicy_timeoutIntervalSelector :: Selector '[Id NSURL, NSURLRequestCachePolicy, CDouble] (Id NSURLRequest)
requestWithURL_cachePolicy_timeoutIntervalSelector = mkSelector "requestWithURL:cachePolicy:timeoutInterval:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id NSURLRequest)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithURL:cachePolicy:timeoutInterval:@
initWithURL_cachePolicy_timeoutIntervalSelector :: Selector '[Id NSURL, NSURLRequestCachePolicy, CDouble] (Id NSURLRequest)
initWithURL_cachePolicy_timeoutIntervalSelector = mkSelector "initWithURL:cachePolicy:timeoutInterval:"

-- | @Selector@ for @valueForHTTPHeaderField:@
valueForHTTPHeaderFieldSelector :: Selector '[Id NSString] (Id NSString)
valueForHTTPHeaderFieldSelector = mkSelector "valueForHTTPHeaderField:"

-- | @Selector@ for @supportsSecureCoding@
supportsSecureCodingSelector :: Selector '[] Bool
supportsSecureCodingSelector = mkSelector "supportsSecureCoding"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @cachePolicy@
cachePolicySelector :: Selector '[] NSURLRequestCachePolicy
cachePolicySelector = mkSelector "cachePolicy"

-- | @Selector@ for @timeoutInterval@
timeoutIntervalSelector :: Selector '[] CDouble
timeoutIntervalSelector = mkSelector "timeoutInterval"

-- | @Selector@ for @mainDocumentURL@
mainDocumentURLSelector :: Selector '[] (Id NSURL)
mainDocumentURLSelector = mkSelector "mainDocumentURL"

-- | @Selector@ for @networkServiceType@
networkServiceTypeSelector :: Selector '[] NSURLRequestNetworkServiceType
networkServiceTypeSelector = mkSelector "networkServiceType"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector '[] Bool
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccessSelector :: Selector '[] Bool
allowsExpensiveNetworkAccessSelector = mkSelector "allowsExpensiveNetworkAccess"

-- | @Selector@ for @allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccessSelector :: Selector '[] Bool
allowsConstrainedNetworkAccessSelector = mkSelector "allowsConstrainedNetworkAccess"

-- | @Selector@ for @allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccessSelector :: Selector '[] Bool
allowsUltraConstrainedNetworkAccessSelector = mkSelector "allowsUltraConstrainedNetworkAccess"

-- | @Selector@ for @assumesHTTP3Capable@
assumesHTTP3CapableSelector :: Selector '[] Bool
assumesHTTP3CapableSelector = mkSelector "assumesHTTP3Capable"

-- | @Selector@ for @attribution@
attributionSelector :: Selector '[] NSURLRequestAttribution
attributionSelector = mkSelector "attribution"

-- | @Selector@ for @requiresDNSSECValidation@
requiresDNSSECValidationSelector :: Selector '[] Bool
requiresDNSSECValidationSelector = mkSelector "requiresDNSSECValidation"

-- | @Selector@ for @allowsPersistentDNS@
allowsPersistentDNSSelector :: Selector '[] Bool
allowsPersistentDNSSelector = mkSelector "allowsPersistentDNS"

-- | @Selector@ for @cookiePartitionIdentifier@
cookiePartitionIdentifierSelector :: Selector '[] (Id NSString)
cookiePartitionIdentifierSelector = mkSelector "cookiePartitionIdentifier"

-- | @Selector@ for @HTTPMethod@
httpMethodSelector :: Selector '[] (Id NSString)
httpMethodSelector = mkSelector "HTTPMethod"

-- | @Selector@ for @allHTTPHeaderFields@
allHTTPHeaderFieldsSelector :: Selector '[] (Id NSDictionary)
allHTTPHeaderFieldsSelector = mkSelector "allHTTPHeaderFields"

-- | @Selector@ for @HTTPBody@
httpBodySelector :: Selector '[] (Id NSData)
httpBodySelector = mkSelector "HTTPBody"

-- | @Selector@ for @HTTPBodyStream@
httpBodyStreamSelector :: Selector '[] (Id NSInputStream)
httpBodyStreamSelector = mkSelector "HTTPBodyStream"

-- | @Selector@ for @HTTPShouldHandleCookies@
httpShouldHandleCookiesSelector :: Selector '[] Bool
httpShouldHandleCookiesSelector = mkSelector "HTTPShouldHandleCookies"

-- | @Selector@ for @HTTPShouldUsePipelining@
httpShouldUsePipeliningSelector :: Selector '[] Bool
httpShouldUsePipeliningSelector = mkSelector "HTTPShouldUsePipelining"

