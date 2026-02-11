{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSMutableURLRequest
--
-- An NSMutableURLRequest object represents a mutable URL load    request in a manner independent of protocol and URL scheme.
--
-- This specialization of NSURLRequest is provided to aid    developers who may find it more convenient to mutate a single request    object for a series of URL loads instead of creating an immutable    NSURLRequest for each load. This programming model is supported by    the following contract stipulation between NSMutableURLRequest and     NSURLConnection: NSURLConnection makes a deep copy of each     NSMutableURLRequest object passed to one of its initializers.        NSMutableURLRequest is designed to be extended to support    protocol-specific data by adding categories to access a property    object provided in an interface targeted at protocol implementors.        Protocol implementors should direct their attention to the    NSMutableURLRequestExtensibility category on    NSMutableURLRequest for more information on how to provide    extensions on NSMutableURLRequest to support protocol-specific    request information.    Clients of this API who wish to create NSMutableURLRequest    objects to load URL content should consult the protocol-specific    NSMutableURLRequest categories that are available. The    NSMutableHTTPURLRequest category on NSMutableURLRequest is an    example.
--
-- Generated bindings for @NSMutableURLRequest@.
module ObjC.Foundation.NSMutableURLRequest
  ( NSMutableURLRequest
  , IsNSMutableURLRequest(..)
  , setValue_forHTTPHeaderField
  , addValue_forHTTPHeaderField
  , url
  , setURL
  , cachePolicy
  , setCachePolicy
  , timeoutInterval
  , setTimeoutInterval
  , mainDocumentURL
  , setMainDocumentURL
  , networkServiceType
  , setNetworkServiceType
  , allowsCellularAccess
  , setAllowsCellularAccess
  , allowsExpensiveNetworkAccess
  , setAllowsExpensiveNetworkAccess
  , allowsConstrainedNetworkAccess
  , setAllowsConstrainedNetworkAccess
  , allowsUltraConstrainedNetworkAccess
  , setAllowsUltraConstrainedNetworkAccess
  , assumesHTTP3Capable
  , setAssumesHTTP3Capable
  , attribution
  , setAttribution
  , requiresDNSSECValidation
  , setRequiresDNSSECValidation
  , allowsPersistentDNS
  , setAllowsPersistentDNS
  , cookiePartitionIdentifier
  , setCookiePartitionIdentifier
  , httpMethod
  , setHTTPMethod
  , allHTTPHeaderFields
  , setAllHTTPHeaderFields
  , httpBody
  , setHTTPBody
  , httpBodyStream
  , setHTTPBodyStream
  , httpShouldHandleCookies
  , setHTTPShouldHandleCookies
  , httpShouldUsePipelining
  , setHTTPShouldUsePipelining
  , setValue_forHTTPHeaderFieldSelector
  , addValue_forHTTPHeaderFieldSelector
  , urlSelector
  , setURLSelector
  , cachePolicySelector
  , setCachePolicySelector
  , timeoutIntervalSelector
  , setTimeoutIntervalSelector
  , mainDocumentURLSelector
  , setMainDocumentURLSelector
  , networkServiceTypeSelector
  , setNetworkServiceTypeSelector
  , allowsCellularAccessSelector
  , setAllowsCellularAccessSelector
  , allowsExpensiveNetworkAccessSelector
  , setAllowsExpensiveNetworkAccessSelector
  , allowsConstrainedNetworkAccessSelector
  , setAllowsConstrainedNetworkAccessSelector
  , allowsUltraConstrainedNetworkAccessSelector
  , setAllowsUltraConstrainedNetworkAccessSelector
  , assumesHTTP3CapableSelector
  , setAssumesHTTP3CapableSelector
  , attributionSelector
  , setAttributionSelector
  , requiresDNSSECValidationSelector
  , setRequiresDNSSECValidationSelector
  , allowsPersistentDNSSelector
  , setAllowsPersistentDNSSelector
  , cookiePartitionIdentifierSelector
  , setCookiePartitionIdentifierSelector
  , httpMethodSelector
  , setHTTPMethodSelector
  , allHTTPHeaderFieldsSelector
  , setAllHTTPHeaderFieldsSelector
  , httpBodySelector
  , setHTTPBodySelector
  , httpBodyStreamSelector
  , setHTTPBodyStreamSelector
  , httpShouldHandleCookiesSelector
  , setHTTPShouldHandleCookiesSelector
  , httpShouldUsePipeliningSelector
  , setHTTPShouldUsePipeliningSelector

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

-- | setValue:forHTTPHeaderField:
--
-- Sets the value of the given HTTP header field.
--
-- If a value was previously set for the given header    field, that value is replaced with the given value. Note that, in    keeping with the HTTP RFC, HTTP header field names are    case-insensitive.
--
-- @value@ — the header field value.
--
-- @field@ — the header field name (case-insensitive).
--
-- ObjC selector: @- setValue:forHTTPHeaderField:@
setValue_forHTTPHeaderField :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSString value, IsNSString field) => nsMutableURLRequest -> value -> field -> IO ()
setValue_forHTTPHeaderField nsMutableURLRequest  value field =
withObjCPtr value $ \raw_value ->
  withObjCPtr field $ \raw_field ->
      sendMsg nsMutableURLRequest (mkSelector "setValue:forHTTPHeaderField:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_field :: Ptr ())]

-- | addValue:forHTTPHeaderField:
--
-- Adds an HTTP header field in the current header    dictionary.
--
-- This method provides a way to add values to header    fields incrementally. If a value was previously set for the given    header field, the given value is appended to the previously-existing    value. The appropriate field delimiter, a comma in the case of HTTP,    is added by the implementation, and should not be added to the given    value by the caller. Note that, in keeping with the HTTP RFC, HTTP    header field names are case-insensitive.
--
-- @value@ — the header field value.
--
-- @field@ — the header field name (case-insensitive).
--
-- ObjC selector: @- addValue:forHTTPHeaderField:@
addValue_forHTTPHeaderField :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSString value, IsNSString field) => nsMutableURLRequest -> value -> field -> IO ()
addValue_forHTTPHeaderField nsMutableURLRequest  value field =
withObjCPtr value $ \raw_value ->
  withObjCPtr field $ \raw_field ->
      sendMsg nsMutableURLRequest (mkSelector "addValue:forHTTPHeaderField:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_field :: Ptr ())]

-- | The URL of the receiver.
--
-- ObjC selector: @- URL@
url :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSURL)
url nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL of the receiver.
--
-- ObjC selector: @- setURL:@
setURL :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSURL value) => nsMutableURLRequest -> value -> IO ()
setURL nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The cache policy of the receiver.
--
-- ObjC selector: @- cachePolicy@
cachePolicy :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO NSURLRequestCachePolicy
cachePolicy nsMutableURLRequest  =
  fmap (coerce :: CULong -> NSURLRequestCachePolicy) $ sendMsg nsMutableURLRequest (mkSelector "cachePolicy") retCULong []

-- | The cache policy of the receiver.
--
-- ObjC selector: @- setCachePolicy:@
setCachePolicy :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> NSURLRequestCachePolicy -> IO ()
setCachePolicy nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setCachePolicy:") retVoid [argCULong (coerce value)]

-- | Sets the timeout interval of the receiver.
--
-- The timeout interval specifies the limit on the idle    interval allotted to a request in the process of loading. The "idle    interval" is defined as the period of time that has passed since the    last instance of load activity occurred for a request that is in the    process of loading. Hence, when an instance of load activity occurs    (e.g. bytes are received from the network for a request), the idle    interval for a request is reset to 0. If the idle interval ever    becomes greater than or equal to the timeout interval, the request    is considered to have timed out. This timeout interval is measured    in seconds.
--
-- ObjC selector: @- timeoutInterval@
timeoutInterval :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO CDouble
timeoutInterval nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "timeoutInterval") retCDouble []

-- | Sets the timeout interval of the receiver.
--
-- The timeout interval specifies the limit on the idle    interval allotted to a request in the process of loading. The "idle    interval" is defined as the period of time that has passed since the    last instance of load activity occurred for a request that is in the    process of loading. Hence, when an instance of load activity occurs    (e.g. bytes are received from the network for a request), the idle    interval for a request is reset to 0. If the idle interval ever    becomes greater than or equal to the timeout interval, the request    is considered to have timed out. This timeout interval is measured    in seconds.
--
-- ObjC selector: @- setTimeoutInterval:@
setTimeoutInterval :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> CDouble -> IO ()
setTimeoutInterval nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setTimeoutInterval:") retVoid [argCDouble (fromIntegral value)]

-- | Sets the main document URL
--
-- The caller should pass the URL for an appropriate main    document, if known. For example, when loading a web page, the URL    of the main html document for the top-level frame should be    passed.  This main document is used to implement the cookie "only    from same domain as main document" policy, attributing this request    as a sub-resource of a user-specified URL, and possibly other things    in the future.
--
-- ObjC selector: @- mainDocumentURL@
mainDocumentURL :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSURL)
mainDocumentURL nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "mainDocumentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the main document URL
--
-- The caller should pass the URL for an appropriate main    document, if known. For example, when loading a web page, the URL    of the main html document for the top-level frame should be    passed.  This main document is used to implement the cookie "only    from same domain as main document" policy, attributing this request    as a sub-resource of a user-specified URL, and possibly other things    in the future.
--
-- ObjC selector: @- setMainDocumentURL:@
setMainDocumentURL :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSURL value) => nsMutableURLRequest -> value -> IO ()
setMainDocumentURL nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setMainDocumentURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the NSURLRequestNetworkServiceType to associate with this request
--
-- This method is used to provide the network layers with a hint as to the purpose of the request.  Most clients should not need to use this method.
--
-- ObjC selector: @- networkServiceType@
networkServiceType :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO NSURLRequestNetworkServiceType
networkServiceType nsMutableURLRequest  =
  fmap (coerce :: CULong -> NSURLRequestNetworkServiceType) $ sendMsg nsMutableURLRequest (mkSelector "networkServiceType") retCULong []

-- | Sets the NSURLRequestNetworkServiceType to associate with this request
--
-- This method is used to provide the network layers with a hint as to the purpose of the request.  Most clients should not need to use this method.
--
-- ObjC selector: @- setNetworkServiceType:@
setNetworkServiceType :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> NSURLRequestNetworkServiceType -> IO ()
setNetworkServiceType nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setNetworkServiceType:") retVoid [argCULong (coerce value)]

-- | sets whether a connection created with this request is allowed to use the built in cellular radios (if present).
--
-- NO if the receiver should not be allowed to use the built in cellular radios to satisfy the request, YES otherwise.  The default is YES.
--
-- ObjC selector: @- allowsCellularAccess@
allowsCellularAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
allowsCellularAccess nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "allowsCellularAccess") retCULong []

-- | sets whether a connection created with this request is allowed to use the built in cellular radios (if present).
--
-- NO if the receiver should not be allowed to use the built in cellular radios to satisfy the request, YES otherwise.  The default is YES.
--
-- ObjC selector: @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAllowsCellularAccess nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAllowsCellularAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as expensive.
--
-- NO if the receiver should not be allowed to use an interface marked as expensive to satisfy the request, YES otherwise.
--
-- ObjC selector: @- allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
allowsExpensiveNetworkAccess nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "allowsExpensiveNetworkAccess") retCULong []

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as expensive.
--
-- NO if the receiver should not be allowed to use an interface marked as expensive to satisfy the request, YES otherwise.
--
-- ObjC selector: @- setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAllowsExpensiveNetworkAccess nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAllowsExpensiveNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as constrained.
--
-- NO if the receiver should not be allowed to use an interface marked as constrained to satisfy the request, YES otherwise.
--
-- ObjC selector: @- allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
allowsConstrainedNetworkAccess nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "allowsConstrainedNetworkAccess") retCULong []

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as constrained.
--
-- NO if the receiver should not be allowed to use an interface marked as constrained to satisfy the request, YES otherwise.
--
-- ObjC selector: @- setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAllowsConstrainedNetworkAccess nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAllowsConstrainedNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as ultra constrained.
--
-- NO if the receiver should not be allowed to use an interface marked as ultra constrained to satisfy the request, YES otherwise.
--
-- ObjC selector: @- allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
allowsUltraConstrainedNetworkAccess nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "allowsUltraConstrainedNetworkAccess") retCULong []

-- | sets whether a connection created with this request is allowed to use network interfaces which have been marked as ultra constrained.
--
-- NO if the receiver should not be allowed to use an interface marked as ultra constrained to satisfy the request, YES otherwise.
--
-- ObjC selector: @- setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccess :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAllowsUltraConstrainedNetworkAccess nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAllowsUltraConstrainedNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | returns whether we assume that server supports HTTP/3. Enables QUIC racing without HTTP/3 service discovery.
--
-- Returns: YES if server endpoint is known to support HTTP/3. Defaults to NO. The default may be YES in a future OS update.
--
-- ObjC selector: @- assumesHTTP3Capable@
assumesHTTP3Capable :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
assumesHTTP3Capable nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "assumesHTTP3Capable") retCULong []

-- | returns whether we assume that server supports HTTP/3. Enables QUIC racing without HTTP/3 service discovery.
--
-- Returns: YES if server endpoint is known to support HTTP/3. Defaults to NO. The default may be YES in a future OS update.
--
-- ObjC selector: @- setAssumesHTTP3Capable:@
setAssumesHTTP3Capable :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAssumesHTTP3Capable nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAssumesHTTP3Capable:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the NSURLRequestAttribution to associate with this request.
--
-- Set to NSURLRequestAttributionUser if the URL was specified by the user. Defaults to NSURLRequestAttributionDeveloper.
--
-- ObjC selector: @- attribution@
attribution :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO NSURLRequestAttribution
attribution nsMutableURLRequest  =
  fmap (coerce :: CULong -> NSURLRequestAttribution) $ sendMsg nsMutableURLRequest (mkSelector "attribution") retCULong []

-- | Sets the NSURLRequestAttribution to associate with this request.
--
-- Set to NSURLRequestAttributionUser if the URL was specified by the user. Defaults to NSURLRequestAttributionDeveloper.
--
-- ObjC selector: @- setAttribution:@
setAttribution :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> NSURLRequestAttribution -> IO ()
setAttribution nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAttribution:") retVoid [argCULong (coerce value)]

-- | sets whether a request is required to do DNSSEC validation during DNS lookup.
--
-- YES, if the DNS lookup for this request should require DNSSEC validation, No otherwise. Defaults to NO.
--
-- ObjC selector: @- requiresDNSSECValidation@
requiresDNSSECValidation :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
requiresDNSSECValidation nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "requiresDNSSECValidation") retCULong []

-- | sets whether a request is required to do DNSSEC validation during DNS lookup.
--
-- YES, if the DNS lookup for this request should require DNSSEC validation, No otherwise. Defaults to NO.
--
-- ObjC selector: @- setRequiresDNSSECValidation:@
setRequiresDNSSECValidation :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setRequiresDNSSECValidation nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setRequiresDNSSECValidation:") retVoid [argCULong (if value then 1 else 0)]

-- | Allows storing and usage of DNS answers, potentially beyond TTL expiry, in a persistent per-process cache. This should only be set for hostnames whose resolutions are not expected to change across networks.
--
-- YES, if the DNS lookup for this request is allowed to use a persistent per-process cache, NO otherwise. Defaults to NO.
--
-- ObjC selector: @- allowsPersistentDNS@
allowsPersistentDNS :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
allowsPersistentDNS nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "allowsPersistentDNS") retCULong []

-- | Allows storing and usage of DNS answers, potentially beyond TTL expiry, in a persistent per-process cache. This should only be set for hostnames whose resolutions are not expected to change across networks.
--
-- YES, if the DNS lookup for this request is allowed to use a persistent per-process cache, NO otherwise. Defaults to NO.
--
-- ObjC selector: @- setAllowsPersistentDNS:@
setAllowsPersistentDNS :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setAllowsPersistentDNS nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setAllowsPersistentDNS:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cookiePartitionIdentifier@
cookiePartitionIdentifier :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSString)
cookiePartitionIdentifier nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "cookiePartitionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCookiePartitionIdentifier:@
setCookiePartitionIdentifier :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSString value) => nsMutableURLRequest -> value -> IO ()
setCookiePartitionIdentifier nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setCookiePartitionIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the HTTP request method of the receiver.
--
-- ObjC selector: @- HTTPMethod@
httpMethod :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSString)
httpMethod nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "HTTPMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the HTTP request method of the receiver.
--
-- ObjC selector: @- setHTTPMethod:@
setHTTPMethod :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSString value) => nsMutableURLRequest -> value -> IO ()
setHTTPMethod nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setHTTPMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the HTTP header fields of the receiver to the given    dictionary.
--
-- This method replaces all header fields that may have    existed before this method call.     Since HTTP header fields must be string values, each object and    key in the dictionary passed to this method must answer YES when    sent an -isKindOfClass:[NSString class] message. If either    the key or value for a key-value pair answers NO when sent this    message, the key-value pair is skipped.
--
-- ObjC selector: @- allHTTPHeaderFields@
allHTTPHeaderFields :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSDictionary)
allHTTPHeaderFields nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "allHTTPHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the HTTP header fields of the receiver to the given    dictionary.
--
-- This method replaces all header fields that may have    existed before this method call.     Since HTTP header fields must be string values, each object and    key in the dictionary passed to this method must answer YES when    sent an -isKindOfClass:[NSString class] message. If either    the key or value for a key-value pair answers NO when sent this    message, the key-value pair is skipped.
--
-- ObjC selector: @- setAllHTTPHeaderFields:@
setAllHTTPHeaderFields :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSDictionary value) => nsMutableURLRequest -> value -> IO ()
setAllHTTPHeaderFields nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setAllHTTPHeaderFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the request body data of the receiver.
--
-- This data is sent as the message body of the request, as    in done in an HTTP POST request.
--
-- ObjC selector: @- HTTPBody@
httpBody :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSData)
httpBody nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "HTTPBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the request body data of the receiver.
--
-- This data is sent as the message body of the request, as    in done in an HTTP POST request.
--
-- ObjC selector: @- setHTTPBody:@
setHTTPBody :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSData value) => nsMutableURLRequest -> value -> IO ()
setHTTPBody nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setHTTPBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the request body to be the contents of the given stream.
--
-- The provided stream should be unopened; the request will take    over the stream's delegate.  The entire stream's contents will be     transmitted as the HTTP body of the request.  Note that the body stream    and the body data (set by setHTTPBody:, above) are mutually exclusive     - setting one will clear the other.
--
-- ObjC selector: @- HTTPBodyStream@
httpBodyStream :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO (Id NSInputStream)
httpBodyStream nsMutableURLRequest  =
  sendMsg nsMutableURLRequest (mkSelector "HTTPBodyStream") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the request body to be the contents of the given stream.
--
-- The provided stream should be unopened; the request will take    over the stream's delegate.  The entire stream's contents will be     transmitted as the HTTP body of the request.  Note that the body stream    and the body data (set by setHTTPBody:, above) are mutually exclusive     - setting one will clear the other.
--
-- ObjC selector: @- setHTTPBodyStream:@
setHTTPBodyStream :: (IsNSMutableURLRequest nsMutableURLRequest, IsNSInputStream value) => nsMutableURLRequest -> value -> IO ()
setHTTPBodyStream nsMutableURLRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableURLRequest (mkSelector "setHTTPBodyStream:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Decide whether default cookie handling will happen for     this request (YES if cookies should be sent with and set for this request;    otherwise NO).
--
-- The default is YES - in other words, cookies are sent from and     stored to the cookie manager by default.    NOTE: In releases prior to 10.3, this value is ignored
--
-- ObjC selector: @- HTTPShouldHandleCookies@
httpShouldHandleCookies :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
httpShouldHandleCookies nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "HTTPShouldHandleCookies") retCULong []

-- | Decide whether default cookie handling will happen for     this request (YES if cookies should be sent with and set for this request;    otherwise NO).
--
-- The default is YES - in other words, cookies are sent from and     stored to the cookie manager by default.    NOTE: In releases prior to 10.3, this value is ignored
--
-- ObjC selector: @- setHTTPShouldHandleCookies:@
setHTTPShouldHandleCookies :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setHTTPShouldHandleCookies nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setHTTPShouldHandleCookies:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets whether the request should not wait for the previous response  before transmitting (YES if the receiver should transmit before the previous response is received.  NO to wait for the previous response before transmitting)
--
-- Calling this method with a YES value does not guarantee HTTP  pipelining behavior.  This method may have no effect if an HTTP proxy is configured, or if the HTTP request uses an unsafe request method (e.g., POST requests will not pipeline).  Pipelining behavior also may not begin until the second request on a given TCP connection.  There may be other situations where pipelining does not occur even though YES was set. HTTP 1.1 allows the client to send multiple requests to the server without waiting for a response.  Though HTTP 1.1 requires support for pipelining, some servers report themselves as being HTTP 1.1 but do not support pipelining (disconnecting, sending resources misordered, omitting part of a resource, etc.).
--
-- ObjC selector: @- HTTPShouldUsePipelining@
httpShouldUsePipelining :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> IO Bool
httpShouldUsePipelining nsMutableURLRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableURLRequest (mkSelector "HTTPShouldUsePipelining") retCULong []

-- | Sets whether the request should not wait for the previous response  before transmitting (YES if the receiver should transmit before the previous response is received.  NO to wait for the previous response before transmitting)
--
-- Calling this method with a YES value does not guarantee HTTP  pipelining behavior.  This method may have no effect if an HTTP proxy is configured, or if the HTTP request uses an unsafe request method (e.g., POST requests will not pipeline).  Pipelining behavior also may not begin until the second request on a given TCP connection.  There may be other situations where pipelining does not occur even though YES was set. HTTP 1.1 allows the client to send multiple requests to the server without waiting for a response.  Though HTTP 1.1 requires support for pipelining, some servers report themselves as being HTTP 1.1 but do not support pipelining (disconnecting, sending resources misordered, omitting part of a resource, etc.).
--
-- ObjC selector: @- setHTTPShouldUsePipelining:@
setHTTPShouldUsePipelining :: IsNSMutableURLRequest nsMutableURLRequest => nsMutableURLRequest -> Bool -> IO ()
setHTTPShouldUsePipelining nsMutableURLRequest  value =
  sendMsg nsMutableURLRequest (mkSelector "setHTTPShouldUsePipelining:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValue:forHTTPHeaderField:@
setValue_forHTTPHeaderFieldSelector :: Selector
setValue_forHTTPHeaderFieldSelector = mkSelector "setValue:forHTTPHeaderField:"

-- | @Selector@ for @addValue:forHTTPHeaderField:@
addValue_forHTTPHeaderFieldSelector :: Selector
addValue_forHTTPHeaderFieldSelector = mkSelector "addValue:forHTTPHeaderField:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @cachePolicy@
cachePolicySelector :: Selector
cachePolicySelector = mkSelector "cachePolicy"

-- | @Selector@ for @setCachePolicy:@
setCachePolicySelector :: Selector
setCachePolicySelector = mkSelector "setCachePolicy:"

-- | @Selector@ for @timeoutInterval@
timeoutIntervalSelector :: Selector
timeoutIntervalSelector = mkSelector "timeoutInterval"

-- | @Selector@ for @setTimeoutInterval:@
setTimeoutIntervalSelector :: Selector
setTimeoutIntervalSelector = mkSelector "setTimeoutInterval:"

-- | @Selector@ for @mainDocumentURL@
mainDocumentURLSelector :: Selector
mainDocumentURLSelector = mkSelector "mainDocumentURL"

-- | @Selector@ for @setMainDocumentURL:@
setMainDocumentURLSelector :: Selector
setMainDocumentURLSelector = mkSelector "setMainDocumentURL:"

-- | @Selector@ for @networkServiceType@
networkServiceTypeSelector :: Selector
networkServiceTypeSelector = mkSelector "networkServiceType"

-- | @Selector@ for @setNetworkServiceType:@
setNetworkServiceTypeSelector :: Selector
setNetworkServiceTypeSelector = mkSelector "setNetworkServiceType:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccessSelector :: Selector
allowsExpensiveNetworkAccessSelector = mkSelector "allowsExpensiveNetworkAccess"

-- | @Selector@ for @setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccessSelector :: Selector
setAllowsExpensiveNetworkAccessSelector = mkSelector "setAllowsExpensiveNetworkAccess:"

-- | @Selector@ for @allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccessSelector :: Selector
allowsConstrainedNetworkAccessSelector = mkSelector "allowsConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccessSelector :: Selector
setAllowsConstrainedNetworkAccessSelector = mkSelector "setAllowsConstrainedNetworkAccess:"

-- | @Selector@ for @allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccessSelector :: Selector
allowsUltraConstrainedNetworkAccessSelector = mkSelector "allowsUltraConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccessSelector :: Selector
setAllowsUltraConstrainedNetworkAccessSelector = mkSelector "setAllowsUltraConstrainedNetworkAccess:"

-- | @Selector@ for @assumesHTTP3Capable@
assumesHTTP3CapableSelector :: Selector
assumesHTTP3CapableSelector = mkSelector "assumesHTTP3Capable"

-- | @Selector@ for @setAssumesHTTP3Capable:@
setAssumesHTTP3CapableSelector :: Selector
setAssumesHTTP3CapableSelector = mkSelector "setAssumesHTTP3Capable:"

-- | @Selector@ for @attribution@
attributionSelector :: Selector
attributionSelector = mkSelector "attribution"

-- | @Selector@ for @setAttribution:@
setAttributionSelector :: Selector
setAttributionSelector = mkSelector "setAttribution:"

-- | @Selector@ for @requiresDNSSECValidation@
requiresDNSSECValidationSelector :: Selector
requiresDNSSECValidationSelector = mkSelector "requiresDNSSECValidation"

-- | @Selector@ for @setRequiresDNSSECValidation:@
setRequiresDNSSECValidationSelector :: Selector
setRequiresDNSSECValidationSelector = mkSelector "setRequiresDNSSECValidation:"

-- | @Selector@ for @allowsPersistentDNS@
allowsPersistentDNSSelector :: Selector
allowsPersistentDNSSelector = mkSelector "allowsPersistentDNS"

-- | @Selector@ for @setAllowsPersistentDNS:@
setAllowsPersistentDNSSelector :: Selector
setAllowsPersistentDNSSelector = mkSelector "setAllowsPersistentDNS:"

-- | @Selector@ for @cookiePartitionIdentifier@
cookiePartitionIdentifierSelector :: Selector
cookiePartitionIdentifierSelector = mkSelector "cookiePartitionIdentifier"

-- | @Selector@ for @setCookiePartitionIdentifier:@
setCookiePartitionIdentifierSelector :: Selector
setCookiePartitionIdentifierSelector = mkSelector "setCookiePartitionIdentifier:"

-- | @Selector@ for @HTTPMethod@
httpMethodSelector :: Selector
httpMethodSelector = mkSelector "HTTPMethod"

-- | @Selector@ for @setHTTPMethod:@
setHTTPMethodSelector :: Selector
setHTTPMethodSelector = mkSelector "setHTTPMethod:"

-- | @Selector@ for @allHTTPHeaderFields@
allHTTPHeaderFieldsSelector :: Selector
allHTTPHeaderFieldsSelector = mkSelector "allHTTPHeaderFields"

-- | @Selector@ for @setAllHTTPHeaderFields:@
setAllHTTPHeaderFieldsSelector :: Selector
setAllHTTPHeaderFieldsSelector = mkSelector "setAllHTTPHeaderFields:"

-- | @Selector@ for @HTTPBody@
httpBodySelector :: Selector
httpBodySelector = mkSelector "HTTPBody"

-- | @Selector@ for @setHTTPBody:@
setHTTPBodySelector :: Selector
setHTTPBodySelector = mkSelector "setHTTPBody:"

-- | @Selector@ for @HTTPBodyStream@
httpBodyStreamSelector :: Selector
httpBodyStreamSelector = mkSelector "HTTPBodyStream"

-- | @Selector@ for @setHTTPBodyStream:@
setHTTPBodyStreamSelector :: Selector
setHTTPBodyStreamSelector = mkSelector "setHTTPBodyStream:"

-- | @Selector@ for @HTTPShouldHandleCookies@
httpShouldHandleCookiesSelector :: Selector
httpShouldHandleCookiesSelector = mkSelector "HTTPShouldHandleCookies"

-- | @Selector@ for @setHTTPShouldHandleCookies:@
setHTTPShouldHandleCookiesSelector :: Selector
setHTTPShouldHandleCookiesSelector = mkSelector "setHTTPShouldHandleCookies:"

-- | @Selector@ for @HTTPShouldUsePipelining@
httpShouldUsePipeliningSelector :: Selector
httpShouldUsePipeliningSelector = mkSelector "HTTPShouldUsePipelining"

-- | @Selector@ for @setHTTPShouldUsePipelining:@
setHTTPShouldUsePipeliningSelector :: Selector
setHTTPShouldUsePipeliningSelector = mkSelector "setHTTPShouldUsePipelining:"

