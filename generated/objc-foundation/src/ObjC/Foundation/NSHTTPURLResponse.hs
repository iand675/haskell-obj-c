{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSHTTPURLResponse
--
-- An NSHTTPURLResponse object represents a response to an    HTTP URL load. It is a specialization of NSURLResponse which    provides conveniences for accessing information specific to HTTP    protocol responses.
--
-- Generated bindings for @NSHTTPURLResponse@.
module ObjC.Foundation.NSHTTPURLResponse
  ( NSHTTPURLResponse
  , IsNSHTTPURLResponse(..)
  , initWithURL_statusCode_HTTPVersion_headerFields
  , valueForHTTPHeaderField
  , localizedStringForStatusCode
  , statusCode
  , allHeaderFields
  , initWithURL_statusCode_HTTPVersion_headerFieldsSelector
  , valueForHTTPHeaderFieldSelector
  , localizedStringForStatusCodeSelector
  , statusCodeSelector
  , allHeaderFieldsSelector


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

-- | initWithURL:statusCode:HTTPVersion:headerFields:
--
-- initializer for NSHTTPURLResponse objects.
--
-- @url@ — the URL from which the response was generated.
--
-- @statusCode@ — an HTTP status code.
--
-- @HTTPVersion@ — The version of the HTTP response as represented by the server.  This is typically represented as "HTTP/1.1".
--
-- @headerFields@ — A dictionary representing the header keys and values of the server response.
--
-- Returns: the instance of the object, or NULL if an error occurred during initialization.
--
-- This API was introduced in Mac OS X 10.7.2 and iOS 5.0 and is not available prior to those releases.
--
-- ObjC selector: @- initWithURL:statusCode:HTTPVersion:headerFields:@
initWithURL_statusCode_HTTPVersion_headerFields :: (IsNSHTTPURLResponse nshttpurlResponse, IsNSURL url, IsNSString httpVersion, IsNSDictionary headerFields) => nshttpurlResponse -> url -> CLong -> httpVersion -> headerFields -> IO (Id NSHTTPURLResponse)
initWithURL_statusCode_HTTPVersion_headerFields nshttpurlResponse  url statusCode httpVersion headerFields =
withObjCPtr url $ \raw_url ->
  withObjCPtr httpVersion $ \raw_httpVersion ->
    withObjCPtr headerFields $ \raw_headerFields ->
        sendMsg nshttpurlResponse (mkSelector "initWithURL:statusCode:HTTPVersion:headerFields:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCLong (fromIntegral statusCode), argPtr (castPtr raw_httpVersion :: Ptr ()), argPtr (castPtr raw_headerFields :: Ptr ())] >>= ownedObject . castPtr

-- | valueForHTTPHeaderField:
--
-- Returns the value which corresponds to the given header    field. Note that, in keeping with the HTTP RFC, HTTP header field    names are case-insensitive.
--
-- @field@ — the header field name to use for the lookup    (case-insensitive).
--
-- Returns: the value associated with the given header field, or nil if    there is no value associated with the given header field.
--
-- ObjC selector: @- valueForHTTPHeaderField:@
valueForHTTPHeaderField :: (IsNSHTTPURLResponse nshttpurlResponse, IsNSString field) => nshttpurlResponse -> field -> IO (Id NSString)
valueForHTTPHeaderField nshttpurlResponse  field =
withObjCPtr field $ \raw_field ->
    sendMsg nshttpurlResponse (mkSelector "valueForHTTPHeaderField:") (retPtr retVoid) [argPtr (castPtr raw_field :: Ptr ())] >>= retainedObject . castPtr

-- | localizedStringForStatusCode:
--
-- Convenience method which returns a localized string    corresponding to the status code for this response.
--
-- @statusCode@ — the status code to use to produce a localized string.
--
-- Returns: A localized string corresponding to the given status code.
--
-- ObjC selector: @+ localizedStringForStatusCode:@
localizedStringForStatusCode :: CLong -> IO (Id NSString)
localizedStringForStatusCode statusCode =
  do
    cls' <- getRequiredClass "NSHTTPURLResponse"
    sendClassMsg cls' (mkSelector "localizedStringForStatusCode:") (retPtr retVoid) [argCLong (fromIntegral statusCode)] >>= retainedObject . castPtr

-- | Returns the HTTP status code of the receiver.
--
-- Returns: The HTTP status code of the receiver.
--
-- ObjC selector: @- statusCode@
statusCode :: IsNSHTTPURLResponse nshttpurlResponse => nshttpurlResponse -> IO CLong
statusCode nshttpurlResponse  =
  sendMsg nshttpurlResponse (mkSelector "statusCode") retCLong []

-- | Returns a dictionary containing all the HTTP header fields    of the receiver.
--
-- By examining this header dictionary, clients can see    the "raw" header information which was reported to the protocol    implementation by the HTTP server. This may be of use to    sophisticated or special-purpose HTTP clients.
--
-- Returns: A dictionary containing all the HTTP header fields of the    receiver.
--
-- ObjC selector: @- allHeaderFields@
allHeaderFields :: IsNSHTTPURLResponse nshttpurlResponse => nshttpurlResponse -> IO (Id NSDictionary)
allHeaderFields nshttpurlResponse  =
  sendMsg nshttpurlResponse (mkSelector "allHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:statusCode:HTTPVersion:headerFields:@
initWithURL_statusCode_HTTPVersion_headerFieldsSelector :: Selector
initWithURL_statusCode_HTTPVersion_headerFieldsSelector = mkSelector "initWithURL:statusCode:HTTPVersion:headerFields:"

-- | @Selector@ for @valueForHTTPHeaderField:@
valueForHTTPHeaderFieldSelector :: Selector
valueForHTTPHeaderFieldSelector = mkSelector "valueForHTTPHeaderField:"

-- | @Selector@ for @localizedStringForStatusCode:@
localizedStringForStatusCodeSelector :: Selector
localizedStringForStatusCodeSelector = mkSelector "localizedStringForStatusCode:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @allHeaderFields@
allHeaderFieldsSelector :: Selector
allHeaderFieldsSelector = mkSelector "allHeaderFields"

