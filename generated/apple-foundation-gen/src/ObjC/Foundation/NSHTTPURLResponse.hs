{-# LANGUAGE DataKinds #-}
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
  , allHeaderFieldsSelector
  , initWithURL_statusCode_HTTPVersion_headerFieldsSelector
  , localizedStringForStatusCodeSelector
  , statusCodeSelector
  , valueForHTTPHeaderFieldSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithURL_statusCode_HTTPVersion_headerFields nshttpurlResponse url statusCode httpVersion headerFields =
  sendOwnedMessage nshttpurlResponse initWithURL_statusCode_HTTPVersion_headerFieldsSelector (toNSURL url) statusCode (toNSString httpVersion) (toNSDictionary headerFields)

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
valueForHTTPHeaderField nshttpurlResponse field =
  sendMessage nshttpurlResponse valueForHTTPHeaderFieldSelector (toNSString field)

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
    sendClassMessage cls' localizedStringForStatusCodeSelector statusCode

-- | Returns the HTTP status code of the receiver.
--
-- Returns: The HTTP status code of the receiver.
--
-- ObjC selector: @- statusCode@
statusCode :: IsNSHTTPURLResponse nshttpurlResponse => nshttpurlResponse -> IO CLong
statusCode nshttpurlResponse =
  sendMessage nshttpurlResponse statusCodeSelector

-- | Returns a dictionary containing all the HTTP header fields    of the receiver.
--
-- By examining this header dictionary, clients can see    the "raw" header information which was reported to the protocol    implementation by the HTTP server. This may be of use to    sophisticated or special-purpose HTTP clients.
--
-- Returns: A dictionary containing all the HTTP header fields of the    receiver.
--
-- ObjC selector: @- allHeaderFields@
allHeaderFields :: IsNSHTTPURLResponse nshttpurlResponse => nshttpurlResponse -> IO (Id NSDictionary)
allHeaderFields nshttpurlResponse =
  sendMessage nshttpurlResponse allHeaderFieldsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:statusCode:HTTPVersion:headerFields:@
initWithURL_statusCode_HTTPVersion_headerFieldsSelector :: Selector '[Id NSURL, CLong, Id NSString, Id NSDictionary] (Id NSHTTPURLResponse)
initWithURL_statusCode_HTTPVersion_headerFieldsSelector = mkSelector "initWithURL:statusCode:HTTPVersion:headerFields:"

-- | @Selector@ for @valueForHTTPHeaderField:@
valueForHTTPHeaderFieldSelector :: Selector '[Id NSString] (Id NSString)
valueForHTTPHeaderFieldSelector = mkSelector "valueForHTTPHeaderField:"

-- | @Selector@ for @localizedStringForStatusCode:@
localizedStringForStatusCodeSelector :: Selector '[CLong] (Id NSString)
localizedStringForStatusCodeSelector = mkSelector "localizedStringForStatusCode:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] CLong
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @allHeaderFields@
allHeaderFieldsSelector :: Selector '[] (Id NSDictionary)
allHeaderFieldsSelector = mkSelector "allHeaderFields"

