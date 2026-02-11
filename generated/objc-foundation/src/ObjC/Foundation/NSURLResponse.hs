{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLResponse
--
-- An NSURLResponse object represents a URL load response in a    manner independent of protocol and URL scheme.
--
-- NSURLResponse encapsulates the metadata associated    with a URL load. Note that NSURLResponse objects do not contain    the actual bytes representing the content of a URL. See    NSURLConnection and NSURLConnectionDelegate for more information    about receiving the content data for a URL load.
--
-- Generated bindings for @NSURLResponse@.
module ObjC.Foundation.NSURLResponse
  ( NSURLResponse
  , IsNSURLResponse(..)
  , initWithURL_MIMEType_expectedContentLength_textEncodingName
  , url
  , mimeType
  , expectedContentLength
  , textEncodingName
  , suggestedFilename
  , initWithURL_MIMEType_expectedContentLength_textEncodingNameSelector
  , urlSelector
  , mimeTypeSelector
  , expectedContentLengthSelector
  , textEncodingNameSelector
  , suggestedFilenameSelector


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

-- | initWithURL:MIMEType:expectedContentLength:textEncodingName:
--
-- Initialize an NSURLResponse with the provided values.
--
-- @URL@ — the URL
--
-- @MIMEType@ — the MIME content type of the response
--
-- @length@ — the expected content length of the associated data
--
-- @name@ — the name of the text encoding for the associated data, if applicable, else nil
--
-- Returns: The initialized NSURLResponse.
--
-- This is the designated initializer for NSURLResponse.
--
-- ObjC selector: @- initWithURL:MIMEType:expectedContentLength:textEncodingName:@
initWithURL_MIMEType_expectedContentLength_textEncodingName :: (IsNSURLResponse nsurlResponse, IsNSURL url, IsNSString mimeType, IsNSString name) => nsurlResponse -> url -> mimeType -> CLong -> name -> IO (Id NSURLResponse)
initWithURL_MIMEType_expectedContentLength_textEncodingName nsurlResponse  url mimeType length_ name =
withObjCPtr url $ \raw_url ->
  withObjCPtr mimeType $ \raw_mimeType ->
    withObjCPtr name $ \raw_name ->
        sendMsg nsurlResponse (mkSelector "initWithURL:MIMEType:expectedContentLength:textEncodingName:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_mimeType :: Ptr ()), argCLong (fromIntegral length_), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | Returns the URL of the receiver.
--
-- Returns: The URL of the receiver.
--
-- ObjC selector: @- URL@
url :: IsNSURLResponse nsurlResponse => nsurlResponse -> IO (Id NSURL)
url nsurlResponse  =
  sendMsg nsurlResponse (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the MIME type of the receiver.
--
-- The MIME type is based on the information provided    from an origin source. However, that value may be changed or    corrected by a protocol implementation if it can be determined    that the origin server or source reported the information    incorrectly or imprecisely. An attempt to guess the MIME type may    be made if the origin source did not report any such information.
--
-- Returns: The MIME type of the receiver.
--
-- ObjC selector: @- MIMEType@
mimeType :: IsNSURLResponse nsurlResponse => nsurlResponse -> IO (Id NSString)
mimeType nsurlResponse  =
  sendMsg nsurlResponse (mkSelector "MIMEType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the expected content length of the receiver.
--
-- Some protocol implementations report a content length    as part of delivering load metadata, but not all protocols    guarantee the amount of data that will be delivered in actuality.    Hence, this method returns an expected amount. Clients should use    this value as an advisory, and should be prepared to deal with    either more or less data.
--
-- Returns: The expected content length of the receiver, or -1 if    there is no expectation that can be arrived at regarding expected    content length.
--
-- ObjC selector: @- expectedContentLength@
expectedContentLength :: IsNSURLResponse nsurlResponse => nsurlResponse -> IO CLong
expectedContentLength nsurlResponse  =
  sendMsg nsurlResponse (mkSelector "expectedContentLength") retCLong []

-- | Returns the name of the text encoding of the receiver.
--
-- This name will be the actual string reported by the    origin source during the course of performing a protocol-specific    URL load. Clients can inspect this string and convert it to an    NSStringEncoding or CFStringEncoding using the methods and    functions made available in the appropriate framework.
--
-- Returns: The name of the text encoding of the receiver, or nil if no    text encoding was specified.
--
-- ObjC selector: @- textEncodingName@
textEncodingName :: IsNSURLResponse nsurlResponse => nsurlResponse -> IO (Id NSString)
textEncodingName nsurlResponse  =
  sendMsg nsurlResponse (mkSelector "textEncodingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a suggested filename if the resource were saved to disk.
--
-- The method first checks if the server has specified a filename using the    content disposition header. If no valid filename is specified using that mechanism,    this method checks the last path component of the URL. If no valid filename can be    obtained using the last path component, this method uses the URL's host as the filename.    If the URL's host can't be converted to a valid filename, the filename "unknown" is used.    In most cases, this method appends the proper file extension based on the MIME type.    This method always returns a valid filename.
--
-- Returns: A suggested filename to use if saving the resource to disk.
--
-- ObjC selector: @- suggestedFilename@
suggestedFilename :: IsNSURLResponse nsurlResponse => nsurlResponse -> IO (Id NSString)
suggestedFilename nsurlResponse  =
  sendMsg nsurlResponse (mkSelector "suggestedFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:MIMEType:expectedContentLength:textEncodingName:@
initWithURL_MIMEType_expectedContentLength_textEncodingNameSelector :: Selector
initWithURL_MIMEType_expectedContentLength_textEncodingNameSelector = mkSelector "initWithURL:MIMEType:expectedContentLength:textEncodingName:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @MIMEType@
mimeTypeSelector :: Selector
mimeTypeSelector = mkSelector "MIMEType"

-- | @Selector@ for @expectedContentLength@
expectedContentLengthSelector :: Selector
expectedContentLengthSelector = mkSelector "expectedContentLength"

-- | @Selector@ for @textEncodingName@
textEncodingNameSelector :: Selector
textEncodingNameSelector = mkSelector "textEncodingName"

-- | @Selector@ for @suggestedFilename@
suggestedFilenameSelector :: Selector
suggestedFilenameSelector = mkSelector "suggestedFilename"

