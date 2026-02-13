{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebResource
--
-- A WebResource represents a fully downloaded URL.     It includes the data of the resource as well as the metadata associated with the resource.
--
-- Generated bindings for @WebResource@.
module ObjC.WebKit.WebResource
  ( WebResource
  , IsWebResource(..)
  , initWithData_URL_MIMEType_textEncodingName_frameName
  , data_
  , url
  , mimeType
  , textEncodingName
  , frameName
  , dataSelector
  , frameNameSelector
  , initWithData_URL_MIMEType_textEncodingName_frameNameSelector
  , mimeTypeSelector
  , textEncodingNameSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithData:URL:MIMEType:textEncodingName:frameName
--
-- The initializer for WebResource.
--
-- @data@ — The data of the resource.
--
-- @URL@ — The URL of the resource.
--
-- @MIMEType@ — The MIME type of the resource.
--
-- @textEncodingName@ — The text encoding name of the resource (can be nil).
--
-- @frameName@ — The frame name of the resource if the resource represents the contents of an entire HTML frame (can be nil).
--
-- Returns: An initialized WebResource.
--
-- ObjC selector: @- initWithData:URL:MIMEType:textEncodingName:frameName:@
initWithData_URL_MIMEType_textEncodingName_frameName :: (IsWebResource webResource, IsNSData data_, IsNSURL url, IsNSString mimeType, IsNSString textEncodingName, IsNSString frameName) => webResource -> data_ -> url -> mimeType -> textEncodingName -> frameName -> IO (Id WebResource)
initWithData_URL_MIMEType_textEncodingName_frameName webResource data_ url mimeType textEncodingName frameName =
  sendOwnedMessage webResource initWithData_URL_MIMEType_textEncodingName_frameNameSelector (toNSData data_) (toNSURL url) (toNSString mimeType) (toNSString textEncodingName) (toNSString frameName)

-- | data
--
-- The data of the resource.
--
-- ObjC selector: @- data@
data_ :: IsWebResource webResource => webResource -> IO (Id NSData)
data_ webResource =
  sendMessage webResource dataSelector

-- | URL
--
-- The URL of the resource.
--
-- ObjC selector: @- URL@
url :: IsWebResource webResource => webResource -> IO (Id NSURL)
url webResource =
  sendMessage webResource urlSelector

-- | MIMEType
--
-- The MIME type of the resource.
--
-- ObjC selector: @- MIMEType@
mimeType :: IsWebResource webResource => webResource -> IO (Id NSString)
mimeType webResource =
  sendMessage webResource mimeTypeSelector

-- | textEncodingName
--
-- The text encoding name of the resource (can be nil).
--
-- ObjC selector: @- textEncodingName@
textEncodingName :: IsWebResource webResource => webResource -> IO (Id NSString)
textEncodingName webResource =
  sendMessage webResource textEncodingNameSelector

-- | frameName
--
-- The frame name of the resource if the resource represents the contents of an entire HTML frame (can be nil).
--
-- ObjC selector: @- frameName@
frameName :: IsWebResource webResource => webResource -> IO (Id NSString)
frameName webResource =
  sendMessage webResource frameNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:URL:MIMEType:textEncodingName:frameName:@
initWithData_URL_MIMEType_textEncodingName_frameNameSelector :: Selector '[Id NSData, Id NSURL, Id NSString, Id NSString, Id NSString] (Id WebResource)
initWithData_URL_MIMEType_textEncodingName_frameNameSelector = mkSelector "initWithData:URL:MIMEType:textEncodingName:frameName:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @MIMEType@
mimeTypeSelector :: Selector '[] (Id NSString)
mimeTypeSelector = mkSelector "MIMEType"

-- | @Selector@ for @textEncodingName@
textEncodingNameSelector :: Selector '[] (Id NSString)
textEncodingNameSelector = mkSelector "textEncodingName"

-- | @Selector@ for @frameName@
frameNameSelector :: Selector '[] (Id NSString)
frameNameSelector = mkSelector "frameName"

