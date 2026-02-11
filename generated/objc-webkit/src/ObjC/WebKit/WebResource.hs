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
  , initWithData_URL_MIMEType_textEncodingName_frameNameSelector
  , dataSelector
  , urlSelector
  , mimeTypeSelector
  , textEncodingNameSelector
  , frameNameSelector


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
initWithData_URL_MIMEType_textEncodingName_frameName webResource  data_ url mimeType textEncodingName frameName =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr mimeType $ \raw_mimeType ->
      withObjCPtr textEncodingName $ \raw_textEncodingName ->
        withObjCPtr frameName $ \raw_frameName ->
            sendMsg webResource (mkSelector "initWithData:URL:MIMEType:textEncodingName:frameName:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_mimeType :: Ptr ()), argPtr (castPtr raw_textEncodingName :: Ptr ()), argPtr (castPtr raw_frameName :: Ptr ())] >>= ownedObject . castPtr

-- | data
--
-- The data of the resource.
--
-- ObjC selector: @- data@
data_ :: IsWebResource webResource => webResource -> IO (Id NSData)
data_ webResource  =
  sendMsg webResource (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL
--
-- The URL of the resource.
--
-- ObjC selector: @- URL@
url :: IsWebResource webResource => webResource -> IO (Id NSURL)
url webResource  =
  sendMsg webResource (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | MIMEType
--
-- The MIME type of the resource.
--
-- ObjC selector: @- MIMEType@
mimeType :: IsWebResource webResource => webResource -> IO (Id NSString)
mimeType webResource  =
  sendMsg webResource (mkSelector "MIMEType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textEncodingName
--
-- The text encoding name of the resource (can be nil).
--
-- ObjC selector: @- textEncodingName@
textEncodingName :: IsWebResource webResource => webResource -> IO (Id NSString)
textEncodingName webResource  =
  sendMsg webResource (mkSelector "textEncodingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | frameName
--
-- The frame name of the resource if the resource represents the contents of an entire HTML frame (can be nil).
--
-- ObjC selector: @- frameName@
frameName :: IsWebResource webResource => webResource -> IO (Id NSString)
frameName webResource  =
  sendMsg webResource (mkSelector "frameName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:URL:MIMEType:textEncodingName:frameName:@
initWithData_URL_MIMEType_textEncodingName_frameNameSelector :: Selector
initWithData_URL_MIMEType_textEncodingName_frameNameSelector = mkSelector "initWithData:URL:MIMEType:textEncodingName:frameName:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @MIMEType@
mimeTypeSelector :: Selector
mimeTypeSelector = mkSelector "MIMEType"

-- | @Selector@ for @textEncodingName@
textEncodingNameSelector :: Selector
textEncodingNameSelector = mkSelector "textEncodingName"

-- | @Selector@ for @frameName@
frameNameSelector :: Selector
frameNameSelector = mkSelector "frameName"

