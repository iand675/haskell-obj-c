{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebArchive
--
-- WebArchive represents a main resource as well as all the subresources and subframes associated with the main resource.    The main resource can be an entire web page, a portion of a web page, or some other kind of data such as an image.    This class can be used for saving standalone web pages, representing portions of a web page on the pasteboard, or any other    application where one class is needed to represent rich web content.
--
-- Generated bindings for @WebArchive@.
module ObjC.WebKit.WebArchive
  ( WebArchive
  , IsWebArchive(..)
  , initWithMainResource_subresources_subframeArchives
  , initWithData
  , mainResource
  , subresources
  , subframeArchives
  , data_
  , initWithMainResource_subresources_subframeArchivesSelector
  , initWithDataSelector
  , mainResourceSelector
  , subresourcesSelector
  , subframeArchivesSelector
  , dataSelector


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

-- | initWithMainResource:subresources:subframeArchives:
--
-- The initializer for WebArchive.
--
-- @mainResource@ — The main resource of the archive.
--
-- @subresources@ — The subresources of the archive (can be nil).
--
-- @subframeArchives@ — The archives representing the subframes of the archive (can be nil).
--
-- Returns: An initialized WebArchive.
--
-- ObjC selector: @- initWithMainResource:subresources:subframeArchives:@
initWithMainResource_subresources_subframeArchives :: (IsWebArchive webArchive, IsWebResource mainResource, IsNSArray subresources, IsNSArray subframeArchives) => webArchive -> mainResource -> subresources -> subframeArchives -> IO (Id WebArchive)
initWithMainResource_subresources_subframeArchives webArchive  mainResource subresources subframeArchives =
withObjCPtr mainResource $ \raw_mainResource ->
  withObjCPtr subresources $ \raw_subresources ->
    withObjCPtr subframeArchives $ \raw_subframeArchives ->
        sendMsg webArchive (mkSelector "initWithMainResource:subresources:subframeArchives:") (retPtr retVoid) [argPtr (castPtr raw_mainResource :: Ptr ()), argPtr (castPtr raw_subresources :: Ptr ()), argPtr (castPtr raw_subframeArchives :: Ptr ())] >>= ownedObject . castPtr

-- | initWithData:
--
-- The initializer for creating a WebArchive from data.
--
-- @data@ — The data representing the archive. This can be obtained using WebArchive's data method.
--
-- Returns: An initialized WebArchive.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsWebArchive webArchive, IsNSData data_) => webArchive -> data_ -> IO (Id WebArchive)
initWithData webArchive  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg webArchive (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | mainResource
--
-- The main resource of the archive.
--
-- ObjC selector: @- mainResource@
mainResource :: IsWebArchive webArchive => webArchive -> IO (Id WebResource)
mainResource webArchive  =
  sendMsg webArchive (mkSelector "mainResource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subresources
--
-- The subresource of the archive (can be nil).
--
-- ObjC selector: @- subresources@
subresources :: IsWebArchive webArchive => webArchive -> IO (Id NSArray)
subresources webArchive  =
  sendMsg webArchive (mkSelector "subresources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subframeArchives
--
-- The archives representing the subframes of the archive (can be nil).
--
-- ObjC selector: @- subframeArchives@
subframeArchives :: IsWebArchive webArchive => webArchive -> IO (Id NSArray)
subframeArchives webArchive  =
  sendMsg webArchive (mkSelector "subframeArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The data representation of the archive.
--
-- The data returned by this method can be used to save a web archive to a file or to place a web archive on the pasteboard    using WebArchivePboardType. To create a WebArchive using the returned data, call initWithData:.
--
-- ObjC selector: @- data@
data_ :: IsWebArchive webArchive => webArchive -> IO (Id NSData)
data_ webArchive  =
  sendMsg webArchive (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMainResource:subresources:subframeArchives:@
initWithMainResource_subresources_subframeArchivesSelector :: Selector
initWithMainResource_subresources_subframeArchivesSelector = mkSelector "initWithMainResource:subresources:subframeArchives:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @mainResource@
mainResourceSelector :: Selector
mainResourceSelector = mkSelector "mainResource"

-- | @Selector@ for @subresources@
subresourcesSelector :: Selector
subresourcesSelector = mkSelector "subresources"

-- | @Selector@ for @subframeArchives@
subframeArchivesSelector :: Selector
subframeArchivesSelector = mkSelector "subframeArchives"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

