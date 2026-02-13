{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , initWithDataSelector
  , initWithMainResource_subresources_subframeArchivesSelector
  , mainResourceSelector
  , subframeArchivesSelector
  , subresourcesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithMainResource_subresources_subframeArchives webArchive mainResource subresources subframeArchives =
  sendOwnedMessage webArchive initWithMainResource_subresources_subframeArchivesSelector (toWebResource mainResource) (toNSArray subresources) (toNSArray subframeArchives)

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
initWithData webArchive data_ =
  sendOwnedMessage webArchive initWithDataSelector (toNSData data_)

-- | mainResource
--
-- The main resource of the archive.
--
-- ObjC selector: @- mainResource@
mainResource :: IsWebArchive webArchive => webArchive -> IO (Id WebResource)
mainResource webArchive =
  sendMessage webArchive mainResourceSelector

-- | subresources
--
-- The subresource of the archive (can be nil).
--
-- ObjC selector: @- subresources@
subresources :: IsWebArchive webArchive => webArchive -> IO (Id NSArray)
subresources webArchive =
  sendMessage webArchive subresourcesSelector

-- | subframeArchives
--
-- The archives representing the subframes of the archive (can be nil).
--
-- ObjC selector: @- subframeArchives@
subframeArchives :: IsWebArchive webArchive => webArchive -> IO (Id NSArray)
subframeArchives webArchive =
  sendMessage webArchive subframeArchivesSelector

-- | data
--
-- The data representation of the archive.
--
-- The data returned by this method can be used to save a web archive to a file or to place a web archive on the pasteboard    using WebArchivePboardType. To create a WebArchive using the returned data, call initWithData:.
--
-- ObjC selector: @- data@
data_ :: IsWebArchive webArchive => webArchive -> IO (Id NSData)
data_ webArchive =
  sendMessage webArchive dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMainResource:subresources:subframeArchives:@
initWithMainResource_subresources_subframeArchivesSelector :: Selector '[Id WebResource, Id NSArray, Id NSArray] (Id WebArchive)
initWithMainResource_subresources_subframeArchivesSelector = mkSelector "initWithMainResource:subresources:subframeArchives:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id WebArchive)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @mainResource@
mainResourceSelector :: Selector '[] (Id WebResource)
mainResourceSelector = mkSelector "mainResource"

-- | @Selector@ for @subresources@
subresourcesSelector :: Selector '[] (Id NSArray)
subresourcesSelector = mkSelector "subresources"

-- | @Selector@ for @subframeArchives@
subframeArchivesSelector :: Selector '[] (Id NSArray)
subframeArchivesSelector = mkSelector "subframeArchives"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

