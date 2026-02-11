{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebDataSource
--
-- A WebDataSource represents the data associated with a web page.    A datasource has a WebDocumentRepresentation which holds an appropriate    representation of the data.  WebDataSources manage a hierarchy of WebFrames.    WebDataSources are typically related to a view by their containing WebFrame.
--
-- Generated bindings for @WebDataSource@.
module ObjC.WebKit.WebDataSource
  ( WebDataSource
  , IsWebDataSource(..)
  , initWithRequest
  , subresourceForURL
  , addSubresource
  , data_
  , representation
  , webFrame
  , initialRequest
  , request
  , response
  , textEncodingName
  , loading
  , pageTitle
  , unreachableURL
  , webArchive
  , mainResource
  , subresources
  , initWithRequestSelector
  , subresourceForURLSelector
  , addSubresourceSelector
  , dataSelector
  , representationSelector
  , webFrameSelector
  , initialRequestSelector
  , requestSelector
  , responseSelector
  , textEncodingNameSelector
  , loadingSelector
  , pageTitleSelector
  , unreachableURLSelector
  , webArchiveSelector
  , mainResourceSelector
  , subresourcesSelector


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

-- | initWithRequest:
--
-- The designated initializer for WebDataSource.
--
-- @request@ — The request to use in creating a datasource.
--
-- Returns: Returns an initialized WebDataSource.
--
-- ObjC selector: @- initWithRequest:@
initWithRequest :: (IsWebDataSource webDataSource, IsNSURLRequest request) => webDataSource -> request -> IO (Id WebDataSource)
initWithRequest webDataSource  request =
  withObjCPtr request $ \raw_request ->
      sendMsg webDataSource (mkSelector "initWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | method subresourceForURL:
--
-- Returns a subresource for a given URL.
--
-- @URL@ — The URL of the subresource.     Returns non-nil if the data source has fully downloaded a subresource with the given URL.
--
-- ObjC selector: @- subresourceForURL:@
subresourceForURL :: (IsWebDataSource webDataSource, IsNSURL url) => webDataSource -> url -> IO (Id WebResource)
subresourceForURL webDataSource  url =
  withObjCPtr url $ \raw_url ->
      sendMsg webDataSource (mkSelector "subresourceForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | addSubresource:
--
-- Adds a subresource to the data source.
--
-- @subresource@ — The subresource to be added.     addSubresource: adds a subresource to the data source's list of subresources.    Later, if something causes the data source to load the URL of the subresource, the data source    will load the data from the subresource instead of from the network. For example, if one wants to add    an image that is already downloaded to a web page, addSubresource: can be called so that the data source    uses the downloaded image rather than accessing the network. NOTE: If the data source already has a    subresource with the same URL, addSubresource: will replace it.
--
-- ObjC selector: @- addSubresource:@
addSubresource :: (IsWebDataSource webDataSource, IsWebResource subresource) => webDataSource -> subresource -> IO ()
addSubresource webDataSource  subresource =
  withObjCPtr subresource $ \raw_subresource ->
      sendMsg webDataSource (mkSelector "addSubresource:") retVoid [argPtr (castPtr raw_subresource :: Ptr ())]

-- | data
--
-- Returns the raw data associated with the datasource.  Returns nil    if the datasource hasn't loaded any data.
--
-- The data will be incomplete until the datasource has completely loaded.
--
-- ObjC selector: @- data@
data_ :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSData)
data_ webDataSource  =
    sendMsg webDataSource (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | representation
--
-- The representation associated with this datasource.    Returns nil if the datasource hasn't created its representation.
--
-- A representation holds a type specific representation    of the datasource's data.  The representation class is determined by mapping    a MIME type to a class.  The representation is created once the MIME type    of the datasource content has been determined.
--
-- ObjC selector: @- representation@
representation :: IsWebDataSource webDataSource => webDataSource -> IO RawId
representation webDataSource  =
    fmap (RawId . castPtr) $ sendMsg webDataSource (mkSelector "representation") (retPtr retVoid) []

-- | webFrame
--
-- The frame that represents this data source.
--
-- ObjC selector: @- webFrame@
webFrame :: IsWebDataSource webDataSource => webDataSource -> IO (Id WebFrame)
webFrame webDataSource  =
    sendMsg webDataSource (mkSelector "webFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | initialRequest
--
-- A reference to the original request that created the    datasource.  This request will be unmodified by WebKit.
--
-- ObjC selector: @- initialRequest@
initialRequest :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSURLRequest)
initialRequest webDataSource  =
    sendMsg webDataSource (mkSelector "initialRequest") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | request
--
-- The request that was used to create this datasource.
--
-- ObjC selector: @- request@
request :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSMutableURLRequest)
request webDataSource  =
    sendMsg webDataSource (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | response
--
-- The NSURLResponse for the data source.
--
-- ObjC selector: @- response@
response :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSURLResponse)
response webDataSource  =
    sendMsg webDataSource (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textEncodingName
--
-- Returns either the override encoding, as set on the WebView for this    dataSource or the encoding from the response.
--
-- ObjC selector: @- textEncodingName@
textEncodingName :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSString)
textEncodingName webDataSource  =
    sendMsg webDataSource (mkSelector "textEncodingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isLoading
--
-- Returns YES if there are any pending loads.
--
-- ObjC selector: @- loading@
loading :: IsWebDataSource webDataSource => webDataSource -> IO Bool
loading webDataSource  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg webDataSource (mkSelector "loading") retCULong []

-- | pageTitle
--
-- The page title or nil.
--
-- ObjC selector: @- pageTitle@
pageTitle :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSString)
pageTitle webDataSource  =
    sendMsg webDataSource (mkSelector "pageTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | unreachableURL
--
-- The unreachableURL for which this dataSource is showing alternate content, or nil.
--
-- This will be non-nil only for dataSources created by calls to the     WebFrame method loadAlternateHTMLString:baseURL:forUnreachableURL:.
--
-- ObjC selector: @- unreachableURL@
unreachableURL :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSURL)
unreachableURL webDataSource  =
    sendMsg webDataSource (mkSelector "unreachableURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | webArchive
--
-- A WebArchive representing the data source, its subresources and child frames.     This method constructs a WebArchive using the original downloaded data.    In the case of HTML, if the current state of the document is preferred, webArchive should be    called on the DOM document instead.
--
-- ObjC selector: @- webArchive@
webArchive :: IsWebDataSource webDataSource => webDataSource -> IO (Id WebArchive)
webArchive webDataSource  =
    sendMsg webDataSource (mkSelector "webArchive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mainResource
--
-- A WebResource representing the data source.     This method constructs a WebResource using the original downloaded data.    This method can be used to construct a WebArchive in case the archive returned by    WebDataSource's webArchive isn't sufficient.
--
-- ObjC selector: @- mainResource@
mainResource :: IsWebDataSource webDataSource => webDataSource -> IO (Id WebResource)
mainResource webDataSource  =
    sendMsg webDataSource (mkSelector "mainResource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subresources
--
-- All the subresources associated with the data source.     The returned array only contains subresources that have fully downloaded.
--
-- ObjC selector: @- subresources@
subresources :: IsWebDataSource webDataSource => webDataSource -> IO (Id NSArray)
subresources webDataSource  =
    sendMsg webDataSource (mkSelector "subresources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:@
initWithRequestSelector :: Selector
initWithRequestSelector = mkSelector "initWithRequest:"

-- | @Selector@ for @subresourceForURL:@
subresourceForURLSelector :: Selector
subresourceForURLSelector = mkSelector "subresourceForURL:"

-- | @Selector@ for @addSubresource:@
addSubresourceSelector :: Selector
addSubresourceSelector = mkSelector "addSubresource:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @representation@
representationSelector :: Selector
representationSelector = mkSelector "representation"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector
webFrameSelector = mkSelector "webFrame"

-- | @Selector@ for @initialRequest@
initialRequestSelector :: Selector
initialRequestSelector = mkSelector "initialRequest"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @textEncodingName@
textEncodingNameSelector :: Selector
textEncodingNameSelector = mkSelector "textEncodingName"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

-- | @Selector@ for @pageTitle@
pageTitleSelector :: Selector
pageTitleSelector = mkSelector "pageTitle"

-- | @Selector@ for @unreachableURL@
unreachableURLSelector :: Selector
unreachableURLSelector = mkSelector "unreachableURL"

-- | @Selector@ for @webArchive@
webArchiveSelector :: Selector
webArchiveSelector = mkSelector "webArchive"

-- | @Selector@ for @mainResource@
mainResourceSelector :: Selector
mainResourceSelector = mkSelector "mainResource"

-- | @Selector@ for @subresources@
subresourcesSelector :: Selector
subresourcesSelector = mkSelector "subresources"

