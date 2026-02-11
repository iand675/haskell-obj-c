{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebFrame
--
-- Every web page is represented by at least one WebFrame.  A WebFrame    has a WebFrameView and a WebDataSource.
--
-- Generated bindings for @WebFrame@.
module ObjC.WebKit.WebFrame
  ( WebFrame
  , IsWebFrame(..)
  , initWithName_webFrameView_webView
  , loadRequest
  , loadData_MIMEType_textEncodingName_baseURL
  , loadHTMLString_baseURL
  , loadAlternateHTMLString_baseURL_forUnreachableURL
  , loadArchive
  , stopLoading
  , reload
  , reloadFromOrigin
  , findFrameNamed
  , name
  , webView
  , frameView
  , domDocument
  , frameElement
  , dataSource
  , provisionalDataSource
  , parentFrame
  , childFrames
  , windowObject
  , globalContext
  , javaScriptContext
  , initWithName_webFrameView_webViewSelector
  , loadRequestSelector
  , loadData_MIMEType_textEncodingName_baseURLSelector
  , loadHTMLString_baseURLSelector
  , loadAlternateHTMLString_baseURL_forUnreachableURLSelector
  , loadArchiveSelector
  , stopLoadingSelector
  , reloadSelector
  , reloadFromOriginSelector
  , findFrameNamedSelector
  , nameSelector
  , webViewSelector
  , frameViewSelector
  , domDocumentSelector
  , frameElementSelector
  , dataSourceSelector
  , provisionalDataSourceSelector
  , parentFrameSelector
  , childFramesSelector
  , windowObjectSelector
  , globalContextSelector
  , javaScriptContextSelector


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
import ObjC.JavaScriptCore.Internal.Classes

-- | initWithName:webFrameView:webView:
--
-- The designated initializer of WebFrame.
--
-- WebFrames are normally created for you by the WebView.  You should     not need to invoke this method directly.
--
-- @name@ — The name of the frame.
--
-- @view@ — The WebFrameView for the frame.
--
-- @webView@ — The WebView that manages the frame.
--
-- Returns: Returns an initialized WebFrame.
--
-- ObjC selector: @- initWithName:webFrameView:webView:@
initWithName_webFrameView_webView :: (IsWebFrame webFrame, IsNSString name, IsWebFrameView view, IsWebView webView) => webFrame -> name -> view -> webView -> IO (Id WebFrame)
initWithName_webFrameView_webView webFrame  name view webView =
withObjCPtr name $ \raw_name ->
  withObjCPtr view $ \raw_view ->
    withObjCPtr webView $ \raw_webView ->
        sendMsg webFrame (mkSelector "initWithName:webFrameView:webView:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_webView :: Ptr ())] >>= ownedObject . castPtr

-- | loadRequest:
--
-- @request@ — The web request to load.
--
-- ObjC selector: @- loadRequest:@
loadRequest :: (IsWebFrame webFrame, IsNSURLRequest request) => webFrame -> request -> IO ()
loadRequest webFrame  request =
withObjCPtr request $ \raw_request ->
    sendMsg webFrame (mkSelector "loadRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- | loadData:MIMEType:textEncodingName:baseURL:
--
-- @data@ — The data to use for the main page of the document.
--
-- @MIMEType@ — The MIME type of the data.
--
-- @encodingName@ — The encoding of the data.
--
-- @URL@ — The base URL to apply to relative URLs within the document.
--
-- ObjC selector: @- loadData:MIMEType:textEncodingName:baseURL:@
loadData_MIMEType_textEncodingName_baseURL :: (IsWebFrame webFrame, IsNSData data_, IsNSString mimeType, IsNSString encodingName, IsNSURL url) => webFrame -> data_ -> mimeType -> encodingName -> url -> IO ()
loadData_MIMEType_textEncodingName_baseURL webFrame  data_ mimeType encodingName url =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr mimeType $ \raw_mimeType ->
    withObjCPtr encodingName $ \raw_encodingName ->
      withObjCPtr url $ \raw_url ->
          sendMsg webFrame (mkSelector "loadData:MIMEType:textEncodingName:baseURL:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_mimeType :: Ptr ()), argPtr (castPtr raw_encodingName :: Ptr ()), argPtr (castPtr raw_url :: Ptr ())]

-- | loadHTMLString:baseURL:
--
-- @string@ — The string to use for the main page of the document.
--
-- @URL@ — The base URL to apply to relative URLs within the document.
--
-- ObjC selector: @- loadHTMLString:baseURL:@
loadHTMLString_baseURL :: (IsWebFrame webFrame, IsNSString string, IsNSURL url) => webFrame -> string -> url -> IO ()
loadHTMLString_baseURL webFrame  string url =
withObjCPtr string $ \raw_string ->
  withObjCPtr url $ \raw_url ->
      sendMsg webFrame (mkSelector "loadHTMLString:baseURL:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_url :: Ptr ())]

-- | loadAlternateHTMLString:baseURL:forUnreachableURL:
--
-- Loads a page to display as a substitute for a URL that could not be reached.
--
-- This allows clients to display page-loading errors in the webview itself.    This is typically called while processing the WebFrameLoadDelegate method    -webView:didFailProvisionalLoadWithError:forFrame: or one of the WebPolicyDelegate methods    -webView:decidePolicyForMIMEType:request:frame:decisionListener: or    -webView:unableToImplementPolicyWithError:frame:. If it is called from within one of those    three delegate methods then the back/forward list will be maintained appropriately.
--
-- @string@ — The string to use for the main page of the document.
--
-- @baseURL@ — The baseURL to apply to relative URLs within the document.
--
-- @unreachableURL@ — The URL for which this page will serve as alternate content.
--
-- ObjC selector: @- loadAlternateHTMLString:baseURL:forUnreachableURL:@
loadAlternateHTMLString_baseURL_forUnreachableURL :: (IsWebFrame webFrame, IsNSString string, IsNSURL baseURL, IsNSURL unreachableURL) => webFrame -> string -> baseURL -> unreachableURL -> IO ()
loadAlternateHTMLString_baseURL_forUnreachableURL webFrame  string baseURL unreachableURL =
withObjCPtr string $ \raw_string ->
  withObjCPtr baseURL $ \raw_baseURL ->
    withObjCPtr unreachableURL $ \raw_unreachableURL ->
        sendMsg webFrame (mkSelector "loadAlternateHTMLString:baseURL:forUnreachableURL:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ()), argPtr (castPtr raw_unreachableURL :: Ptr ())]

-- | loadArchive:
--
-- Causes WebFrame to load a WebArchive.
--
-- @archive@ — The archive to be loaded.
--
-- ObjC selector: @- loadArchive:@
loadArchive :: (IsWebFrame webFrame, IsWebArchive archive) => webFrame -> archive -> IO ()
loadArchive webFrame  archive =
withObjCPtr archive $ \raw_archive ->
    sendMsg webFrame (mkSelector "loadArchive:") retVoid [argPtr (castPtr raw_archive :: Ptr ())]

-- | stopLoading
--
-- Stop any pending loads on the frame's data source,    and its children.
--
-- ObjC selector: @- stopLoading@
stopLoading :: IsWebFrame webFrame => webFrame -> IO ()
stopLoading webFrame  =
  sendMsg webFrame (mkSelector "stopLoading") retVoid []

-- | reload
--
-- Performs HTTP/1.1 end-to-end revalidation using cache-validating conditionals if possible.
--
-- ObjC selector: @- reload@
reload :: IsWebFrame webFrame => webFrame -> IO ()
reload webFrame  =
  sendMsg webFrame (mkSelector "reload") retVoid []

-- | reloadFromOrigin
--
-- Performs HTTP/1.1 end-to-end reload.
--
-- ObjC selector: @- reloadFromOrigin@
reloadFromOrigin :: IsWebFrame webFrame => webFrame -> IO ()
reloadFromOrigin webFrame  =
  sendMsg webFrame (mkSelector "reloadFromOrigin") retVoid []

-- | findFrameNamed:
--
-- This method returns a frame with the given name. findFrameNamed returns self     for _self and _current, the parent frame for _parent and the main frame for _top.     findFrameNamed returns self for _parent and _top if the receiver is the mainFrame.    findFrameNamed first searches from the current frame to all descending frames then the    rest of the frames in the WebView. If still not found, findFrameNamed searches the    frames of the other WebViews.
--
-- @name@ — The name of the frame to find.
--
-- Returns: The frame matching the provided name. nil if the frame is not found.
--
-- ObjC selector: @- findFrameNamed:@
findFrameNamed :: (IsWebFrame webFrame, IsNSString name) => webFrame -> name -> IO (Id WebFrame)
findFrameNamed webFrame  name =
withObjCPtr name $ \raw_name ->
    sendMsg webFrame (mkSelector "findFrameNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | name
--
-- The frame name.
--
-- ObjC selector: @- name@
name :: IsWebFrame webFrame => webFrame -> IO (Id NSString)
name webFrame  =
  sendMsg webFrame (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | webView
--
-- The WebView for the document that includes this frame.
--
-- ObjC selector: @- webView@
webView :: IsWebFrame webFrame => webFrame -> IO (Id WebView)
webView webFrame  =
  sendMsg webFrame (mkSelector "webView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | frameView
--
-- The WebFrameView for this frame.
--
-- ObjC selector: @- frameView@
frameView :: IsWebFrame webFrame => webFrame -> IO (Id WebFrameView)
frameView webFrame  =
  sendMsg webFrame (mkSelector "frameView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DOMDocument
--
-- The DOM document of the frame.     Returns nil if the frame does not contain a DOM document such as a standalone image.
--
-- ObjC selector: @- DOMDocument@
domDocument :: IsWebFrame webFrame => webFrame -> IO (Id DOMDocument)
domDocument webFrame  =
  sendMsg webFrame (mkSelector "DOMDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | frameElement
--
-- The frame element of the frame.     The class of the result is either DOMHTMLFrameElement, DOMHTMLIFrameElement or DOMHTMLObjectElement.    Returns nil if the frame is the main frame since there is no frame element for the frame in this case.
--
-- ObjC selector: @- frameElement@
frameElement :: IsWebFrame webFrame => webFrame -> IO (Id DOMHTMLElement)
frameElement webFrame  =
  sendMsg webFrame (mkSelector "frameElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dataSource
--
-- The datasource for this frame.
--
-- Returns the committed data source.  Will return nil if the    provisional data source hasn't yet been loaded.
--
-- ObjC selector: @- dataSource@
dataSource :: IsWebFrame webFrame => webFrame -> IO (Id WebDataSource)
dataSource webFrame  =
  sendMsg webFrame (mkSelector "dataSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | provisionalDataSource
--
-- The provisional datasource of this frame.
--
-- Will return the provisional data source.  The provisional data source will    be nil if no data source has been set on the frame, or the data source    has successfully transitioned to the committed data source.
--
-- ObjC selector: @- provisionalDataSource@
provisionalDataSource :: IsWebFrame webFrame => webFrame -> IO (Id WebDataSource)
provisionalDataSource webFrame  =
  sendMsg webFrame (mkSelector "provisionalDataSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | parentFrame
--
-- The frame containing this frame, or nil if this is a top level frame.
--
-- ObjC selector: @- parentFrame@
parentFrame :: IsWebFrame webFrame => webFrame -> IO (Id WebFrame)
parentFrame webFrame  =
  sendMsg webFrame (mkSelector "parentFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | childFrames
--
-- An array of WebFrame.
--
-- The frames in the array are associated with a frame set or iframe.
--
-- ObjC selector: @- childFrames@
childFrames :: IsWebFrame webFrame => webFrame -> IO (Id NSArray)
childFrames webFrame  =
  sendMsg webFrame (mkSelector "childFrames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | windowObject
--
-- The WebScriptObject representing the frame's JavaScript window object.
--
-- ObjC selector: @- windowObject@
windowObject :: IsWebFrame webFrame => webFrame -> IO (Id WebScriptObject)
windowObject webFrame  =
  sendMsg webFrame (mkSelector "windowObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | globalContext
--
-- The frame's global JavaScript execution context.
--
-- Use this method to bridge between the WebKit and JavaScriptCore APIs.
--
-- ObjC selector: @- globalContext@
globalContext :: IsWebFrame webFrame => webFrame -> IO (Ptr ())
globalContext webFrame  =
  fmap castPtr $ sendMsg webFrame (mkSelector "globalContext") (retPtr retVoid) []

-- | javaScriptContext
--
-- The frame's global JavaScript execution context.
--
-- Use this method to bridge between the WebKit and Objective-C JavaScriptCore API.
--
-- ObjC selector: @- javaScriptContext@
javaScriptContext :: IsWebFrame webFrame => webFrame -> IO (Id JSContext)
javaScriptContext webFrame  =
  sendMsg webFrame (mkSelector "javaScriptContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:webFrameView:webView:@
initWithName_webFrameView_webViewSelector :: Selector
initWithName_webFrameView_webViewSelector = mkSelector "initWithName:webFrameView:webView:"

-- | @Selector@ for @loadRequest:@
loadRequestSelector :: Selector
loadRequestSelector = mkSelector "loadRequest:"

-- | @Selector@ for @loadData:MIMEType:textEncodingName:baseURL:@
loadData_MIMEType_textEncodingName_baseURLSelector :: Selector
loadData_MIMEType_textEncodingName_baseURLSelector = mkSelector "loadData:MIMEType:textEncodingName:baseURL:"

-- | @Selector@ for @loadHTMLString:baseURL:@
loadHTMLString_baseURLSelector :: Selector
loadHTMLString_baseURLSelector = mkSelector "loadHTMLString:baseURL:"

-- | @Selector@ for @loadAlternateHTMLString:baseURL:forUnreachableURL:@
loadAlternateHTMLString_baseURL_forUnreachableURLSelector :: Selector
loadAlternateHTMLString_baseURL_forUnreachableURLSelector = mkSelector "loadAlternateHTMLString:baseURL:forUnreachableURL:"

-- | @Selector@ for @loadArchive:@
loadArchiveSelector :: Selector
loadArchiveSelector = mkSelector "loadArchive:"

-- | @Selector@ for @stopLoading@
stopLoadingSelector :: Selector
stopLoadingSelector = mkSelector "stopLoading"

-- | @Selector@ for @reload@
reloadSelector :: Selector
reloadSelector = mkSelector "reload"

-- | @Selector@ for @reloadFromOrigin@
reloadFromOriginSelector :: Selector
reloadFromOriginSelector = mkSelector "reloadFromOrigin"

-- | @Selector@ for @findFrameNamed:@
findFrameNamedSelector :: Selector
findFrameNamedSelector = mkSelector "findFrameNamed:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @webView@
webViewSelector :: Selector
webViewSelector = mkSelector "webView"

-- | @Selector@ for @frameView@
frameViewSelector :: Selector
frameViewSelector = mkSelector "frameView"

-- | @Selector@ for @DOMDocument@
domDocumentSelector :: Selector
domDocumentSelector = mkSelector "DOMDocument"

-- | @Selector@ for @frameElement@
frameElementSelector :: Selector
frameElementSelector = mkSelector "frameElement"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @provisionalDataSource@
provisionalDataSourceSelector :: Selector
provisionalDataSourceSelector = mkSelector "provisionalDataSource"

-- | @Selector@ for @parentFrame@
parentFrameSelector :: Selector
parentFrameSelector = mkSelector "parentFrame"

-- | @Selector@ for @childFrames@
childFramesSelector :: Selector
childFramesSelector = mkSelector "childFrames"

-- | @Selector@ for @windowObject@
windowObjectSelector :: Selector
windowObjectSelector = mkSelector "windowObject"

-- | @Selector@ for @globalContext@
globalContextSelector :: Selector
globalContextSelector = mkSelector "globalContext"

-- | @Selector@ for @javaScriptContext@
javaScriptContextSelector :: Selector
javaScriptContextSelector = mkSelector "javaScriptContext"

