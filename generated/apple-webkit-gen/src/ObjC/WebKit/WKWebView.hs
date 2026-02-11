{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKWebView@.
module ObjC.WebKit.WKWebView
  ( WKWebView
  , IsWKWebView(..)
  , initWithCoder
  , loadRequest
  , loadFileURL_allowingReadAccessToURL
  , loadHTMLString_baseURL
  , loadData_MIMEType_characterEncodingName_baseURL
  , goToBackForwardListItem
  , goBack
  , goForward
  , reload
  , reloadFromOrigin
  , stopLoading
  , evaluateJavaScript_completionHandler
  , evaluateJavaScript_inFrame_inContentWorld_completionHandler
  , callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandler
  , closeAllMediaPresentationsWithCompletionHandler
  , closeAllMediaPresentations
  , pauseAllMediaPlaybackWithCompletionHandler
  , pauseAllMediaPlayback
  , setAllMediaPlaybackSuspended_completionHandler
  , resumeAllMediaPlayback
  , suspendAllMediaPlayback
  , requestMediaPlaybackStateWithCompletionHandler
  , requestMediaPlaybackState
  , setCameraCaptureState_completionHandler
  , setMicrophoneCaptureState_completionHandler
  , takeSnapshotWithConfiguration_completionHandler
  , createPDFWithConfiguration_completionHandler
  , createWebArchiveDataWithCompletionHandler
  , findString_withConfiguration_completionHandler
  , handlesURLScheme
  , startDownloadUsingRequest_completionHandler
  , resumeDownloadFromResumeData_completionHandler
  , loadSimulatedRequest_response_responseData
  , loadSimulatedRequest_withResponse_responseData
  , loadFileRequest_allowingReadAccessToURL
  , loadSimulatedRequest_responseHTMLString
  , loadSimulatedRequest_withResponseHTMLString
  , printOperationWithPrintInfo
  , setMinimumViewportInset_maximumViewportInset
  , fetchDataOfTypes_completionHandler
  , restoreData_completionHandler
  , configuration
  , navigationDelegate
  , setNavigationDelegate
  , uiDelegate
  , setUIDelegate
  , backForwardList
  , title
  , url
  , loading
  , estimatedProgress
  , hasOnlySecureContent
  , serverTrust
  , canGoBack
  , canGoForward
  , cameraCaptureState
  , microphoneCaptureState
  , allowsBackForwardNavigationGestures
  , setAllowsBackForwardNavigationGestures
  , customUserAgent
  , setCustomUserAgent
  , allowsLinkPreview
  , setAllowsLinkPreview
  , allowsMagnification
  , setAllowsMagnification
  , magnification
  , setMagnification
  , pageZoom
  , setPageZoom
  , mediaType
  , setMediaType
  , interactionState
  , setInteractionState
  , isBlockedByScreenTime
  , themeColor
  , underPageBackgroundColor
  , setUnderPageBackgroundColor
  , fullscreenState
  , minimumViewportInset
  , maximumViewportInset
  , inspectable
  , setInspectable
  , writingToolsActive
  , obscuredContentInsets
  , setObscuredContentInsets
  , certificateChain
  , initWithCoderSelector
  , loadRequestSelector
  , loadFileURL_allowingReadAccessToURLSelector
  , loadHTMLString_baseURLSelector
  , loadData_MIMEType_characterEncodingName_baseURLSelector
  , goToBackForwardListItemSelector
  , goBackSelector
  , goForwardSelector
  , reloadSelector
  , reloadFromOriginSelector
  , stopLoadingSelector
  , evaluateJavaScript_completionHandlerSelector
  , evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector
  , callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector
  , closeAllMediaPresentationsWithCompletionHandlerSelector
  , closeAllMediaPresentationsSelector
  , pauseAllMediaPlaybackWithCompletionHandlerSelector
  , pauseAllMediaPlaybackSelector
  , setAllMediaPlaybackSuspended_completionHandlerSelector
  , resumeAllMediaPlaybackSelector
  , suspendAllMediaPlaybackSelector
  , requestMediaPlaybackStateWithCompletionHandlerSelector
  , requestMediaPlaybackStateSelector
  , setCameraCaptureState_completionHandlerSelector
  , setMicrophoneCaptureState_completionHandlerSelector
  , takeSnapshotWithConfiguration_completionHandlerSelector
  , createPDFWithConfiguration_completionHandlerSelector
  , createWebArchiveDataWithCompletionHandlerSelector
  , findString_withConfiguration_completionHandlerSelector
  , handlesURLSchemeSelector
  , startDownloadUsingRequest_completionHandlerSelector
  , resumeDownloadFromResumeData_completionHandlerSelector
  , loadSimulatedRequest_response_responseDataSelector
  , loadSimulatedRequest_withResponse_responseDataSelector
  , loadFileRequest_allowingReadAccessToURLSelector
  , loadSimulatedRequest_responseHTMLStringSelector
  , loadSimulatedRequest_withResponseHTMLStringSelector
  , printOperationWithPrintInfoSelector
  , setMinimumViewportInset_maximumViewportInsetSelector
  , fetchDataOfTypes_completionHandlerSelector
  , restoreData_completionHandlerSelector
  , configurationSelector
  , navigationDelegateSelector
  , setNavigationDelegateSelector
  , uiDelegateSelector
  , setUIDelegateSelector
  , backForwardListSelector
  , titleSelector
  , urlSelector
  , loadingSelector
  , estimatedProgressSelector
  , hasOnlySecureContentSelector
  , serverTrustSelector
  , canGoBackSelector
  , canGoForwardSelector
  , cameraCaptureStateSelector
  , microphoneCaptureStateSelector
  , allowsBackForwardNavigationGesturesSelector
  , setAllowsBackForwardNavigationGesturesSelector
  , customUserAgentSelector
  , setCustomUserAgentSelector
  , allowsLinkPreviewSelector
  , setAllowsLinkPreviewSelector
  , allowsMagnificationSelector
  , setAllowsMagnificationSelector
  , magnificationSelector
  , setMagnificationSelector
  , pageZoomSelector
  , setPageZoomSelector
  , mediaTypeSelector
  , setMediaTypeSelector
  , interactionStateSelector
  , setInteractionStateSelector
  , isBlockedByScreenTimeSelector
  , themeColorSelector
  , underPageBackgroundColorSelector
  , setUnderPageBackgroundColorSelector
  , fullscreenStateSelector
  , minimumViewportInsetSelector
  , maximumViewportInsetSelector
  , inspectableSelector
  , setInspectableSelector
  , writingToolsActiveSelector
  , obscuredContentInsetsSelector
  , setObscuredContentInsetsSelector
  , certificateChainSelector

  -- * Enum types
  , WKFullscreenState(WKFullscreenState)
  , pattern WKFullscreenStateNotInFullscreen
  , pattern WKFullscreenStateEnteringFullscreen
  , pattern WKFullscreenStateInFullscreen
  , pattern WKFullscreenStateExitingFullscreen
  , WKMediaCaptureState(WKMediaCaptureState)
  , pattern WKMediaCaptureStateNone
  , pattern WKMediaCaptureStateActive
  , pattern WKMediaCaptureStateMuted
  , WKWebViewDataType(WKWebViewDataType)
  , pattern WKWebViewDataTypeSessionStorage

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsWKWebView wkWebView, IsNSCoder coder) => wkWebView -> coder -> IO (Id WKWebView)
initWithCoder wkWebView  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg wkWebView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | Navigates to a requested URL.
--
-- @request@ — The request specifying the URL to which to navigate.
--
-- Returns: A new navigation for the given request.
--
-- ObjC selector: @- loadRequest:@
loadRequest :: (IsWKWebView wkWebView, IsNSURLRequest request) => wkWebView -> request -> IO (Id WKNavigation)
loadRequest wkWebView  request =
  withObjCPtr request $ \raw_request ->
      sendMsg wkWebView (mkSelector "loadRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | Navigates to the requested file URL on the filesystem.
--
-- @URL@ — The file URL to which to navigate.
--
-- @readAccessURL@ — The URL to allow read access to.
--
-- If readAccessURL references a single file, only that file may be loaded by WebKit. If readAccessURL references a directory, files inside that file may be loaded by WebKit.
--
-- Returns: A new navigation for the given file URL.
--
-- ObjC selector: @- loadFileURL:allowingReadAccessToURL:@
loadFileURL_allowingReadAccessToURL :: (IsWKWebView wkWebView, IsNSURL url, IsNSURL readAccessURL) => wkWebView -> url -> readAccessURL -> IO (Id WKNavigation)
loadFileURL_allowingReadAccessToURL wkWebView  url readAccessURL =
  withObjCPtr url $ \raw_url ->
    withObjCPtr readAccessURL $ \raw_readAccessURL ->
        sendMsg wkWebView (mkSelector "loadFileURL:allowingReadAccessToURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_readAccessURL :: Ptr ())] >>= retainedObject . castPtr

-- | Sets the webpage contents and base URL.
--
-- @string@ — The string to use as the contents of the webpage.
--
-- @baseURL@ — A URL that is used to resolve relative URLs within the document.
--
-- Returns: A new navigation.
--
-- ObjC selector: @- loadHTMLString:baseURL:@
loadHTMLString_baseURL :: (IsWKWebView wkWebView, IsNSString string, IsNSURL baseURL) => wkWebView -> string -> baseURL -> IO (Id WKNavigation)
loadHTMLString_baseURL wkWebView  string baseURL =
  withObjCPtr string $ \raw_string ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg wkWebView (mkSelector "loadHTMLString:baseURL:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | Sets the webpage contents and base URL.
--
-- @data@ — The data to use as the contents of the webpage.
--
-- @MIMEType@ — The MIME type of the data.
--
-- @characterEncodingName@ — The data's character encoding name.
--
-- @baseURL@ — A URL that is used to resolve relative URLs within the document.
--
-- Returns: A new navigation.
--
-- ObjC selector: @- loadData:MIMEType:characterEncodingName:baseURL:@
loadData_MIMEType_characterEncodingName_baseURL :: (IsWKWebView wkWebView, IsNSData data_, IsNSString mimeType, IsNSString characterEncodingName, IsNSURL baseURL) => wkWebView -> data_ -> mimeType -> characterEncodingName -> baseURL -> IO (Id WKNavigation)
loadData_MIMEType_characterEncodingName_baseURL wkWebView  data_ mimeType characterEncodingName baseURL =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr mimeType $ \raw_mimeType ->
      withObjCPtr characterEncodingName $ \raw_characterEncodingName ->
        withObjCPtr baseURL $ \raw_baseURL ->
            sendMsg wkWebView (mkSelector "loadData:MIMEType:characterEncodingName:baseURL:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_mimeType :: Ptr ()), argPtr (castPtr raw_characterEncodingName :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | Navigates to an item from the back-forward list and sets it as the current item.
--
-- @item@ — The item to which to navigate. Must be one of the items in the web view's back-forward list.
--
-- Returns: A new navigation to the requested item, or nil if it is already the current item or is not part of the web view's back-forward list.
--
-- backForwardList
--
-- ObjC selector: @- goToBackForwardListItem:@
goToBackForwardListItem :: (IsWKWebView wkWebView, IsWKBackForwardListItem item) => wkWebView -> item -> IO (Id WKNavigation)
goToBackForwardListItem wkWebView  item =
  withObjCPtr item $ \raw_item ->
      sendMsg wkWebView (mkSelector "goToBackForwardListItem:") (retPtr retVoid) [argPtr (castPtr raw_item :: Ptr ())] >>= retainedObject . castPtr

-- | Navigates to the back item in the back-forward list.
--
-- Returns: A new navigation to the requested item, or nil if there is no back item in the back-forward list.
--
-- ObjC selector: @- goBack@
goBack :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
goBack wkWebView  =
    sendMsg wkWebView (mkSelector "goBack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Navigates to the forward item in the back-forward list.
--
-- Returns: A new navigation to the requested item, or nil if there is no forward item in the back-forward list.
--
-- ObjC selector: @- goForward@
goForward :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
goForward wkWebView  =
    sendMsg wkWebView (mkSelector "goForward") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reloads the current page.
--
-- Returns: A new navigation representing the reload.
--
-- ObjC selector: @- reload@
reload :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
reload wkWebView  =
    sendMsg wkWebView (mkSelector "reload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reloads the current page, performing end-to-end revalidation using cache-validating conditionals if possible.
--
-- Returns: A new navigation representing the reload.
--
-- ObjC selector: @- reloadFromOrigin@
reloadFromOrigin :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
reloadFromOrigin wkWebView  =
    sendMsg wkWebView (mkSelector "reloadFromOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Stops loading all resources on the current page.
--
-- ObjC selector: @- stopLoading@
stopLoading :: IsWKWebView wkWebView => wkWebView -> IO ()
stopLoading wkWebView  =
    sendMsg wkWebView (mkSelector "stopLoading") retVoid []

-- | @- evaluateJavaScript:completionHandler:@
evaluateJavaScript_completionHandler :: (IsWKWebView wkWebView, IsNSString javaScriptString) => wkWebView -> javaScriptString -> Ptr () -> IO ()
evaluateJavaScript_completionHandler wkWebView  javaScriptString completionHandler =
  withObjCPtr javaScriptString $ \raw_javaScriptString ->
      sendMsg wkWebView (mkSelector "evaluateJavaScript:completionHandler:") retVoid [argPtr (castPtr raw_javaScriptString :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- evaluateJavaScript:inFrame:inContentWorld:completionHandler:@
evaluateJavaScript_inFrame_inContentWorld_completionHandler :: (IsWKWebView wkWebView, IsNSString javaScriptString, IsWKFrameInfo frame, IsWKContentWorld contentWorld) => wkWebView -> javaScriptString -> frame -> contentWorld -> Ptr () -> IO ()
evaluateJavaScript_inFrame_inContentWorld_completionHandler wkWebView  javaScriptString frame contentWorld completionHandler =
  withObjCPtr javaScriptString $ \raw_javaScriptString ->
    withObjCPtr frame $ \raw_frame ->
      withObjCPtr contentWorld $ \raw_contentWorld ->
          sendMsg wkWebView (mkSelector "evaluateJavaScript:inFrame:inContentWorld:completionHandler:") retVoid [argPtr (castPtr raw_javaScriptString :: Ptr ()), argPtr (castPtr raw_frame :: Ptr ()), argPtr (castPtr raw_contentWorld :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:@
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandler :: (IsWKWebView wkWebView, IsNSString functionBody, IsNSDictionary arguments, IsWKFrameInfo frame, IsWKContentWorld contentWorld) => wkWebView -> functionBody -> arguments -> frame -> contentWorld -> Ptr () -> IO ()
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandler wkWebView  functionBody arguments frame contentWorld completionHandler =
  withObjCPtr functionBody $ \raw_functionBody ->
    withObjCPtr arguments $ \raw_arguments ->
      withObjCPtr frame $ \raw_frame ->
        withObjCPtr contentWorld $ \raw_contentWorld ->
            sendMsg wkWebView (mkSelector "callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:") retVoid [argPtr (castPtr raw_functionBody :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_frame :: Ptr ()), argPtr (castPtr raw_contentWorld :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Closes all out-of-window media presentations in a WKWebView.
--
-- Includes picture-in-picture and fullscreen.
--
-- ObjC selector: @- closeAllMediaPresentationsWithCompletionHandler:@
closeAllMediaPresentationsWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
closeAllMediaPresentationsWithCompletionHandler wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "closeAllMediaPresentationsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- closeAllMediaPresentations@
closeAllMediaPresentations :: IsWKWebView wkWebView => wkWebView -> IO ()
closeAllMediaPresentations wkWebView  =
    sendMsg wkWebView (mkSelector "closeAllMediaPresentations") retVoid []

-- | Pauses media playback in WKWebView.
--
-- Pauses media playback. Media in the page can be restarted by calling play() on a media element or resume() on an AudioContext in JavaScript. A user can also use media controls to play media content after it has been paused.
--
-- ObjC selector: @- pauseAllMediaPlaybackWithCompletionHandler:@
pauseAllMediaPlaybackWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
pauseAllMediaPlaybackWithCompletionHandler wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "pauseAllMediaPlaybackWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseAllMediaPlayback:@
pauseAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
pauseAllMediaPlayback wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "pauseAllMediaPlayback:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Suspends or resumes all media playback in WKWebView.
--
-- @suspended@ — Whether media playback should be suspended or resumed.
--
-- If suspended is true, this pauses media playback and blocks all attempts by the page or the user to resume until setAllMediaPlaybackSuspended is called again with suspended set to false. Media playback should always be suspended and resumed in pairs.
--
-- ObjC selector: @- setAllMediaPlaybackSuspended:completionHandler:@
setAllMediaPlaybackSuspended_completionHandler :: IsWKWebView wkWebView => wkWebView -> Bool -> Ptr () -> IO ()
setAllMediaPlaybackSuspended_completionHandler wkWebView  suspended completionHandler =
    sendMsg wkWebView (mkSelector "setAllMediaPlaybackSuspended:completionHandler:") retVoid [argCULong (if suspended then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resumeAllMediaPlayback:@
resumeAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
resumeAllMediaPlayback wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "resumeAllMediaPlayback:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- suspendAllMediaPlayback:@
suspendAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
suspendAllMediaPlayback wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "suspendAllMediaPlayback:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Get the current media playback state of a WKWebView.
--
-- @completionHandler@ — A block to invoke with the return value of the function call.
--
-- If media playback exists, WKMediaPlaybackState will be one of three values: WKMediaPlaybackPaused, WKMediaPlaybackSuspended, or WKMediaPlaybackPlaying. If no media playback exists in the current WKWebView, WKMediaPlaybackState will equal WKMediaPlaybackStateNone.
--
-- ObjC selector: @- requestMediaPlaybackStateWithCompletionHandler:@
requestMediaPlaybackStateWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
requestMediaPlaybackStateWithCompletionHandler wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "requestMediaPlaybackStateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestMediaPlaybackState:@
requestMediaPlaybackState :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
requestMediaPlaybackState wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "requestMediaPlaybackState:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Set camera capture state of a WKWebView.
--
-- @state@ — State to apply for capture.
--
-- @completionHandler@ — A block to invoke after the camera state has been changed.
--
-- If value is WKMediaCaptureStateNone, this will stop any camera capture. If value is WKMediaCaptureStateMuted, any active camera capture will become muted. If value is WKMediaCaptureStateActive, any muted camera capture will become active.
--
-- ObjC selector: @- setCameraCaptureState:completionHandler:@
setCameraCaptureState_completionHandler :: IsWKWebView wkWebView => wkWebView -> WKMediaCaptureState -> Ptr () -> IO ()
setCameraCaptureState_completionHandler wkWebView  state completionHandler =
    sendMsg wkWebView (mkSelector "setCameraCaptureState:completionHandler:") retVoid [argCLong (coerce state), argPtr (castPtr completionHandler :: Ptr ())]

-- | Set microphone capture state of a WKWebView.
--
-- @state@ — state to apply for capture.
--
-- @completionHandler@ — A block to invoke after the microphone state has been changed.
--
-- If value is WKMediaCaptureStateNone, this will stop any microphone capture. If value is WKMediaCaptureStateMuted, any active microphone capture will become muted. If value is WKMediaCaptureStateActive, any muted microphone capture will become active.
--
-- ObjC selector: @- setMicrophoneCaptureState:completionHandler:@
setMicrophoneCaptureState_completionHandler :: IsWKWebView wkWebView => wkWebView -> WKMediaCaptureState -> Ptr () -> IO ()
setMicrophoneCaptureState_completionHandler wkWebView  state completionHandler =
    sendMsg wkWebView (mkSelector "setMicrophoneCaptureState:completionHandler:") retVoid [argCLong (coerce state), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- takeSnapshotWithConfiguration:completionHandler:@
takeSnapshotWithConfiguration_completionHandler :: (IsWKWebView wkWebView, IsWKSnapshotConfiguration snapshotConfiguration) => wkWebView -> snapshotConfiguration -> Ptr () -> IO ()
takeSnapshotWithConfiguration_completionHandler wkWebView  snapshotConfiguration completionHandler =
  withObjCPtr snapshotConfiguration $ \raw_snapshotConfiguration ->
      sendMsg wkWebView (mkSelector "takeSnapshotWithConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_snapshotConfiguration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Create a PDF document representation from the web page currently displayed in the WKWebView
--
-- @pdfConfiguration@ — An object that specifies how the PDF capture is configured.
--
-- @completionHandler@ — A block to invoke when the pdf document data is ready.
--
-- If the WKPDFConfiguration is nil, the method will create a PDF document representing the bounds of the currently displayed web page.The completionHandler is passed the resulting PDF document data or an error.The data can be used to create a PDFDocument object.If the data is written to a file the resulting file is a valid PDF document.
--
-- ObjC selector: @- createPDFWithConfiguration:completionHandler:@
createPDFWithConfiguration_completionHandler :: (IsWKWebView wkWebView, IsWKPDFConfiguration pdfConfiguration) => wkWebView -> pdfConfiguration -> Ptr () -> IO ()
createPDFWithConfiguration_completionHandler wkWebView  pdfConfiguration completionHandler =
  withObjCPtr pdfConfiguration $ \raw_pdfConfiguration ->
      sendMsg wkWebView (mkSelector "createPDFWithConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_pdfConfiguration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- createWebArchiveDataWithCompletionHandler:@
createWebArchiveDataWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
createWebArchiveDataWithCompletionHandler wkWebView  completionHandler =
    sendMsg wkWebView (mkSelector "createWebArchiveDataWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- findString:withConfiguration:completionHandler:@
findString_withConfiguration_completionHandler :: (IsWKWebView wkWebView, IsNSString string, IsWKFindConfiguration configuration) => wkWebView -> string -> configuration -> Ptr () -> IO ()
findString_withConfiguration_completionHandler wkWebView  string configuration completionHandler =
  withObjCPtr string $ \raw_string ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg wkWebView (mkSelector "findString:withConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ handlesURLScheme:@
handlesURLScheme :: IsNSString urlScheme => urlScheme -> IO Bool
handlesURLScheme urlScheme =
  do
    cls' <- getRequiredClass "WKWebView"
    withObjCPtr urlScheme $ \raw_urlScheme ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "handlesURLScheme:") retCULong [argPtr (castPtr raw_urlScheme :: Ptr ())]

-- | @- startDownloadUsingRequest:completionHandler:@
startDownloadUsingRequest_completionHandler :: (IsWKWebView wkWebView, IsNSURLRequest request) => wkWebView -> request -> Ptr () -> IO ()
startDownloadUsingRequest_completionHandler wkWebView  request completionHandler =
  withObjCPtr request $ \raw_request ->
      sendMsg wkWebView (mkSelector "startDownloadUsingRequest:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resumeDownloadFromResumeData:completionHandler:@
resumeDownloadFromResumeData_completionHandler :: (IsWKWebView wkWebView, IsNSData resumeData) => wkWebView -> resumeData -> Ptr () -> IO ()
resumeDownloadFromResumeData_completionHandler wkWebView  resumeData completionHandler =
  withObjCPtr resumeData $ \raw_resumeData ->
      sendMsg wkWebView (mkSelector "resumeDownloadFromResumeData:completionHandler:") retVoid [argPtr (castPtr raw_resumeData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Sets the webpage contents from the passed data as if it was the response to the supplied request. The request is never actually sent to the supplied URL, though loads of resources defined in the NSData object would be performed.
--
-- @request@ — The request specifying the base URL and other loading details to be used while interpreting the supplied data object.
--
-- @response@ — A response that is used to interpret the supplied data object.
--
-- @data@ — The data to use as the contents of the webpage.
--
-- Returns: A new navigation.
--
-- ObjC selector: @- loadSimulatedRequest:response:responseData:@
loadSimulatedRequest_response_responseData :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSURLResponse response, IsNSData data_) => wkWebView -> request -> response -> data_ -> IO (Id WKNavigation)
loadSimulatedRequest_response_responseData wkWebView  request response data_ =
  withObjCPtr request $ \raw_request ->
    withObjCPtr response $ \raw_response ->
      withObjCPtr data_ $ \raw_data_ ->
          sendMsg wkWebView (mkSelector "loadSimulatedRequest:response:responseData:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- loadSimulatedRequest:withResponse:responseData:@
loadSimulatedRequest_withResponse_responseData :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSURLResponse response, IsNSData data_) => wkWebView -> request -> response -> data_ -> IO (Id WKNavigation)
loadSimulatedRequest_withResponse_responseData wkWebView  request response data_ =
  withObjCPtr request $ \raw_request ->
    withObjCPtr response $ \raw_response ->
      withObjCPtr data_ $ \raw_data_ ->
          sendMsg wkWebView (mkSelector "loadSimulatedRequest:withResponse:responseData:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | Navigates to the requested file URL on the filesystem.
--
-- @request@ — The request specifying the file URL to which to navigate.
--
-- @readAccessURL@ — The URL to allow read access to.
--
-- If readAccessURL references a single file, only that file may be loaded by WebKit. If readAccessURL references a directory, files inside that file may be loaded by WebKit.
--
-- Returns: A new navigation for the given file URL.
--
-- ObjC selector: @- loadFileRequest:allowingReadAccessToURL:@
loadFileRequest_allowingReadAccessToURL :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSURL readAccessURL) => wkWebView -> request -> readAccessURL -> IO (Id WKNavigation)
loadFileRequest_allowingReadAccessToURL wkWebView  request readAccessURL =
  withObjCPtr request $ \raw_request ->
    withObjCPtr readAccessURL $ \raw_readAccessURL ->
        sendMsg wkWebView (mkSelector "loadFileRequest:allowingReadAccessToURL:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_readAccessURL :: Ptr ())] >>= retainedObject . castPtr

-- | Sets the webpage contents from the passed HTML string as if it was the response to the supplied request. The request is never actually sent to the supplied URL, though loads of resources defined in the HTML string would be performed.
--
-- @request@ — The request specifying the base URL and other loading details to be used while interpreting the supplied data object.
--
-- @string@ — The data to use as the contents of the webpage.
--
-- Returns: A new navigation.
--
-- ObjC selector: @- loadSimulatedRequest:responseHTMLString:@
loadSimulatedRequest_responseHTMLString :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSString string) => wkWebView -> request -> string -> IO (Id WKNavigation)
loadSimulatedRequest_responseHTMLString wkWebView  request string =
  withObjCPtr request $ \raw_request ->
    withObjCPtr string $ \raw_string ->
        sendMsg wkWebView (mkSelector "loadSimulatedRequest:responseHTMLString:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- loadSimulatedRequest:withResponseHTMLString:@
loadSimulatedRequest_withResponseHTMLString :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSString string) => wkWebView -> request -> string -> IO (Id WKNavigation)
loadSimulatedRequest_withResponseHTMLString wkWebView  request string =
  withObjCPtr request $ \raw_request ->
    withObjCPtr string $ \raw_string ->
        sendMsg wkWebView (mkSelector "loadSimulatedRequest:withResponseHTMLString:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- printOperationWithPrintInfo:@
printOperationWithPrintInfo :: (IsWKWebView wkWebView, IsNSPrintInfo printInfo) => wkWebView -> printInfo -> IO (Id NSPrintOperation)
printOperationWithPrintInfo wkWebView  printInfo =
  withObjCPtr printInfo $ \raw_printInfo ->
      sendMsg wkWebView (mkSelector "printOperationWithPrintInfo:") (retPtr retVoid) [argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- setMinimumViewportInset:maximumViewportInset:@
setMinimumViewportInset_maximumViewportInset :: IsWKWebView wkWebView => wkWebView -> NSEdgeInsets -> NSEdgeInsets -> IO ()
setMinimumViewportInset_maximumViewportInset wkWebView  minimumViewportInset maximumViewportInset =
    sendMsg wkWebView (mkSelector "setMinimumViewportInset:maximumViewportInset:") retVoid [argNSEdgeInsets minimumViewportInset, argNSEdgeInsets maximumViewportInset]

-- | @- fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandler :: IsWKWebView wkWebView => wkWebView -> WKWebViewDataType -> Ptr () -> IO ()
fetchDataOfTypes_completionHandler wkWebView  dataTypes completionHandler =
    sendMsg wkWebView (mkSelector "fetchDataOfTypes:completionHandler:") retVoid [argCULong (coerce dataTypes), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- restoreData:completionHandler:@
restoreData_completionHandler :: (IsWKWebView wkWebView, IsNSData data_) => wkWebView -> data_ -> Ptr () -> IO ()
restoreData_completionHandler wkWebView  data_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg wkWebView (mkSelector "restoreData:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | A copy of the configuration with which the web view was initialized.
--
-- ObjC selector: @- configuration@
configuration :: IsWKWebView wkWebView => wkWebView -> IO (Id WKWebViewConfiguration)
configuration wkWebView  =
    sendMsg wkWebView (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The web view's navigation delegate.
--
-- ObjC selector: @- navigationDelegate@
navigationDelegate :: IsWKWebView wkWebView => wkWebView -> IO RawId
navigationDelegate wkWebView  =
    fmap (RawId . castPtr) $ sendMsg wkWebView (mkSelector "navigationDelegate") (retPtr retVoid) []

-- | The web view's navigation delegate.
--
-- ObjC selector: @- setNavigationDelegate:@
setNavigationDelegate :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setNavigationDelegate wkWebView  value =
    sendMsg wkWebView (mkSelector "setNavigationDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The web view's user interface delegate.
--
-- ObjC selector: @- UIDelegate@
uiDelegate :: IsWKWebView wkWebView => wkWebView -> IO RawId
uiDelegate wkWebView  =
    fmap (RawId . castPtr) $ sendMsg wkWebView (mkSelector "UIDelegate") (retPtr retVoid) []

-- | The web view's user interface delegate.
--
-- ObjC selector: @- setUIDelegate:@
setUIDelegate :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setUIDelegate wkWebView  value =
    sendMsg wkWebView (mkSelector "setUIDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The web view's back-forward list.
--
-- ObjC selector: @- backForwardList@
backForwardList :: IsWKWebView wkWebView => wkWebView -> IO (Id WKBackForwardList)
backForwardList wkWebView  =
    sendMsg wkWebView (mkSelector "backForwardList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The page title.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- title@
title :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
title wkWebView  =
    sendMsg wkWebView (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The active URL.
--
-- This is the URL that should be reflected in the user interface.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- URL@
url :: IsWKWebView wkWebView => wkWebView -> IO (Id NSURL)
url wkWebView  =
    sendMsg wkWebView (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value indicating whether the view is currently loading content.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- loading@
loading :: IsWKWebView wkWebView => wkWebView -> IO Bool
loading wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "loading") retCULong []

-- | An estimate of what fraction of the current navigation has been completed.
--
-- This value ranges from 0.0 to 1.0 based on the total number of bytes expected to be received, including the main document and all of its potential subresources. After a navigation completes, the value remains at 1.0 until a new navigation starts, at which point it is reset to 0.0.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- estimatedProgress@
estimatedProgress :: IsWKWebView wkWebView => wkWebView -> IO CDouble
estimatedProgress wkWebView  =
    sendMsg wkWebView (mkSelector "estimatedProgress") retCDouble []

-- | A Boolean value indicating whether all resources on the page have been loaded over securely encrypted connections.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- hasOnlySecureContent@
hasOnlySecureContent :: IsWKWebView wkWebView => wkWebView -> IO Bool
hasOnlySecureContent wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "hasOnlySecureContent") retCULong []

-- | A SecTrustRef for the currently committed navigation.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant  for this property.
--
-- ObjC selector: @- serverTrust@
serverTrust :: IsWKWebView wkWebView => wkWebView -> IO (Ptr ())
serverTrust wkWebView  =
    fmap castPtr $ sendMsg wkWebView (mkSelector "serverTrust") (retPtr retVoid) []

-- | A Boolean value indicating whether there is a back item in the back-forward list that can be navigated to.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- backForwardList.
--
-- ObjC selector: @- canGoBack@
canGoBack :: IsWKWebView wkWebView => wkWebView -> IO Bool
canGoBack wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "canGoBack") retCULong []

-- | A Boolean value indicating whether there is a forward item in the back-forward list that can be navigated to.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- backForwardList.
--
-- ObjC selector: @- canGoForward@
canGoForward :: IsWKWebView wkWebView => wkWebView -> IO Bool
canGoForward wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "canGoForward") retCULong []

-- | The state of camera capture on a web page.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- cameraCaptureState@
cameraCaptureState :: IsWKWebView wkWebView => wkWebView -> IO WKMediaCaptureState
cameraCaptureState wkWebView  =
    fmap (coerce :: CLong -> WKMediaCaptureState) $ sendMsg wkWebView (mkSelector "cameraCaptureState") retCLong []

-- | The state of microphone capture on a web page.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- microphoneCaptureState@
microphoneCaptureState :: IsWKWebView wkWebView => wkWebView -> IO WKMediaCaptureState
microphoneCaptureState wkWebView  =
    fmap (coerce :: CLong -> WKMediaCaptureState) $ sendMsg wkWebView (mkSelector "microphoneCaptureState") retCLong []

-- | A Boolean value indicating whether horizontal swipe gestures will trigger back-forward list navigations.
--
-- The default value is NO.
--
-- ObjC selector: @- allowsBackForwardNavigationGestures@
allowsBackForwardNavigationGestures :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsBackForwardNavigationGestures wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "allowsBackForwardNavigationGestures") retCULong []

-- | A Boolean value indicating whether horizontal swipe gestures will trigger back-forward list navigations.
--
-- The default value is NO.
--
-- ObjC selector: @- setAllowsBackForwardNavigationGestures:@
setAllowsBackForwardNavigationGestures :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsBackForwardNavigationGestures wkWebView  value =
    sendMsg wkWebView (mkSelector "setAllowsBackForwardNavigationGestures:") retVoid [argCULong (if value then 1 else 0)]

-- | The custom user agent string or nil if no custom user agent string has been set.
--
-- ObjC selector: @- customUserAgent@
customUserAgent :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
customUserAgent wkWebView  =
    sendMsg wkWebView (mkSelector "customUserAgent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The custom user agent string or nil if no custom user agent string has been set.
--
-- ObjC selector: @- setCustomUserAgent:@
setCustomUserAgent :: (IsWKWebView wkWebView, IsNSString value) => wkWebView -> value -> IO ()
setCustomUserAgent wkWebView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg wkWebView (mkSelector "setCustomUserAgent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether link preview is allowed for any links inside this WKWebView.
--
-- The default value is YES on Mac and iOS.
--
-- ObjC selector: @- allowsLinkPreview@
allowsLinkPreview :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsLinkPreview wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "allowsLinkPreview") retCULong []

-- | A Boolean value indicating whether link preview is allowed for any links inside this WKWebView.
--
-- The default value is YES on Mac and iOS.
--
-- ObjC selector: @- setAllowsLinkPreview:@
setAllowsLinkPreview :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsLinkPreview wkWebView  value =
    sendMsg wkWebView (mkSelector "setAllowsLinkPreview:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMagnification@
allowsMagnification :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsMagnification wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "allowsMagnification") retCULong []

-- | @- setAllowsMagnification:@
setAllowsMagnification :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsMagnification wkWebView  value =
    sendMsg wkWebView (mkSelector "setAllowsMagnification:") retVoid [argCULong (if value then 1 else 0)]

-- | @- magnification@
magnification :: IsWKWebView wkWebView => wkWebView -> IO CDouble
magnification wkWebView  =
    sendMsg wkWebView (mkSelector "magnification") retCDouble []

-- | @- setMagnification:@
setMagnification :: IsWKWebView wkWebView => wkWebView -> CDouble -> IO ()
setMagnification wkWebView  value =
    sendMsg wkWebView (mkSelector "setMagnification:") retVoid [argCDouble value]

-- | @- pageZoom@
pageZoom :: IsWKWebView wkWebView => wkWebView -> IO CDouble
pageZoom wkWebView  =
    sendMsg wkWebView (mkSelector "pageZoom") retCDouble []

-- | @- setPageZoom:@
setPageZoom :: IsWKWebView wkWebView => wkWebView -> CDouble -> IO ()
setPageZoom wkWebView  value =
    sendMsg wkWebView (mkSelector "setPageZoom:") retVoid [argCDouble value]

-- | @- mediaType@
mediaType :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
mediaType wkWebView  =
    sendMsg wkWebView (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMediaType:@
setMediaType :: (IsWKWebView wkWebView, IsNSString value) => wkWebView -> value -> IO ()
setMediaType wkWebView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg wkWebView (mkSelector "setMediaType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interactionState@
interactionState :: IsWKWebView wkWebView => wkWebView -> IO RawId
interactionState wkWebView  =
    fmap (RawId . castPtr) $ sendMsg wkWebView (mkSelector "interactionState") (retPtr retVoid) []

-- | @- setInteractionState:@
setInteractionState :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setInteractionState wkWebView  value =
    sendMsg wkWebView (mkSelector "setInteractionState:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A Boolean value indicating whether Screen Time blocking has occurred.
--
-- ObjC selector: @- isBlockedByScreenTime@
isBlockedByScreenTime :: IsWKWebView wkWebView => wkWebView -> IO Bool
isBlockedByScreenTime wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "isBlockedByScreenTime") retCULong []

-- | @- themeColor@
themeColor :: IsWKWebView wkWebView => wkWebView -> IO (Id NSColor)
themeColor wkWebView  =
    sendMsg wkWebView (mkSelector "themeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- underPageBackgroundColor@
underPageBackgroundColor :: IsWKWebView wkWebView => wkWebView -> IO (Id NSColor)
underPageBackgroundColor wkWebView  =
    sendMsg wkWebView (mkSelector "underPageBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUnderPageBackgroundColor:@
setUnderPageBackgroundColor :: (IsWKWebView wkWebView, IsNSColor value) => wkWebView -> value -> IO ()
setUnderPageBackgroundColor wkWebView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg wkWebView (mkSelector "setUnderPageBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A WKWebView's fullscreen state.
--
-- WKWebView @link is key-value observing (KVO) compliant for this property. When an element
-- in the WKWebView enters fullscreen, WebKit will replace the WKWebView in the application view hierarchy with
-- a "placeholder" view, and move the WKWebView into a fullscreen window. When the element exits fullscreen later,
-- the WKWebView will be moved back into the application view hierarchy. An application may need to adjust/restore
-- its native UI components when the fullscreen state changes. The application should observe the fullscreenState
-- property of WKWebView in order to receive notifications regarding the fullscreen state change.
--
-- ObjC selector: @- fullscreenState@
fullscreenState :: IsWKWebView wkWebView => wkWebView -> IO WKFullscreenState
fullscreenState wkWebView  =
    fmap (coerce :: CLong -> WKFullscreenState) $ sendMsg wkWebView (mkSelector "fullscreenState") retCLong []

-- | @- minimumViewportInset@
minimumViewportInset :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
minimumViewportInset wkWebView  =
    sendMsgStret wkWebView (mkSelector "minimumViewportInset") retNSEdgeInsets []

-- | @- maximumViewportInset@
maximumViewportInset :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
maximumViewportInset wkWebView  =
    sendMsgStret wkWebView (mkSelector "maximumViewportInset") retNSEdgeInsets []

-- | Controls whether this
--
-- WKWebView
--
-- is inspectable in Web Inspector.
--
-- The default value is NO.
--
-- ObjC selector: @- inspectable@
inspectable :: IsWKWebView wkWebView => wkWebView -> IO Bool
inspectable wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "inspectable") retCULong []

-- | Controls whether this
--
-- WKWebView
--
-- is inspectable in Web Inspector.
--
-- The default value is NO.
--
-- ObjC selector: @- setInspectable:@
setInspectable :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setInspectable wkWebView  value =
    sendMsg wkWebView (mkSelector "setInspectable:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether Writing Tools is active for the view.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- writingToolsActive@
writingToolsActive :: IsWKWebView wkWebView => wkWebView -> IO Bool
writingToolsActive wkWebView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebView (mkSelector "writingToolsActive") retCULong []

-- | @- obscuredContentInsets@
obscuredContentInsets :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
obscuredContentInsets wkWebView  =
    sendMsgStret wkWebView (mkSelector "obscuredContentInsets") retNSEdgeInsets []

-- | @- setObscuredContentInsets:@
setObscuredContentInsets :: IsWKWebView wkWebView => wkWebView -> NSEdgeInsets -> IO ()
setObscuredContentInsets wkWebView  value =
    sendMsg wkWebView (mkSelector "setObscuredContentInsets:") retVoid [argNSEdgeInsets value]

-- | @- certificateChain@
certificateChain :: IsWKWebView wkWebView => wkWebView -> IO (Id NSArray)
certificateChain wkWebView  =
    sendMsg wkWebView (mkSelector "certificateChain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @loadRequest:@
loadRequestSelector :: Selector
loadRequestSelector = mkSelector "loadRequest:"

-- | @Selector@ for @loadFileURL:allowingReadAccessToURL:@
loadFileURL_allowingReadAccessToURLSelector :: Selector
loadFileURL_allowingReadAccessToURLSelector = mkSelector "loadFileURL:allowingReadAccessToURL:"

-- | @Selector@ for @loadHTMLString:baseURL:@
loadHTMLString_baseURLSelector :: Selector
loadHTMLString_baseURLSelector = mkSelector "loadHTMLString:baseURL:"

-- | @Selector@ for @loadData:MIMEType:characterEncodingName:baseURL:@
loadData_MIMEType_characterEncodingName_baseURLSelector :: Selector
loadData_MIMEType_characterEncodingName_baseURLSelector = mkSelector "loadData:MIMEType:characterEncodingName:baseURL:"

-- | @Selector@ for @goToBackForwardListItem:@
goToBackForwardListItemSelector :: Selector
goToBackForwardListItemSelector = mkSelector "goToBackForwardListItem:"

-- | @Selector@ for @goBack@
goBackSelector :: Selector
goBackSelector = mkSelector "goBack"

-- | @Selector@ for @goForward@
goForwardSelector :: Selector
goForwardSelector = mkSelector "goForward"

-- | @Selector@ for @reload@
reloadSelector :: Selector
reloadSelector = mkSelector "reload"

-- | @Selector@ for @reloadFromOrigin@
reloadFromOriginSelector :: Selector
reloadFromOriginSelector = mkSelector "reloadFromOrigin"

-- | @Selector@ for @stopLoading@
stopLoadingSelector :: Selector
stopLoadingSelector = mkSelector "stopLoading"

-- | @Selector@ for @evaluateJavaScript:completionHandler:@
evaluateJavaScript_completionHandlerSelector :: Selector
evaluateJavaScript_completionHandlerSelector = mkSelector "evaluateJavaScript:completionHandler:"

-- | @Selector@ for @evaluateJavaScript:inFrame:inContentWorld:completionHandler:@
evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector :: Selector
evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector = mkSelector "evaluateJavaScript:inFrame:inContentWorld:completionHandler:"

-- | @Selector@ for @callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:@
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector :: Selector
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector = mkSelector "callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:"

-- | @Selector@ for @closeAllMediaPresentationsWithCompletionHandler:@
closeAllMediaPresentationsWithCompletionHandlerSelector :: Selector
closeAllMediaPresentationsWithCompletionHandlerSelector = mkSelector "closeAllMediaPresentationsWithCompletionHandler:"

-- | @Selector@ for @closeAllMediaPresentations@
closeAllMediaPresentationsSelector :: Selector
closeAllMediaPresentationsSelector = mkSelector "closeAllMediaPresentations"

-- | @Selector@ for @pauseAllMediaPlaybackWithCompletionHandler:@
pauseAllMediaPlaybackWithCompletionHandlerSelector :: Selector
pauseAllMediaPlaybackWithCompletionHandlerSelector = mkSelector "pauseAllMediaPlaybackWithCompletionHandler:"

-- | @Selector@ for @pauseAllMediaPlayback:@
pauseAllMediaPlaybackSelector :: Selector
pauseAllMediaPlaybackSelector = mkSelector "pauseAllMediaPlayback:"

-- | @Selector@ for @setAllMediaPlaybackSuspended:completionHandler:@
setAllMediaPlaybackSuspended_completionHandlerSelector :: Selector
setAllMediaPlaybackSuspended_completionHandlerSelector = mkSelector "setAllMediaPlaybackSuspended:completionHandler:"

-- | @Selector@ for @resumeAllMediaPlayback:@
resumeAllMediaPlaybackSelector :: Selector
resumeAllMediaPlaybackSelector = mkSelector "resumeAllMediaPlayback:"

-- | @Selector@ for @suspendAllMediaPlayback:@
suspendAllMediaPlaybackSelector :: Selector
suspendAllMediaPlaybackSelector = mkSelector "suspendAllMediaPlayback:"

-- | @Selector@ for @requestMediaPlaybackStateWithCompletionHandler:@
requestMediaPlaybackStateWithCompletionHandlerSelector :: Selector
requestMediaPlaybackStateWithCompletionHandlerSelector = mkSelector "requestMediaPlaybackStateWithCompletionHandler:"

-- | @Selector@ for @requestMediaPlaybackState:@
requestMediaPlaybackStateSelector :: Selector
requestMediaPlaybackStateSelector = mkSelector "requestMediaPlaybackState:"

-- | @Selector@ for @setCameraCaptureState:completionHandler:@
setCameraCaptureState_completionHandlerSelector :: Selector
setCameraCaptureState_completionHandlerSelector = mkSelector "setCameraCaptureState:completionHandler:"

-- | @Selector@ for @setMicrophoneCaptureState:completionHandler:@
setMicrophoneCaptureState_completionHandlerSelector :: Selector
setMicrophoneCaptureState_completionHandlerSelector = mkSelector "setMicrophoneCaptureState:completionHandler:"

-- | @Selector@ for @takeSnapshotWithConfiguration:completionHandler:@
takeSnapshotWithConfiguration_completionHandlerSelector :: Selector
takeSnapshotWithConfiguration_completionHandlerSelector = mkSelector "takeSnapshotWithConfiguration:completionHandler:"

-- | @Selector@ for @createPDFWithConfiguration:completionHandler:@
createPDFWithConfiguration_completionHandlerSelector :: Selector
createPDFWithConfiguration_completionHandlerSelector = mkSelector "createPDFWithConfiguration:completionHandler:"

-- | @Selector@ for @createWebArchiveDataWithCompletionHandler:@
createWebArchiveDataWithCompletionHandlerSelector :: Selector
createWebArchiveDataWithCompletionHandlerSelector = mkSelector "createWebArchiveDataWithCompletionHandler:"

-- | @Selector@ for @findString:withConfiguration:completionHandler:@
findString_withConfiguration_completionHandlerSelector :: Selector
findString_withConfiguration_completionHandlerSelector = mkSelector "findString:withConfiguration:completionHandler:"

-- | @Selector@ for @handlesURLScheme:@
handlesURLSchemeSelector :: Selector
handlesURLSchemeSelector = mkSelector "handlesURLScheme:"

-- | @Selector@ for @startDownloadUsingRequest:completionHandler:@
startDownloadUsingRequest_completionHandlerSelector :: Selector
startDownloadUsingRequest_completionHandlerSelector = mkSelector "startDownloadUsingRequest:completionHandler:"

-- | @Selector@ for @resumeDownloadFromResumeData:completionHandler:@
resumeDownloadFromResumeData_completionHandlerSelector :: Selector
resumeDownloadFromResumeData_completionHandlerSelector = mkSelector "resumeDownloadFromResumeData:completionHandler:"

-- | @Selector@ for @loadSimulatedRequest:response:responseData:@
loadSimulatedRequest_response_responseDataSelector :: Selector
loadSimulatedRequest_response_responseDataSelector = mkSelector "loadSimulatedRequest:response:responseData:"

-- | @Selector@ for @loadSimulatedRequest:withResponse:responseData:@
loadSimulatedRequest_withResponse_responseDataSelector :: Selector
loadSimulatedRequest_withResponse_responseDataSelector = mkSelector "loadSimulatedRequest:withResponse:responseData:"

-- | @Selector@ for @loadFileRequest:allowingReadAccessToURL:@
loadFileRequest_allowingReadAccessToURLSelector :: Selector
loadFileRequest_allowingReadAccessToURLSelector = mkSelector "loadFileRequest:allowingReadAccessToURL:"

-- | @Selector@ for @loadSimulatedRequest:responseHTMLString:@
loadSimulatedRequest_responseHTMLStringSelector :: Selector
loadSimulatedRequest_responseHTMLStringSelector = mkSelector "loadSimulatedRequest:responseHTMLString:"

-- | @Selector@ for @loadSimulatedRequest:withResponseHTMLString:@
loadSimulatedRequest_withResponseHTMLStringSelector :: Selector
loadSimulatedRequest_withResponseHTMLStringSelector = mkSelector "loadSimulatedRequest:withResponseHTMLString:"

-- | @Selector@ for @printOperationWithPrintInfo:@
printOperationWithPrintInfoSelector :: Selector
printOperationWithPrintInfoSelector = mkSelector "printOperationWithPrintInfo:"

-- | @Selector@ for @setMinimumViewportInset:maximumViewportInset:@
setMinimumViewportInset_maximumViewportInsetSelector :: Selector
setMinimumViewportInset_maximumViewportInsetSelector = mkSelector "setMinimumViewportInset:maximumViewportInset:"

-- | @Selector@ for @fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandlerSelector :: Selector
fetchDataOfTypes_completionHandlerSelector = mkSelector "fetchDataOfTypes:completionHandler:"

-- | @Selector@ for @restoreData:completionHandler:@
restoreData_completionHandlerSelector :: Selector
restoreData_completionHandlerSelector = mkSelector "restoreData:completionHandler:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @navigationDelegate@
navigationDelegateSelector :: Selector
navigationDelegateSelector = mkSelector "navigationDelegate"

-- | @Selector@ for @setNavigationDelegate:@
setNavigationDelegateSelector :: Selector
setNavigationDelegateSelector = mkSelector "setNavigationDelegate:"

-- | @Selector@ for @UIDelegate@
uiDelegateSelector :: Selector
uiDelegateSelector = mkSelector "UIDelegate"

-- | @Selector@ for @setUIDelegate:@
setUIDelegateSelector :: Selector
setUIDelegateSelector = mkSelector "setUIDelegate:"

-- | @Selector@ for @backForwardList@
backForwardListSelector :: Selector
backForwardListSelector = mkSelector "backForwardList"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

-- | @Selector@ for @estimatedProgress@
estimatedProgressSelector :: Selector
estimatedProgressSelector = mkSelector "estimatedProgress"

-- | @Selector@ for @hasOnlySecureContent@
hasOnlySecureContentSelector :: Selector
hasOnlySecureContentSelector = mkSelector "hasOnlySecureContent"

-- | @Selector@ for @serverTrust@
serverTrustSelector :: Selector
serverTrustSelector = mkSelector "serverTrust"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @cameraCaptureState@
cameraCaptureStateSelector :: Selector
cameraCaptureStateSelector = mkSelector "cameraCaptureState"

-- | @Selector@ for @microphoneCaptureState@
microphoneCaptureStateSelector :: Selector
microphoneCaptureStateSelector = mkSelector "microphoneCaptureState"

-- | @Selector@ for @allowsBackForwardNavigationGestures@
allowsBackForwardNavigationGesturesSelector :: Selector
allowsBackForwardNavigationGesturesSelector = mkSelector "allowsBackForwardNavigationGestures"

-- | @Selector@ for @setAllowsBackForwardNavigationGestures:@
setAllowsBackForwardNavigationGesturesSelector :: Selector
setAllowsBackForwardNavigationGesturesSelector = mkSelector "setAllowsBackForwardNavigationGestures:"

-- | @Selector@ for @customUserAgent@
customUserAgentSelector :: Selector
customUserAgentSelector = mkSelector "customUserAgent"

-- | @Selector@ for @setCustomUserAgent:@
setCustomUserAgentSelector :: Selector
setCustomUserAgentSelector = mkSelector "setCustomUserAgent:"

-- | @Selector@ for @allowsLinkPreview@
allowsLinkPreviewSelector :: Selector
allowsLinkPreviewSelector = mkSelector "allowsLinkPreview"

-- | @Selector@ for @setAllowsLinkPreview:@
setAllowsLinkPreviewSelector :: Selector
setAllowsLinkPreviewSelector = mkSelector "setAllowsLinkPreview:"

-- | @Selector@ for @allowsMagnification@
allowsMagnificationSelector :: Selector
allowsMagnificationSelector = mkSelector "allowsMagnification"

-- | @Selector@ for @setAllowsMagnification:@
setAllowsMagnificationSelector :: Selector
setAllowsMagnificationSelector = mkSelector "setAllowsMagnification:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector
setMagnificationSelector = mkSelector "setMagnification:"

-- | @Selector@ for @pageZoom@
pageZoomSelector :: Selector
pageZoomSelector = mkSelector "pageZoom"

-- | @Selector@ for @setPageZoom:@
setPageZoomSelector :: Selector
setPageZoomSelector = mkSelector "setPageZoom:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @setMediaType:@
setMediaTypeSelector :: Selector
setMediaTypeSelector = mkSelector "setMediaType:"

-- | @Selector@ for @interactionState@
interactionStateSelector :: Selector
interactionStateSelector = mkSelector "interactionState"

-- | @Selector@ for @setInteractionState:@
setInteractionStateSelector :: Selector
setInteractionStateSelector = mkSelector "setInteractionState:"

-- | @Selector@ for @isBlockedByScreenTime@
isBlockedByScreenTimeSelector :: Selector
isBlockedByScreenTimeSelector = mkSelector "isBlockedByScreenTime"

-- | @Selector@ for @themeColor@
themeColorSelector :: Selector
themeColorSelector = mkSelector "themeColor"

-- | @Selector@ for @underPageBackgroundColor@
underPageBackgroundColorSelector :: Selector
underPageBackgroundColorSelector = mkSelector "underPageBackgroundColor"

-- | @Selector@ for @setUnderPageBackgroundColor:@
setUnderPageBackgroundColorSelector :: Selector
setUnderPageBackgroundColorSelector = mkSelector "setUnderPageBackgroundColor:"

-- | @Selector@ for @fullscreenState@
fullscreenStateSelector :: Selector
fullscreenStateSelector = mkSelector "fullscreenState"

-- | @Selector@ for @minimumViewportInset@
minimumViewportInsetSelector :: Selector
minimumViewportInsetSelector = mkSelector "minimumViewportInset"

-- | @Selector@ for @maximumViewportInset@
maximumViewportInsetSelector :: Selector
maximumViewportInsetSelector = mkSelector "maximumViewportInset"

-- | @Selector@ for @inspectable@
inspectableSelector :: Selector
inspectableSelector = mkSelector "inspectable"

-- | @Selector@ for @setInspectable:@
setInspectableSelector :: Selector
setInspectableSelector = mkSelector "setInspectable:"

-- | @Selector@ for @writingToolsActive@
writingToolsActiveSelector :: Selector
writingToolsActiveSelector = mkSelector "writingToolsActive"

-- | @Selector@ for @obscuredContentInsets@
obscuredContentInsetsSelector :: Selector
obscuredContentInsetsSelector = mkSelector "obscuredContentInsets"

-- | @Selector@ for @setObscuredContentInsets:@
setObscuredContentInsetsSelector :: Selector
setObscuredContentInsetsSelector = mkSelector "setObscuredContentInsets:"

-- | @Selector@ for @certificateChain@
certificateChainSelector :: Selector
certificateChainSelector = mkSelector "certificateChain"

