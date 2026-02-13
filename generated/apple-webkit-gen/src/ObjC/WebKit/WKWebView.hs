{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsBackForwardNavigationGesturesSelector
  , allowsLinkPreviewSelector
  , allowsMagnificationSelector
  , backForwardListSelector
  , callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector
  , cameraCaptureStateSelector
  , canGoBackSelector
  , canGoForwardSelector
  , certificateChainSelector
  , closeAllMediaPresentationsSelector
  , closeAllMediaPresentationsWithCompletionHandlerSelector
  , configurationSelector
  , createPDFWithConfiguration_completionHandlerSelector
  , createWebArchiveDataWithCompletionHandlerSelector
  , customUserAgentSelector
  , estimatedProgressSelector
  , evaluateJavaScript_completionHandlerSelector
  , evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector
  , fetchDataOfTypes_completionHandlerSelector
  , findString_withConfiguration_completionHandlerSelector
  , fullscreenStateSelector
  , goBackSelector
  , goForwardSelector
  , goToBackForwardListItemSelector
  , handlesURLSchemeSelector
  , hasOnlySecureContentSelector
  , initWithCoderSelector
  , inspectableSelector
  , interactionStateSelector
  , isBlockedByScreenTimeSelector
  , loadData_MIMEType_characterEncodingName_baseURLSelector
  , loadFileRequest_allowingReadAccessToURLSelector
  , loadFileURL_allowingReadAccessToURLSelector
  , loadHTMLString_baseURLSelector
  , loadRequestSelector
  , loadSimulatedRequest_responseHTMLStringSelector
  , loadSimulatedRequest_response_responseDataSelector
  , loadSimulatedRequest_withResponseHTMLStringSelector
  , loadSimulatedRequest_withResponse_responseDataSelector
  , loadingSelector
  , magnificationSelector
  , maximumViewportInsetSelector
  , mediaTypeSelector
  , microphoneCaptureStateSelector
  , minimumViewportInsetSelector
  , navigationDelegateSelector
  , obscuredContentInsetsSelector
  , pageZoomSelector
  , pauseAllMediaPlaybackSelector
  , pauseAllMediaPlaybackWithCompletionHandlerSelector
  , printOperationWithPrintInfoSelector
  , reloadFromOriginSelector
  , reloadSelector
  , requestMediaPlaybackStateSelector
  , requestMediaPlaybackStateWithCompletionHandlerSelector
  , restoreData_completionHandlerSelector
  , resumeAllMediaPlaybackSelector
  , resumeDownloadFromResumeData_completionHandlerSelector
  , serverTrustSelector
  , setAllMediaPlaybackSuspended_completionHandlerSelector
  , setAllowsBackForwardNavigationGesturesSelector
  , setAllowsLinkPreviewSelector
  , setAllowsMagnificationSelector
  , setCameraCaptureState_completionHandlerSelector
  , setCustomUserAgentSelector
  , setInspectableSelector
  , setInteractionStateSelector
  , setMagnificationSelector
  , setMediaTypeSelector
  , setMicrophoneCaptureState_completionHandlerSelector
  , setMinimumViewportInset_maximumViewportInsetSelector
  , setNavigationDelegateSelector
  , setObscuredContentInsetsSelector
  , setPageZoomSelector
  , setUIDelegateSelector
  , setUnderPageBackgroundColorSelector
  , startDownloadUsingRequest_completionHandlerSelector
  , stopLoadingSelector
  , suspendAllMediaPlaybackSelector
  , takeSnapshotWithConfiguration_completionHandlerSelector
  , themeColorSelector
  , titleSelector
  , uiDelegateSelector
  , underPageBackgroundColorSelector
  , urlSelector
  , writingToolsActiveSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsWKWebView wkWebView, IsNSCoder coder) => wkWebView -> coder -> IO (Id WKWebView)
initWithCoder wkWebView coder =
  sendOwnedMessage wkWebView initWithCoderSelector (toNSCoder coder)

-- | Navigates to a requested URL.
--
-- @request@ — The request specifying the URL to which to navigate.
--
-- Returns: A new navigation for the given request.
--
-- ObjC selector: @- loadRequest:@
loadRequest :: (IsWKWebView wkWebView, IsNSURLRequest request) => wkWebView -> request -> IO (Id WKNavigation)
loadRequest wkWebView request =
  sendMessage wkWebView loadRequestSelector (toNSURLRequest request)

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
loadFileURL_allowingReadAccessToURL wkWebView url readAccessURL =
  sendMessage wkWebView loadFileURL_allowingReadAccessToURLSelector (toNSURL url) (toNSURL readAccessURL)

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
loadHTMLString_baseURL wkWebView string baseURL =
  sendMessage wkWebView loadHTMLString_baseURLSelector (toNSString string) (toNSURL baseURL)

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
loadData_MIMEType_characterEncodingName_baseURL wkWebView data_ mimeType characterEncodingName baseURL =
  sendMessage wkWebView loadData_MIMEType_characterEncodingName_baseURLSelector (toNSData data_) (toNSString mimeType) (toNSString characterEncodingName) (toNSURL baseURL)

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
goToBackForwardListItem wkWebView item =
  sendMessage wkWebView goToBackForwardListItemSelector (toWKBackForwardListItem item)

-- | Navigates to the back item in the back-forward list.
--
-- Returns: A new navigation to the requested item, or nil if there is no back item in the back-forward list.
--
-- ObjC selector: @- goBack@
goBack :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
goBack wkWebView =
  sendMessage wkWebView goBackSelector

-- | Navigates to the forward item in the back-forward list.
--
-- Returns: A new navigation to the requested item, or nil if there is no forward item in the back-forward list.
--
-- ObjC selector: @- goForward@
goForward :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
goForward wkWebView =
  sendMessage wkWebView goForwardSelector

-- | Reloads the current page.
--
-- Returns: A new navigation representing the reload.
--
-- ObjC selector: @- reload@
reload :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
reload wkWebView =
  sendMessage wkWebView reloadSelector

-- | Reloads the current page, performing end-to-end revalidation using cache-validating conditionals if possible.
--
-- Returns: A new navigation representing the reload.
--
-- ObjC selector: @- reloadFromOrigin@
reloadFromOrigin :: IsWKWebView wkWebView => wkWebView -> IO (Id WKNavigation)
reloadFromOrigin wkWebView =
  sendMessage wkWebView reloadFromOriginSelector

-- | Stops loading all resources on the current page.
--
-- ObjC selector: @- stopLoading@
stopLoading :: IsWKWebView wkWebView => wkWebView -> IO ()
stopLoading wkWebView =
  sendMessage wkWebView stopLoadingSelector

-- | @- evaluateJavaScript:completionHandler:@
evaluateJavaScript_completionHandler :: (IsWKWebView wkWebView, IsNSString javaScriptString) => wkWebView -> javaScriptString -> Ptr () -> IO ()
evaluateJavaScript_completionHandler wkWebView javaScriptString completionHandler =
  sendMessage wkWebView evaluateJavaScript_completionHandlerSelector (toNSString javaScriptString) completionHandler

-- | @- evaluateJavaScript:inFrame:inContentWorld:completionHandler:@
evaluateJavaScript_inFrame_inContentWorld_completionHandler :: (IsWKWebView wkWebView, IsNSString javaScriptString, IsWKFrameInfo frame, IsWKContentWorld contentWorld) => wkWebView -> javaScriptString -> frame -> contentWorld -> Ptr () -> IO ()
evaluateJavaScript_inFrame_inContentWorld_completionHandler wkWebView javaScriptString frame contentWorld completionHandler =
  sendMessage wkWebView evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector (toNSString javaScriptString) (toWKFrameInfo frame) (toWKContentWorld contentWorld) completionHandler

-- | @- callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:@
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandler :: (IsWKWebView wkWebView, IsNSString functionBody, IsNSDictionary arguments, IsWKFrameInfo frame, IsWKContentWorld contentWorld) => wkWebView -> functionBody -> arguments -> frame -> contentWorld -> Ptr () -> IO ()
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandler wkWebView functionBody arguments frame contentWorld completionHandler =
  sendMessage wkWebView callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector (toNSString functionBody) (toNSDictionary arguments) (toWKFrameInfo frame) (toWKContentWorld contentWorld) completionHandler

-- | Closes all out-of-window media presentations in a WKWebView.
--
-- Includes picture-in-picture and fullscreen.
--
-- ObjC selector: @- closeAllMediaPresentationsWithCompletionHandler:@
closeAllMediaPresentationsWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
closeAllMediaPresentationsWithCompletionHandler wkWebView completionHandler =
  sendMessage wkWebView closeAllMediaPresentationsWithCompletionHandlerSelector completionHandler

-- | @- closeAllMediaPresentations@
closeAllMediaPresentations :: IsWKWebView wkWebView => wkWebView -> IO ()
closeAllMediaPresentations wkWebView =
  sendMessage wkWebView closeAllMediaPresentationsSelector

-- | Pauses media playback in WKWebView.
--
-- Pauses media playback. Media in the page can be restarted by calling play() on a media element or resume() on an AudioContext in JavaScript. A user can also use media controls to play media content after it has been paused.
--
-- ObjC selector: @- pauseAllMediaPlaybackWithCompletionHandler:@
pauseAllMediaPlaybackWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
pauseAllMediaPlaybackWithCompletionHandler wkWebView completionHandler =
  sendMessage wkWebView pauseAllMediaPlaybackWithCompletionHandlerSelector completionHandler

-- | @- pauseAllMediaPlayback:@
pauseAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
pauseAllMediaPlayback wkWebView completionHandler =
  sendMessage wkWebView pauseAllMediaPlaybackSelector completionHandler

-- | Suspends or resumes all media playback in WKWebView.
--
-- @suspended@ — Whether media playback should be suspended or resumed.
--
-- If suspended is true, this pauses media playback and blocks all attempts by the page or the user to resume until setAllMediaPlaybackSuspended is called again with suspended set to false. Media playback should always be suspended and resumed in pairs.
--
-- ObjC selector: @- setAllMediaPlaybackSuspended:completionHandler:@
setAllMediaPlaybackSuspended_completionHandler :: IsWKWebView wkWebView => wkWebView -> Bool -> Ptr () -> IO ()
setAllMediaPlaybackSuspended_completionHandler wkWebView suspended completionHandler =
  sendMessage wkWebView setAllMediaPlaybackSuspended_completionHandlerSelector suspended completionHandler

-- | @- resumeAllMediaPlayback:@
resumeAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
resumeAllMediaPlayback wkWebView completionHandler =
  sendMessage wkWebView resumeAllMediaPlaybackSelector completionHandler

-- | @- suspendAllMediaPlayback:@
suspendAllMediaPlayback :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
suspendAllMediaPlayback wkWebView completionHandler =
  sendMessage wkWebView suspendAllMediaPlaybackSelector completionHandler

-- | Get the current media playback state of a WKWebView.
--
-- @completionHandler@ — A block to invoke with the return value of the function call.
--
-- If media playback exists, WKMediaPlaybackState will be one of three values: WKMediaPlaybackPaused, WKMediaPlaybackSuspended, or WKMediaPlaybackPlaying. If no media playback exists in the current WKWebView, WKMediaPlaybackState will equal WKMediaPlaybackStateNone.
--
-- ObjC selector: @- requestMediaPlaybackStateWithCompletionHandler:@
requestMediaPlaybackStateWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
requestMediaPlaybackStateWithCompletionHandler wkWebView completionHandler =
  sendMessage wkWebView requestMediaPlaybackStateWithCompletionHandlerSelector completionHandler

-- | @- requestMediaPlaybackState:@
requestMediaPlaybackState :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
requestMediaPlaybackState wkWebView completionHandler =
  sendMessage wkWebView requestMediaPlaybackStateSelector completionHandler

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
setCameraCaptureState_completionHandler wkWebView state completionHandler =
  sendMessage wkWebView setCameraCaptureState_completionHandlerSelector state completionHandler

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
setMicrophoneCaptureState_completionHandler wkWebView state completionHandler =
  sendMessage wkWebView setMicrophoneCaptureState_completionHandlerSelector state completionHandler

-- | @- takeSnapshotWithConfiguration:completionHandler:@
takeSnapshotWithConfiguration_completionHandler :: (IsWKWebView wkWebView, IsWKSnapshotConfiguration snapshotConfiguration) => wkWebView -> snapshotConfiguration -> Ptr () -> IO ()
takeSnapshotWithConfiguration_completionHandler wkWebView snapshotConfiguration completionHandler =
  sendMessage wkWebView takeSnapshotWithConfiguration_completionHandlerSelector (toWKSnapshotConfiguration snapshotConfiguration) completionHandler

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
createPDFWithConfiguration_completionHandler wkWebView pdfConfiguration completionHandler =
  sendMessage wkWebView createPDFWithConfiguration_completionHandlerSelector (toWKPDFConfiguration pdfConfiguration) completionHandler

-- | @- createWebArchiveDataWithCompletionHandler:@
createWebArchiveDataWithCompletionHandler :: IsWKWebView wkWebView => wkWebView -> Ptr () -> IO ()
createWebArchiveDataWithCompletionHandler wkWebView completionHandler =
  sendMessage wkWebView createWebArchiveDataWithCompletionHandlerSelector completionHandler

-- | @- findString:withConfiguration:completionHandler:@
findString_withConfiguration_completionHandler :: (IsWKWebView wkWebView, IsNSString string, IsWKFindConfiguration configuration) => wkWebView -> string -> configuration -> Ptr () -> IO ()
findString_withConfiguration_completionHandler wkWebView string configuration completionHandler =
  sendMessage wkWebView findString_withConfiguration_completionHandlerSelector (toNSString string) (toWKFindConfiguration configuration) completionHandler

-- | @+ handlesURLScheme:@
handlesURLScheme :: IsNSString urlScheme => urlScheme -> IO Bool
handlesURLScheme urlScheme =
  do
    cls' <- getRequiredClass "WKWebView"
    sendClassMessage cls' handlesURLSchemeSelector (toNSString urlScheme)

-- | @- startDownloadUsingRequest:completionHandler:@
startDownloadUsingRequest_completionHandler :: (IsWKWebView wkWebView, IsNSURLRequest request) => wkWebView -> request -> Ptr () -> IO ()
startDownloadUsingRequest_completionHandler wkWebView request completionHandler =
  sendMessage wkWebView startDownloadUsingRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | @- resumeDownloadFromResumeData:completionHandler:@
resumeDownloadFromResumeData_completionHandler :: (IsWKWebView wkWebView, IsNSData resumeData) => wkWebView -> resumeData -> Ptr () -> IO ()
resumeDownloadFromResumeData_completionHandler wkWebView resumeData completionHandler =
  sendMessage wkWebView resumeDownloadFromResumeData_completionHandlerSelector (toNSData resumeData) completionHandler

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
loadSimulatedRequest_response_responseData wkWebView request response data_ =
  sendMessage wkWebView loadSimulatedRequest_response_responseDataSelector (toNSURLRequest request) (toNSURLResponse response) (toNSData data_)

-- | @- loadSimulatedRequest:withResponse:responseData:@
loadSimulatedRequest_withResponse_responseData :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSURLResponse response, IsNSData data_) => wkWebView -> request -> response -> data_ -> IO (Id WKNavigation)
loadSimulatedRequest_withResponse_responseData wkWebView request response data_ =
  sendMessage wkWebView loadSimulatedRequest_withResponse_responseDataSelector (toNSURLRequest request) (toNSURLResponse response) (toNSData data_)

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
loadFileRequest_allowingReadAccessToURL wkWebView request readAccessURL =
  sendMessage wkWebView loadFileRequest_allowingReadAccessToURLSelector (toNSURLRequest request) (toNSURL readAccessURL)

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
loadSimulatedRequest_responseHTMLString wkWebView request string =
  sendMessage wkWebView loadSimulatedRequest_responseHTMLStringSelector (toNSURLRequest request) (toNSString string)

-- | @- loadSimulatedRequest:withResponseHTMLString:@
loadSimulatedRequest_withResponseHTMLString :: (IsWKWebView wkWebView, IsNSURLRequest request, IsNSString string) => wkWebView -> request -> string -> IO (Id WKNavigation)
loadSimulatedRequest_withResponseHTMLString wkWebView request string =
  sendMessage wkWebView loadSimulatedRequest_withResponseHTMLStringSelector (toNSURLRequest request) (toNSString string)

-- | @- printOperationWithPrintInfo:@
printOperationWithPrintInfo :: (IsWKWebView wkWebView, IsNSPrintInfo printInfo) => wkWebView -> printInfo -> IO (Id NSPrintOperation)
printOperationWithPrintInfo wkWebView printInfo =
  sendMessage wkWebView printOperationWithPrintInfoSelector (toNSPrintInfo printInfo)

-- | @- setMinimumViewportInset:maximumViewportInset:@
setMinimumViewportInset_maximumViewportInset :: IsWKWebView wkWebView => wkWebView -> NSEdgeInsets -> NSEdgeInsets -> IO ()
setMinimumViewportInset_maximumViewportInset wkWebView minimumViewportInset maximumViewportInset =
  sendMessage wkWebView setMinimumViewportInset_maximumViewportInsetSelector minimumViewportInset maximumViewportInset

-- | @- fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandler :: IsWKWebView wkWebView => wkWebView -> WKWebViewDataType -> Ptr () -> IO ()
fetchDataOfTypes_completionHandler wkWebView dataTypes completionHandler =
  sendMessage wkWebView fetchDataOfTypes_completionHandlerSelector dataTypes completionHandler

-- | @- restoreData:completionHandler:@
restoreData_completionHandler :: (IsWKWebView wkWebView, IsNSData data_) => wkWebView -> data_ -> Ptr () -> IO ()
restoreData_completionHandler wkWebView data_ completionHandler =
  sendMessage wkWebView restoreData_completionHandlerSelector (toNSData data_) completionHandler

-- | A copy of the configuration with which the web view was initialized.
--
-- ObjC selector: @- configuration@
configuration :: IsWKWebView wkWebView => wkWebView -> IO (Id WKWebViewConfiguration)
configuration wkWebView =
  sendMessage wkWebView configurationSelector

-- | The web view's navigation delegate.
--
-- ObjC selector: @- navigationDelegate@
navigationDelegate :: IsWKWebView wkWebView => wkWebView -> IO RawId
navigationDelegate wkWebView =
  sendMessage wkWebView navigationDelegateSelector

-- | The web view's navigation delegate.
--
-- ObjC selector: @- setNavigationDelegate:@
setNavigationDelegate :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setNavigationDelegate wkWebView value =
  sendMessage wkWebView setNavigationDelegateSelector value

-- | The web view's user interface delegate.
--
-- ObjC selector: @- UIDelegate@
uiDelegate :: IsWKWebView wkWebView => wkWebView -> IO RawId
uiDelegate wkWebView =
  sendMessage wkWebView uiDelegateSelector

-- | The web view's user interface delegate.
--
-- ObjC selector: @- setUIDelegate:@
setUIDelegate :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setUIDelegate wkWebView value =
  sendMessage wkWebView setUIDelegateSelector value

-- | The web view's back-forward list.
--
-- ObjC selector: @- backForwardList@
backForwardList :: IsWKWebView wkWebView => wkWebView -> IO (Id WKBackForwardList)
backForwardList wkWebView =
  sendMessage wkWebView backForwardListSelector

-- | The page title.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- title@
title :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
title wkWebView =
  sendMessage wkWebView titleSelector

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
url wkWebView =
  sendMessage wkWebView urlSelector

-- | A Boolean value indicating whether the view is currently loading content.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- loading@
loading :: IsWKWebView wkWebView => wkWebView -> IO Bool
loading wkWebView =
  sendMessage wkWebView loadingSelector

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
estimatedProgress wkWebView =
  sendMessage wkWebView estimatedProgressSelector

-- | A Boolean value indicating whether all resources on the page have been loaded over securely encrypted connections.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- hasOnlySecureContent@
hasOnlySecureContent :: IsWKWebView wkWebView => wkWebView -> IO Bool
hasOnlySecureContent wkWebView =
  sendMessage wkWebView hasOnlySecureContentSelector

-- | A SecTrustRef for the currently committed navigation.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant  for this property.
--
-- ObjC selector: @- serverTrust@
serverTrust :: IsWKWebView wkWebView => wkWebView -> IO (Ptr ())
serverTrust wkWebView =
  sendMessage wkWebView serverTrustSelector

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
canGoBack wkWebView =
  sendMessage wkWebView canGoBackSelector

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
canGoForward wkWebView =
  sendMessage wkWebView canGoForwardSelector

-- | The state of camera capture on a web page.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- cameraCaptureState@
cameraCaptureState :: IsWKWebView wkWebView => wkWebView -> IO WKMediaCaptureState
cameraCaptureState wkWebView =
  sendMessage wkWebView cameraCaptureStateSelector

-- | The state of microphone capture on a web page.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- microphoneCaptureState@
microphoneCaptureState :: IsWKWebView wkWebView => wkWebView -> IO WKMediaCaptureState
microphoneCaptureState wkWebView =
  sendMessage wkWebView microphoneCaptureStateSelector

-- | A Boolean value indicating whether horizontal swipe gestures will trigger back-forward list navigations.
--
-- The default value is NO.
--
-- ObjC selector: @- allowsBackForwardNavigationGestures@
allowsBackForwardNavigationGestures :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsBackForwardNavigationGestures wkWebView =
  sendMessage wkWebView allowsBackForwardNavigationGesturesSelector

-- | A Boolean value indicating whether horizontal swipe gestures will trigger back-forward list navigations.
--
-- The default value is NO.
--
-- ObjC selector: @- setAllowsBackForwardNavigationGestures:@
setAllowsBackForwardNavigationGestures :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsBackForwardNavigationGestures wkWebView value =
  sendMessage wkWebView setAllowsBackForwardNavigationGesturesSelector value

-- | The custom user agent string or nil if no custom user agent string has been set.
--
-- ObjC selector: @- customUserAgent@
customUserAgent :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
customUserAgent wkWebView =
  sendMessage wkWebView customUserAgentSelector

-- | The custom user agent string or nil if no custom user agent string has been set.
--
-- ObjC selector: @- setCustomUserAgent:@
setCustomUserAgent :: (IsWKWebView wkWebView, IsNSString value) => wkWebView -> value -> IO ()
setCustomUserAgent wkWebView value =
  sendMessage wkWebView setCustomUserAgentSelector (toNSString value)

-- | A Boolean value indicating whether link preview is allowed for any links inside this WKWebView.
--
-- The default value is YES on Mac and iOS.
--
-- ObjC selector: @- allowsLinkPreview@
allowsLinkPreview :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsLinkPreview wkWebView =
  sendMessage wkWebView allowsLinkPreviewSelector

-- | A Boolean value indicating whether link preview is allowed for any links inside this WKWebView.
--
-- The default value is YES on Mac and iOS.
--
-- ObjC selector: @- setAllowsLinkPreview:@
setAllowsLinkPreview :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsLinkPreview wkWebView value =
  sendMessage wkWebView setAllowsLinkPreviewSelector value

-- | @- allowsMagnification@
allowsMagnification :: IsWKWebView wkWebView => wkWebView -> IO Bool
allowsMagnification wkWebView =
  sendMessage wkWebView allowsMagnificationSelector

-- | @- setAllowsMagnification:@
setAllowsMagnification :: IsWKWebView wkWebView => wkWebView -> Bool -> IO ()
setAllowsMagnification wkWebView value =
  sendMessage wkWebView setAllowsMagnificationSelector value

-- | @- magnification@
magnification :: IsWKWebView wkWebView => wkWebView -> IO CDouble
magnification wkWebView =
  sendMessage wkWebView magnificationSelector

-- | @- setMagnification:@
setMagnification :: IsWKWebView wkWebView => wkWebView -> CDouble -> IO ()
setMagnification wkWebView value =
  sendMessage wkWebView setMagnificationSelector value

-- | @- pageZoom@
pageZoom :: IsWKWebView wkWebView => wkWebView -> IO CDouble
pageZoom wkWebView =
  sendMessage wkWebView pageZoomSelector

-- | @- setPageZoom:@
setPageZoom :: IsWKWebView wkWebView => wkWebView -> CDouble -> IO ()
setPageZoom wkWebView value =
  sendMessage wkWebView setPageZoomSelector value

-- | @- mediaType@
mediaType :: IsWKWebView wkWebView => wkWebView -> IO (Id NSString)
mediaType wkWebView =
  sendMessage wkWebView mediaTypeSelector

-- | @- setMediaType:@
setMediaType :: (IsWKWebView wkWebView, IsNSString value) => wkWebView -> value -> IO ()
setMediaType wkWebView value =
  sendMessage wkWebView setMediaTypeSelector (toNSString value)

-- | @- interactionState@
interactionState :: IsWKWebView wkWebView => wkWebView -> IO RawId
interactionState wkWebView =
  sendMessage wkWebView interactionStateSelector

-- | @- setInteractionState:@
setInteractionState :: IsWKWebView wkWebView => wkWebView -> RawId -> IO ()
setInteractionState wkWebView value =
  sendMessage wkWebView setInteractionStateSelector value

-- | A Boolean value indicating whether Screen Time blocking has occurred.
--
-- ObjC selector: @- isBlockedByScreenTime@
isBlockedByScreenTime :: IsWKWebView wkWebView => wkWebView -> IO Bool
isBlockedByScreenTime wkWebView =
  sendMessage wkWebView isBlockedByScreenTimeSelector

-- | @- themeColor@
themeColor :: IsWKWebView wkWebView => wkWebView -> IO (Id NSColor)
themeColor wkWebView =
  sendMessage wkWebView themeColorSelector

-- | @- underPageBackgroundColor@
underPageBackgroundColor :: IsWKWebView wkWebView => wkWebView -> IO (Id NSColor)
underPageBackgroundColor wkWebView =
  sendMessage wkWebView underPageBackgroundColorSelector

-- | @- setUnderPageBackgroundColor:@
setUnderPageBackgroundColor :: (IsWKWebView wkWebView, IsNSColor value) => wkWebView -> value -> IO ()
setUnderPageBackgroundColor wkWebView value =
  sendMessage wkWebView setUnderPageBackgroundColorSelector (toNSColor value)

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
fullscreenState wkWebView =
  sendMessage wkWebView fullscreenStateSelector

-- | @- minimumViewportInset@
minimumViewportInset :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
minimumViewportInset wkWebView =
  sendMessage wkWebView minimumViewportInsetSelector

-- | @- maximumViewportInset@
maximumViewportInset :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
maximumViewportInset wkWebView =
  sendMessage wkWebView maximumViewportInsetSelector

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
inspectable wkWebView =
  sendMessage wkWebView inspectableSelector

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
setInspectable wkWebView value =
  sendMessage wkWebView setInspectableSelector value

-- | A Boolean value indicating whether Writing Tools is active for the view.
--
-- WKWebView
--
-- is key-value observing (KVO) compliant for this property.
--
-- ObjC selector: @- writingToolsActive@
writingToolsActive :: IsWKWebView wkWebView => wkWebView -> IO Bool
writingToolsActive wkWebView =
  sendMessage wkWebView writingToolsActiveSelector

-- | @- obscuredContentInsets@
obscuredContentInsets :: IsWKWebView wkWebView => wkWebView -> IO NSEdgeInsets
obscuredContentInsets wkWebView =
  sendMessage wkWebView obscuredContentInsetsSelector

-- | @- setObscuredContentInsets:@
setObscuredContentInsets :: IsWKWebView wkWebView => wkWebView -> NSEdgeInsets -> IO ()
setObscuredContentInsets wkWebView value =
  sendMessage wkWebView setObscuredContentInsetsSelector value

-- | @- certificateChain@
certificateChain :: IsWKWebView wkWebView => wkWebView -> IO (Id NSArray)
certificateChain wkWebView =
  sendMessage wkWebView certificateChainSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id WKWebView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @loadRequest:@
loadRequestSelector :: Selector '[Id NSURLRequest] (Id WKNavigation)
loadRequestSelector = mkSelector "loadRequest:"

-- | @Selector@ for @loadFileURL:allowingReadAccessToURL:@
loadFileURL_allowingReadAccessToURLSelector :: Selector '[Id NSURL, Id NSURL] (Id WKNavigation)
loadFileURL_allowingReadAccessToURLSelector = mkSelector "loadFileURL:allowingReadAccessToURL:"

-- | @Selector@ for @loadHTMLString:baseURL:@
loadHTMLString_baseURLSelector :: Selector '[Id NSString, Id NSURL] (Id WKNavigation)
loadHTMLString_baseURLSelector = mkSelector "loadHTMLString:baseURL:"

-- | @Selector@ for @loadData:MIMEType:characterEncodingName:baseURL:@
loadData_MIMEType_characterEncodingName_baseURLSelector :: Selector '[Id NSData, Id NSString, Id NSString, Id NSURL] (Id WKNavigation)
loadData_MIMEType_characterEncodingName_baseURLSelector = mkSelector "loadData:MIMEType:characterEncodingName:baseURL:"

-- | @Selector@ for @goToBackForwardListItem:@
goToBackForwardListItemSelector :: Selector '[Id WKBackForwardListItem] (Id WKNavigation)
goToBackForwardListItemSelector = mkSelector "goToBackForwardListItem:"

-- | @Selector@ for @goBack@
goBackSelector :: Selector '[] (Id WKNavigation)
goBackSelector = mkSelector "goBack"

-- | @Selector@ for @goForward@
goForwardSelector :: Selector '[] (Id WKNavigation)
goForwardSelector = mkSelector "goForward"

-- | @Selector@ for @reload@
reloadSelector :: Selector '[] (Id WKNavigation)
reloadSelector = mkSelector "reload"

-- | @Selector@ for @reloadFromOrigin@
reloadFromOriginSelector :: Selector '[] (Id WKNavigation)
reloadFromOriginSelector = mkSelector "reloadFromOrigin"

-- | @Selector@ for @stopLoading@
stopLoadingSelector :: Selector '[] ()
stopLoadingSelector = mkSelector "stopLoading"

-- | @Selector@ for @evaluateJavaScript:completionHandler:@
evaluateJavaScript_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
evaluateJavaScript_completionHandlerSelector = mkSelector "evaluateJavaScript:completionHandler:"

-- | @Selector@ for @evaluateJavaScript:inFrame:inContentWorld:completionHandler:@
evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector :: Selector '[Id NSString, Id WKFrameInfo, Id WKContentWorld, Ptr ()] ()
evaluateJavaScript_inFrame_inContentWorld_completionHandlerSelector = mkSelector "evaluateJavaScript:inFrame:inContentWorld:completionHandler:"

-- | @Selector@ for @callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:@
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector :: Selector '[Id NSString, Id NSDictionary, Id WKFrameInfo, Id WKContentWorld, Ptr ()] ()
callAsyncJavaScript_arguments_inFrame_inContentWorld_completionHandlerSelector = mkSelector "callAsyncJavaScript:arguments:inFrame:inContentWorld:completionHandler:"

-- | @Selector@ for @closeAllMediaPresentationsWithCompletionHandler:@
closeAllMediaPresentationsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
closeAllMediaPresentationsWithCompletionHandlerSelector = mkSelector "closeAllMediaPresentationsWithCompletionHandler:"

-- | @Selector@ for @closeAllMediaPresentations@
closeAllMediaPresentationsSelector :: Selector '[] ()
closeAllMediaPresentationsSelector = mkSelector "closeAllMediaPresentations"

-- | @Selector@ for @pauseAllMediaPlaybackWithCompletionHandler:@
pauseAllMediaPlaybackWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
pauseAllMediaPlaybackWithCompletionHandlerSelector = mkSelector "pauseAllMediaPlaybackWithCompletionHandler:"

-- | @Selector@ for @pauseAllMediaPlayback:@
pauseAllMediaPlaybackSelector :: Selector '[Ptr ()] ()
pauseAllMediaPlaybackSelector = mkSelector "pauseAllMediaPlayback:"

-- | @Selector@ for @setAllMediaPlaybackSuspended:completionHandler:@
setAllMediaPlaybackSuspended_completionHandlerSelector :: Selector '[Bool, Ptr ()] ()
setAllMediaPlaybackSuspended_completionHandlerSelector = mkSelector "setAllMediaPlaybackSuspended:completionHandler:"

-- | @Selector@ for @resumeAllMediaPlayback:@
resumeAllMediaPlaybackSelector :: Selector '[Ptr ()] ()
resumeAllMediaPlaybackSelector = mkSelector "resumeAllMediaPlayback:"

-- | @Selector@ for @suspendAllMediaPlayback:@
suspendAllMediaPlaybackSelector :: Selector '[Ptr ()] ()
suspendAllMediaPlaybackSelector = mkSelector "suspendAllMediaPlayback:"

-- | @Selector@ for @requestMediaPlaybackStateWithCompletionHandler:@
requestMediaPlaybackStateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestMediaPlaybackStateWithCompletionHandlerSelector = mkSelector "requestMediaPlaybackStateWithCompletionHandler:"

-- | @Selector@ for @requestMediaPlaybackState:@
requestMediaPlaybackStateSelector :: Selector '[Ptr ()] ()
requestMediaPlaybackStateSelector = mkSelector "requestMediaPlaybackState:"

-- | @Selector@ for @setCameraCaptureState:completionHandler:@
setCameraCaptureState_completionHandlerSelector :: Selector '[WKMediaCaptureState, Ptr ()] ()
setCameraCaptureState_completionHandlerSelector = mkSelector "setCameraCaptureState:completionHandler:"

-- | @Selector@ for @setMicrophoneCaptureState:completionHandler:@
setMicrophoneCaptureState_completionHandlerSelector :: Selector '[WKMediaCaptureState, Ptr ()] ()
setMicrophoneCaptureState_completionHandlerSelector = mkSelector "setMicrophoneCaptureState:completionHandler:"

-- | @Selector@ for @takeSnapshotWithConfiguration:completionHandler:@
takeSnapshotWithConfiguration_completionHandlerSelector :: Selector '[Id WKSnapshotConfiguration, Ptr ()] ()
takeSnapshotWithConfiguration_completionHandlerSelector = mkSelector "takeSnapshotWithConfiguration:completionHandler:"

-- | @Selector@ for @createPDFWithConfiguration:completionHandler:@
createPDFWithConfiguration_completionHandlerSelector :: Selector '[Id WKPDFConfiguration, Ptr ()] ()
createPDFWithConfiguration_completionHandlerSelector = mkSelector "createPDFWithConfiguration:completionHandler:"

-- | @Selector@ for @createWebArchiveDataWithCompletionHandler:@
createWebArchiveDataWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
createWebArchiveDataWithCompletionHandlerSelector = mkSelector "createWebArchiveDataWithCompletionHandler:"

-- | @Selector@ for @findString:withConfiguration:completionHandler:@
findString_withConfiguration_completionHandlerSelector :: Selector '[Id NSString, Id WKFindConfiguration, Ptr ()] ()
findString_withConfiguration_completionHandlerSelector = mkSelector "findString:withConfiguration:completionHandler:"

-- | @Selector@ for @handlesURLScheme:@
handlesURLSchemeSelector :: Selector '[Id NSString] Bool
handlesURLSchemeSelector = mkSelector "handlesURLScheme:"

-- | @Selector@ for @startDownloadUsingRequest:completionHandler:@
startDownloadUsingRequest_completionHandlerSelector :: Selector '[Id NSURLRequest, Ptr ()] ()
startDownloadUsingRequest_completionHandlerSelector = mkSelector "startDownloadUsingRequest:completionHandler:"

-- | @Selector@ for @resumeDownloadFromResumeData:completionHandler:@
resumeDownloadFromResumeData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
resumeDownloadFromResumeData_completionHandlerSelector = mkSelector "resumeDownloadFromResumeData:completionHandler:"

-- | @Selector@ for @loadSimulatedRequest:response:responseData:@
loadSimulatedRequest_response_responseDataSelector :: Selector '[Id NSURLRequest, Id NSURLResponse, Id NSData] (Id WKNavigation)
loadSimulatedRequest_response_responseDataSelector = mkSelector "loadSimulatedRequest:response:responseData:"

-- | @Selector@ for @loadSimulatedRequest:withResponse:responseData:@
loadSimulatedRequest_withResponse_responseDataSelector :: Selector '[Id NSURLRequest, Id NSURLResponse, Id NSData] (Id WKNavigation)
loadSimulatedRequest_withResponse_responseDataSelector = mkSelector "loadSimulatedRequest:withResponse:responseData:"

-- | @Selector@ for @loadFileRequest:allowingReadAccessToURL:@
loadFileRequest_allowingReadAccessToURLSelector :: Selector '[Id NSURLRequest, Id NSURL] (Id WKNavigation)
loadFileRequest_allowingReadAccessToURLSelector = mkSelector "loadFileRequest:allowingReadAccessToURL:"

-- | @Selector@ for @loadSimulatedRequest:responseHTMLString:@
loadSimulatedRequest_responseHTMLStringSelector :: Selector '[Id NSURLRequest, Id NSString] (Id WKNavigation)
loadSimulatedRequest_responseHTMLStringSelector = mkSelector "loadSimulatedRequest:responseHTMLString:"

-- | @Selector@ for @loadSimulatedRequest:withResponseHTMLString:@
loadSimulatedRequest_withResponseHTMLStringSelector :: Selector '[Id NSURLRequest, Id NSString] (Id WKNavigation)
loadSimulatedRequest_withResponseHTMLStringSelector = mkSelector "loadSimulatedRequest:withResponseHTMLString:"

-- | @Selector@ for @printOperationWithPrintInfo:@
printOperationWithPrintInfoSelector :: Selector '[Id NSPrintInfo] (Id NSPrintOperation)
printOperationWithPrintInfoSelector = mkSelector "printOperationWithPrintInfo:"

-- | @Selector@ for @setMinimumViewportInset:maximumViewportInset:@
setMinimumViewportInset_maximumViewportInsetSelector :: Selector '[NSEdgeInsets, NSEdgeInsets] ()
setMinimumViewportInset_maximumViewportInsetSelector = mkSelector "setMinimumViewportInset:maximumViewportInset:"

-- | @Selector@ for @fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandlerSelector :: Selector '[WKWebViewDataType, Ptr ()] ()
fetchDataOfTypes_completionHandlerSelector = mkSelector "fetchDataOfTypes:completionHandler:"

-- | @Selector@ for @restoreData:completionHandler:@
restoreData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
restoreData_completionHandlerSelector = mkSelector "restoreData:completionHandler:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id WKWebViewConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @navigationDelegate@
navigationDelegateSelector :: Selector '[] RawId
navigationDelegateSelector = mkSelector "navigationDelegate"

-- | @Selector@ for @setNavigationDelegate:@
setNavigationDelegateSelector :: Selector '[RawId] ()
setNavigationDelegateSelector = mkSelector "setNavigationDelegate:"

-- | @Selector@ for @UIDelegate@
uiDelegateSelector :: Selector '[] RawId
uiDelegateSelector = mkSelector "UIDelegate"

-- | @Selector@ for @setUIDelegate:@
setUIDelegateSelector :: Selector '[RawId] ()
setUIDelegateSelector = mkSelector "setUIDelegate:"

-- | @Selector@ for @backForwardList@
backForwardListSelector :: Selector '[] (Id WKBackForwardList)
backForwardListSelector = mkSelector "backForwardList"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

-- | @Selector@ for @estimatedProgress@
estimatedProgressSelector :: Selector '[] CDouble
estimatedProgressSelector = mkSelector "estimatedProgress"

-- | @Selector@ for @hasOnlySecureContent@
hasOnlySecureContentSelector :: Selector '[] Bool
hasOnlySecureContentSelector = mkSelector "hasOnlySecureContent"

-- | @Selector@ for @serverTrust@
serverTrustSelector :: Selector '[] (Ptr ())
serverTrustSelector = mkSelector "serverTrust"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector '[] Bool
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector '[] Bool
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @cameraCaptureState@
cameraCaptureStateSelector :: Selector '[] WKMediaCaptureState
cameraCaptureStateSelector = mkSelector "cameraCaptureState"

-- | @Selector@ for @microphoneCaptureState@
microphoneCaptureStateSelector :: Selector '[] WKMediaCaptureState
microphoneCaptureStateSelector = mkSelector "microphoneCaptureState"

-- | @Selector@ for @allowsBackForwardNavigationGestures@
allowsBackForwardNavigationGesturesSelector :: Selector '[] Bool
allowsBackForwardNavigationGesturesSelector = mkSelector "allowsBackForwardNavigationGestures"

-- | @Selector@ for @setAllowsBackForwardNavigationGestures:@
setAllowsBackForwardNavigationGesturesSelector :: Selector '[Bool] ()
setAllowsBackForwardNavigationGesturesSelector = mkSelector "setAllowsBackForwardNavigationGestures:"

-- | @Selector@ for @customUserAgent@
customUserAgentSelector :: Selector '[] (Id NSString)
customUserAgentSelector = mkSelector "customUserAgent"

-- | @Selector@ for @setCustomUserAgent:@
setCustomUserAgentSelector :: Selector '[Id NSString] ()
setCustomUserAgentSelector = mkSelector "setCustomUserAgent:"

-- | @Selector@ for @allowsLinkPreview@
allowsLinkPreviewSelector :: Selector '[] Bool
allowsLinkPreviewSelector = mkSelector "allowsLinkPreview"

-- | @Selector@ for @setAllowsLinkPreview:@
setAllowsLinkPreviewSelector :: Selector '[Bool] ()
setAllowsLinkPreviewSelector = mkSelector "setAllowsLinkPreview:"

-- | @Selector@ for @allowsMagnification@
allowsMagnificationSelector :: Selector '[] Bool
allowsMagnificationSelector = mkSelector "allowsMagnification"

-- | @Selector@ for @setAllowsMagnification:@
setAllowsMagnificationSelector :: Selector '[Bool] ()
setAllowsMagnificationSelector = mkSelector "setAllowsMagnification:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector '[] CDouble
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @setMagnification:@
setMagnificationSelector :: Selector '[CDouble] ()
setMagnificationSelector = mkSelector "setMagnification:"

-- | @Selector@ for @pageZoom@
pageZoomSelector :: Selector '[] CDouble
pageZoomSelector = mkSelector "pageZoom"

-- | @Selector@ for @setPageZoom:@
setPageZoomSelector :: Selector '[CDouble] ()
setPageZoomSelector = mkSelector "setPageZoom:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @setMediaType:@
setMediaTypeSelector :: Selector '[Id NSString] ()
setMediaTypeSelector = mkSelector "setMediaType:"

-- | @Selector@ for @interactionState@
interactionStateSelector :: Selector '[] RawId
interactionStateSelector = mkSelector "interactionState"

-- | @Selector@ for @setInteractionState:@
setInteractionStateSelector :: Selector '[RawId] ()
setInteractionStateSelector = mkSelector "setInteractionState:"

-- | @Selector@ for @isBlockedByScreenTime@
isBlockedByScreenTimeSelector :: Selector '[] Bool
isBlockedByScreenTimeSelector = mkSelector "isBlockedByScreenTime"

-- | @Selector@ for @themeColor@
themeColorSelector :: Selector '[] (Id NSColor)
themeColorSelector = mkSelector "themeColor"

-- | @Selector@ for @underPageBackgroundColor@
underPageBackgroundColorSelector :: Selector '[] (Id NSColor)
underPageBackgroundColorSelector = mkSelector "underPageBackgroundColor"

-- | @Selector@ for @setUnderPageBackgroundColor:@
setUnderPageBackgroundColorSelector :: Selector '[Id NSColor] ()
setUnderPageBackgroundColorSelector = mkSelector "setUnderPageBackgroundColor:"

-- | @Selector@ for @fullscreenState@
fullscreenStateSelector :: Selector '[] WKFullscreenState
fullscreenStateSelector = mkSelector "fullscreenState"

-- | @Selector@ for @minimumViewportInset@
minimumViewportInsetSelector :: Selector '[] NSEdgeInsets
minimumViewportInsetSelector = mkSelector "minimumViewportInset"

-- | @Selector@ for @maximumViewportInset@
maximumViewportInsetSelector :: Selector '[] NSEdgeInsets
maximumViewportInsetSelector = mkSelector "maximumViewportInset"

-- | @Selector@ for @inspectable@
inspectableSelector :: Selector '[] Bool
inspectableSelector = mkSelector "inspectable"

-- | @Selector@ for @setInspectable:@
setInspectableSelector :: Selector '[Bool] ()
setInspectableSelector = mkSelector "setInspectable:"

-- | @Selector@ for @writingToolsActive@
writingToolsActiveSelector :: Selector '[] Bool
writingToolsActiveSelector = mkSelector "writingToolsActive"

-- | @Selector@ for @obscuredContentInsets@
obscuredContentInsetsSelector :: Selector '[] NSEdgeInsets
obscuredContentInsetsSelector = mkSelector "obscuredContentInsets"

-- | @Selector@ for @setObscuredContentInsets:@
setObscuredContentInsetsSelector :: Selector '[NSEdgeInsets] ()
setObscuredContentInsetsSelector = mkSelector "setObscuredContentInsets:"

-- | @Selector@ for @certificateChain@
certificateChainSelector :: Selector '[] (Id NSArray)
certificateChainSelector = mkSelector "certificateChain"

