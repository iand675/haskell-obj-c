{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WebView@.
module ObjC.WebKit.WebView
  ( WebView
  , IsWebView(..)
  , canShowMIMEType
  , canShowMIMETypeAsHTML
  , mimeTypesShownAsHTML
  , setMIMETypesShownAsHTML
  , urlFromPasteboard
  , urlTitleFromPasteboard
  , registerURLSchemeAsLocal
  , initWithFrame_frameName_groupName
  , close
  , setMaintainsBackForwardList
  , goBack
  , goForward
  , goToBackForwardItem
  , userAgentForURL
  , stringByEvaluatingJavaScriptFromString
  , searchFor_direction_caseSensitive_wrap
  , registerViewClass_representationClass_forMIMEType
  , elementAtPoint
  , writeSelectionWithPasteboardTypes_toPasteboard
  , pasteboardTypesForElement
  , writeElement_withPasteboardTypes_toPasteboard
  , moveDragCaretToPoint
  , removeDragCaret
  , copy
  , cut
  , paste
  , copyFont
  , pasteFont
  , delete
  , pasteAsPlainText
  , pasteAsRichText
  , changeFont
  , changeAttributes
  , changeDocumentBackgroundColor
  , changeColor
  , alignCenter
  , alignJustified
  , alignLeft
  , alignRight
  , checkSpelling
  , showGuessPanel
  , performFindPanelAction
  , startSpeaking
  , stopSpeaking
  , moveToBeginningOfSentence
  , moveToBeginningOfSentenceAndModifySelection
  , moveToEndOfSentence
  , moveToEndOfSentenceAndModifySelection
  , selectSentence
  , overWrite
  , replaceSelectionWithNode
  , replaceSelectionWithText
  , replaceSelectionWithMarkupString
  , replaceSelectionWithArchive
  , deleteSelection
  , applyStyle
  , editableDOMRangeForPoint
  , setSelectedDOMRange_affinity
  , styleDeclarationWithText
  , computedStyleForElement_pseudoElement
  , takeStringURLFrom
  , stopLoading
  , reload
  , reloadFromOrigin
  , makeTextLarger
  , makeTextSmaller
  , makeTextStandardSize
  , toggleContinuousSpellChecking
  , toggleSmartInsertDelete
  , shouldCloseWithWindow
  , setShouldCloseWithWindow
  , mainFrame
  , selectedFrame
  , backForwardList
  , textSizeMultiplier
  , setTextSizeMultiplier
  , applicationNameForUserAgent
  , setApplicationNameForUserAgent
  , customUserAgent
  , setCustomUserAgent
  , supportsTextEncoding
  , customTextEncodingName
  , setCustomTextEncodingName
  , mediaStyle
  , setMediaStyle
  , windowScriptObject
  , preferences
  , setPreferences
  , preferencesIdentifier
  , setPreferencesIdentifier
  , hostWindow
  , setHostWindow
  , groupName
  , setGroupName
  , estimatedProgress
  , loading
  , pasteboardTypesForSelection
  , drawsBackground
  , setDrawsBackground
  , shouldUpdateWhileOffscreen
  , setShouldUpdateWhileOffscreen
  , mainFrameURL
  , setMainFrameURL
  , mainFrameDocument
  , mainFrameTitle
  , mainFrameIcon
  , selectedDOMRange
  , selectionAffinity
  , maintainsInactiveSelection
  , editable
  , setEditable
  , typingStyle
  , setTypingStyle
  , smartInsertDeleteEnabled
  , setSmartInsertDeleteEnabled
  , continuousSpellCheckingEnabled
  , setContinuousSpellCheckingEnabled
  , spellCheckerDocumentTag
  , undoManager
  , canGoBack
  , canGoForward
  , canMakeTextLarger
  , canMakeTextSmaller
  , canMakeTextStandardSize
  , canShowMIMETypeSelector
  , canShowMIMETypeAsHTMLSelector
  , mimeTypesShownAsHTMLSelector
  , setMIMETypesShownAsHTMLSelector
  , urlFromPasteboardSelector
  , urlTitleFromPasteboardSelector
  , registerURLSchemeAsLocalSelector
  , initWithFrame_frameName_groupNameSelector
  , closeSelector
  , setMaintainsBackForwardListSelector
  , goBackSelector
  , goForwardSelector
  , goToBackForwardItemSelector
  , userAgentForURLSelector
  , stringByEvaluatingJavaScriptFromStringSelector
  , searchFor_direction_caseSensitive_wrapSelector
  , registerViewClass_representationClass_forMIMETypeSelector
  , elementAtPointSelector
  , writeSelectionWithPasteboardTypes_toPasteboardSelector
  , pasteboardTypesForElementSelector
  , writeElement_withPasteboardTypes_toPasteboardSelector
  , moveDragCaretToPointSelector
  , removeDragCaretSelector
  , copySelector
  , cutSelector
  , pasteSelector
  , copyFontSelector
  , pasteFontSelector
  , deleteSelector
  , pasteAsPlainTextSelector
  , pasteAsRichTextSelector
  , changeFontSelector
  , changeAttributesSelector
  , changeDocumentBackgroundColorSelector
  , changeColorSelector
  , alignCenterSelector
  , alignJustifiedSelector
  , alignLeftSelector
  , alignRightSelector
  , checkSpellingSelector
  , showGuessPanelSelector
  , performFindPanelActionSelector
  , startSpeakingSelector
  , stopSpeakingSelector
  , moveToBeginningOfSentenceSelector
  , moveToBeginningOfSentenceAndModifySelectionSelector
  , moveToEndOfSentenceSelector
  , moveToEndOfSentenceAndModifySelectionSelector
  , selectSentenceSelector
  , overWriteSelector
  , replaceSelectionWithNodeSelector
  , replaceSelectionWithTextSelector
  , replaceSelectionWithMarkupStringSelector
  , replaceSelectionWithArchiveSelector
  , deleteSelectionSelector
  , applyStyleSelector
  , editableDOMRangeForPointSelector
  , setSelectedDOMRange_affinitySelector
  , styleDeclarationWithTextSelector
  , computedStyleForElement_pseudoElementSelector
  , takeStringURLFromSelector
  , stopLoadingSelector
  , reloadSelector
  , reloadFromOriginSelector
  , makeTextLargerSelector
  , makeTextSmallerSelector
  , makeTextStandardSizeSelector
  , toggleContinuousSpellCheckingSelector
  , toggleSmartInsertDeleteSelector
  , shouldCloseWithWindowSelector
  , setShouldCloseWithWindowSelector
  , mainFrameSelector
  , selectedFrameSelector
  , backForwardListSelector
  , textSizeMultiplierSelector
  , setTextSizeMultiplierSelector
  , applicationNameForUserAgentSelector
  , setApplicationNameForUserAgentSelector
  , customUserAgentSelector
  , setCustomUserAgentSelector
  , supportsTextEncodingSelector
  , customTextEncodingNameSelector
  , setCustomTextEncodingNameSelector
  , mediaStyleSelector
  , setMediaStyleSelector
  , windowScriptObjectSelector
  , preferencesSelector
  , setPreferencesSelector
  , preferencesIdentifierSelector
  , setPreferencesIdentifierSelector
  , hostWindowSelector
  , setHostWindowSelector
  , groupNameSelector
  , setGroupNameSelector
  , estimatedProgressSelector
  , loadingSelector
  , pasteboardTypesForSelectionSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , shouldUpdateWhileOffscreenSelector
  , setShouldUpdateWhileOffscreenSelector
  , mainFrameURLSelector
  , setMainFrameURLSelector
  , mainFrameDocumentSelector
  , mainFrameTitleSelector
  , mainFrameIconSelector
  , selectedDOMRangeSelector
  , selectionAffinitySelector
  , maintainsInactiveSelectionSelector
  , editableSelector
  , setEditableSelector
  , typingStyleSelector
  , setTypingStyleSelector
  , smartInsertDeleteEnabledSelector
  , setSmartInsertDeleteEnabledSelector
  , continuousSpellCheckingEnabledSelector
  , setContinuousSpellCheckingEnabledSelector
  , spellCheckerDocumentTagSelector
  , undoManagerSelector
  , canGoBackSelector
  , canGoForwardSelector
  , canMakeTextLargerSelector
  , canMakeTextSmallerSelector
  , canMakeTextStandardSizeSelector

  -- * Enum types
  , NSSelectionAffinity(NSSelectionAffinity)
  , pattern NSSelectionAffinityUpstream
  , pattern NSSelectionAffinityDownstream

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | canShowMIMEType:
--
-- Checks if the WebKit can show content of a certain MIME type.
--
-- @MIMEType@ — The MIME type to check.
--
-- Returns: YES if the WebKit can show content with MIMEtype.
--
-- ObjC selector: @+ canShowMIMEType:@
canShowMIMEType :: IsNSString mimeType => mimeType -> IO Bool
canShowMIMEType mimeType =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr mimeType $ \raw_mimeType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canShowMIMEType:") retCULong [argPtr (castPtr raw_mimeType :: Ptr ())]

-- | canShowMIMETypeAsHTML:
--
-- Checks if the MIME type is a type that the WebKit will interpret as HTML.
--
-- @MIMEType@ — The MIME type to check.
--
-- Returns: YES if the MIMEtype in an HTML type.
--
-- ObjC selector: @+ canShowMIMETypeAsHTML:@
canShowMIMETypeAsHTML :: IsNSString mimeType => mimeType -> IO Bool
canShowMIMETypeAsHTML mimeType =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr mimeType $ \raw_mimeType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canShowMIMETypeAsHTML:") retCULong [argPtr (castPtr raw_mimeType :: Ptr ())]

-- | MIMETypesShownAsHTML
--
-- Returns: Returns an array of NSStrings that describe the MIME types    WebKit will attempt to render as HTML.
--
-- ObjC selector: @+ MIMETypesShownAsHTML@
mimeTypesShownAsHTML :: IO (Id NSArray)
mimeTypesShownAsHTML  =
  do
    cls' <- getRequiredClass "WebView"
    sendClassMsg cls' (mkSelector "MIMETypesShownAsHTML") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setMIMETypesShownAsHTML:
--
-- Sets the array of NSString MIME types that WebKit will    attempt to render as HTML.  Typically you will retrieve the built-in    array using MIMETypesShownAsHTML and add additional MIME types to that    array.
--
-- ObjC selector: @+ setMIMETypesShownAsHTML:@
setMIMETypesShownAsHTML :: IsNSArray mimeTypes => mimeTypes -> IO ()
setMIMETypesShownAsHTML mimeTypes =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr mimeTypes $ \raw_mimeTypes ->
      sendClassMsg cls' (mkSelector "setMIMETypesShownAsHTML:") retVoid [argPtr (castPtr raw_mimeTypes :: Ptr ())]

-- | URLFromPasteboard:
--
-- Returns a URL from a pasteboard
--
-- @pasteboard@ — The pasteboard with a URL
--
-- Returns: A URL if the pasteboard has one. Nil if it does not.
--
-- This method differs than NSURL's URLFromPasteboard method in that it tries multiple pasteboard types    including NSURLPboardType to find a URL on the pasteboard.
--
-- ObjC selector: @+ URLFromPasteboard:@
urlFromPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSURL)
urlFromPasteboard pasteboard =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      sendClassMsg cls' (mkSelector "URLFromPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= retainedObject . castPtr

-- | URLTitleFromPasteboard:
--
-- Returns a URL title from a pasteboard
--
-- @pasteboard@ — The pasteboard with a URL title
--
-- Returns: A URL title if the pasteboard has one. Nil if it does not.
--
-- This method returns a title that refers a URL on the pasteboard. An example of this is the link label    which is the text inside the anchor tag.
--
-- ObjC selector: @+ URLTitleFromPasteboard:@
urlTitleFromPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO (Id NSString)
urlTitleFromPasteboard pasteboard =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      sendClassMsg cls' (mkSelector "URLTitleFromPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= retainedObject . castPtr

-- | registerURLSchemeAsLocal:
--
-- Adds the scheme to the list of schemes to be treated as local.
--
-- @scheme@ — The scheme to register
--
-- ObjC selector: @+ registerURLSchemeAsLocal:@
registerURLSchemeAsLocal :: IsNSString scheme => scheme -> IO ()
registerURLSchemeAsLocal scheme =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr scheme $ \raw_scheme ->
      sendClassMsg cls' (mkSelector "registerURLSchemeAsLocal:") retVoid [argPtr (castPtr raw_scheme :: Ptr ())]

-- | initWithFrame:frameName:groupName:
--
-- The designated initializer for WebView.
--
-- Initialize a WebView with the supplied parameters. This method will     create a main WebFrame with the view. Passing a top level frame name is useful if you    handle a targetted frame navigation that would normally open a window in some other     way that still ends up creating a new WebView.
--
-- @frame@ — The frame used to create the view.
--
-- @frameName@ — The name to use for the top level frame. May be nil.
--
-- @groupName@ — The name of the webView set to which this webView will be added.  May be nil.
--
-- Returns: Returns an initialized WebView.
--
-- ObjC selector: @- initWithFrame:frameName:groupName:@
initWithFrame_frameName_groupName :: (IsWebView webView, IsNSString frameName, IsNSString groupName) => webView -> NSRect -> frameName -> groupName -> IO (Id WebView)
initWithFrame_frameName_groupName webView  frame frameName groupName =
withObjCPtr frameName $ \raw_frameName ->
  withObjCPtr groupName $ \raw_groupName ->
      sendMsg webView (mkSelector "initWithFrame:frameName:groupName:") (retPtr retVoid) [argNSRect frame, argPtr (castPtr raw_frameName :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ())] >>= ownedObject . castPtr

-- | close
--
-- Closes the receiver, unloading its web page and canceling any pending loads.    Once the receiver has closed, it will no longer respond to requests or fire delegate methods.    (However, the -close method itself may fire delegate methods.)
--
-- A garbage collected application is required to call close when the receiver is no longer needed.    The close method will be called automatically when the window or hostWindow closes and shouldCloseWithWindow returns YES.    A non-garbage collected application can still call close, providing a convenient way to prevent receiver    from doing any more loading and firing any future delegate methods.
--
-- ObjC selector: @- close@
close :: IsWebView webView => webView -> IO ()
close webView  =
  sendMsg webView (mkSelector "close") retVoid []

-- | setMaintainsBackForwardList:
--
-- Enable or disable the use of a backforward list for this webView.
--
-- @flag@ — Turns use of the back forward list on or off
--
-- ObjC selector: @- setMaintainsBackForwardList:@
setMaintainsBackForwardList :: IsWebView webView => webView -> Bool -> IO ()
setMaintainsBackForwardList webView  flag =
  sendMsg webView (mkSelector "setMaintainsBackForwardList:") retVoid [argCULong (if flag then 1 else 0)]

-- | goBack
--
-- Go back to the previous URL in the backforward list.
--
-- Returns: YES if able to go back in the backforward list, NO otherwise.
--
-- ObjC selector: @- goBack@
goBack :: IsWebView webView => webView -> IO Bool
goBack webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "goBack") retCULong []

-- | goForward
--
-- Go forward to the next URL in the backforward list.
--
-- Returns: YES if able to go forward in the backforward list, NO otherwise.
--
-- ObjC selector: @- goForward@
goForward :: IsWebView webView => webView -> IO Bool
goForward webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "goForward") retCULong []

-- | goToBackForwardItem:
--
-- Go back or forward to an item in the backforward list.
--
-- Returns: YES if able to go to the item, NO otherwise.
--
-- ObjC selector: @- goToBackForwardItem:@
goToBackForwardItem :: (IsWebView webView, IsWebHistoryItem item) => webView -> item -> IO Bool
goToBackForwardItem webView  item =
withObjCPtr item $ \raw_item ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "goToBackForwardItem:") retCULong [argPtr (castPtr raw_item :: Ptr ())]

-- | userAgentForURL:
--
-- Get the appropriate user-agent string for a particular URL.
--
-- @URL@ — The URL.
--
-- Returns: The user-agent string for the supplied URL.
--
-- ObjC selector: @- userAgentForURL:@
userAgentForURL :: (IsWebView webView, IsNSURL url) => webView -> url -> IO (Id NSString)
userAgentForURL webView  url =
withObjCPtr url $ \raw_url ->
    sendMsg webView (mkSelector "userAgentForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | stringByEvaluatingJavaScriptFromString:
--
-- @script@ — The text of the JavaScript.
--
-- Returns: The result of the script, converted to a string, or nil for failure.
--
-- ObjC selector: @- stringByEvaluatingJavaScriptFromString:@
stringByEvaluatingJavaScriptFromString :: (IsWebView webView, IsNSString script) => webView -> script -> IO (Id NSString)
stringByEvaluatingJavaScriptFromString webView  script =
withObjCPtr script $ \raw_script ->
    sendMsg webView (mkSelector "stringByEvaluatingJavaScriptFromString:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ())] >>= retainedObject . castPtr

-- | searchFor:direction:caseSensitive:
--
-- Searches a document view for a string and highlights the string if it is found.    Starts the search from the current selection.  Will search across all frames.
--
-- @string@ — The string to search for.
--
-- @forward@ — YES to search forward, NO to seach backwards.
--
-- @caseFlag@ — YES to for case-sensitive search, NO for case-insensitive search.
--
-- Returns: YES if found, NO if not found.
--
-- ObjC selector: @- searchFor:direction:caseSensitive:wrap:@
searchFor_direction_caseSensitive_wrap :: (IsWebView webView, IsNSString string) => webView -> string -> Bool -> Bool -> Bool -> IO Bool
searchFor_direction_caseSensitive_wrap webView  string forward caseFlag wrapFlag =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "searchFor:direction:caseSensitive:wrap:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argCULong (if forward then 1 else 0), argCULong (if caseFlag then 1 else 0), argCULong (if wrapFlag then 1 else 0)]

-- | registerViewClass:representationClass:forMIMEType:
--
-- Register classes that implement WebDocumentView and WebDocumentRepresentation respectively.    A document class may register for a primary MIME type by excluding    a subtype, i.e. "video/" will match the document class with    all video types.  More specific matching takes precedence    over general matching.
--
-- @viewClass@ — The WebDocumentView class to use to render data for a given MIME type.
--
-- @representationClass@ — The WebDocumentRepresentation class to use to represent data of the given MIME type.
--
-- @MIMEType@ — The MIME type to represent with an object of the given class.
--
-- ObjC selector: @+ registerViewClass:representationClass:forMIMEType:@
registerViewClass_representationClass_forMIMEType :: IsNSString mimeType => Class -> Class -> mimeType -> IO ()
registerViewClass_representationClass_forMIMEType viewClass representationClass mimeType =
  do
    cls' <- getRequiredClass "WebView"
    withObjCPtr mimeType $ \raw_mimeType ->
      sendClassMsg cls' (mkSelector "registerViewClass:representationClass:forMIMEType:") retVoid [argPtr (unClass viewClass), argPtr (unClass representationClass), argPtr (castPtr raw_mimeType :: Ptr ())]

-- | elementAtPoint:
--
-- @point@ — A point in the coordinates of the WebView
--
-- Returns: An element dictionary describing the point
--
-- ObjC selector: @- elementAtPoint:@
elementAtPoint :: IsWebView webView => webView -> NSPoint -> IO (Id NSDictionary)
elementAtPoint webView  point =
  sendMsg webView (mkSelector "elementAtPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | writeSelectionWithPasteboardTypes:toPasteboard:
--
-- Writes the current selection to the pasteboard
--
-- @types@ — The types that WebView will write to the pasteboard
--
-- @pasteboard@ — The pasteboard to write to
--
-- ObjC selector: @- writeSelectionWithPasteboardTypes:toPasteboard:@
writeSelectionWithPasteboardTypes_toPasteboard :: (IsWebView webView, IsNSArray types, IsNSPasteboard pasteboard) => webView -> types -> pasteboard -> IO ()
writeSelectionWithPasteboardTypes_toPasteboard webView  types pasteboard =
withObjCPtr types $ \raw_types ->
  withObjCPtr pasteboard $ \raw_pasteboard ->
      sendMsg webView (mkSelector "writeSelectionWithPasteboardTypes:toPasteboard:") retVoid [argPtr (castPtr raw_types :: Ptr ()), argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | pasteboardTypesForElement:
--
-- Returns the pasteboard types that WebView can use for an element
--
-- @element@ — The element
--
-- ObjC selector: @- pasteboardTypesForElement:@
pasteboardTypesForElement :: (IsWebView webView, IsNSDictionary element) => webView -> element -> IO (Id NSArray)
pasteboardTypesForElement webView  element =
withObjCPtr element $ \raw_element ->
    sendMsg webView (mkSelector "pasteboardTypesForElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ())] >>= retainedObject . castPtr

-- | writeElement:withPasteboardTypes:toPasteboard:
--
-- Writes an element to the pasteboard
--
-- @element@ — The element to write to the pasteboard
--
-- @types@ — The types that WebView will write to the pasteboard
--
-- @pasteboard@ — The pasteboard to write to
--
-- ObjC selector: @- writeElement:withPasteboardTypes:toPasteboard:@
writeElement_withPasteboardTypes_toPasteboard :: (IsWebView webView, IsNSDictionary element, IsNSArray types, IsNSPasteboard pasteboard) => webView -> element -> types -> pasteboard -> IO ()
writeElement_withPasteboardTypes_toPasteboard webView  element types pasteboard =
withObjCPtr element $ \raw_element ->
  withObjCPtr types $ \raw_types ->
    withObjCPtr pasteboard $ \raw_pasteboard ->
        sendMsg webView (mkSelector "writeElement:withPasteboardTypes:toPasteboard:") retVoid [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_types :: Ptr ()), argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | moveDragCaretToPoint:
--
-- @point@ — A point in the coordinates of the WebView
--
-- This method moves the caret that shows where something being dragged will be dropped. It may cause the WebView to scroll    to make the new position of the drag caret visible.
--
-- ObjC selector: @- moveDragCaretToPoint:@
moveDragCaretToPoint :: IsWebView webView => webView -> NSPoint -> IO ()
moveDragCaretToPoint webView  point =
  sendMsg webView (mkSelector "moveDragCaretToPoint:") retVoid [argNSPoint point]

-- | removeDragCaret
--
-- Removes the drag caret from the WebView
--
-- ObjC selector: @- removeDragCaret@
removeDragCaret :: IsWebView webView => webView -> IO ()
removeDragCaret webView  =
  sendMsg webView (mkSelector "removeDragCaret") retVoid []

-- | @- copy:@
copy :: IsWebView webView => webView -> RawId -> IO ()
copy webView  sender =
  sendMsg webView (mkSelector "copy:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- cut:@
cut :: IsWebView webView => webView -> RawId -> IO ()
cut webView  sender =
  sendMsg webView (mkSelector "cut:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- paste:@
paste :: IsWebView webView => webView -> RawId -> IO ()
paste webView  sender =
  sendMsg webView (mkSelector "paste:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- copyFont:@
copyFont :: IsWebView webView => webView -> RawId -> IO ()
copyFont webView  sender =
  sendMsg webView (mkSelector "copyFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteFont:@
pasteFont :: IsWebView webView => webView -> RawId -> IO ()
pasteFont webView  sender =
  sendMsg webView (mkSelector "pasteFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- delete:@
delete :: IsWebView webView => webView -> RawId -> IO ()
delete webView  sender =
  sendMsg webView (mkSelector "delete:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteAsPlainText:@
pasteAsPlainText :: IsWebView webView => webView -> RawId -> IO ()
pasteAsPlainText webView  sender =
  sendMsg webView (mkSelector "pasteAsPlainText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteAsRichText:@
pasteAsRichText :: IsWebView webView => webView -> RawId -> IO ()
pasteAsRichText webView  sender =
  sendMsg webView (mkSelector "pasteAsRichText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeFont:@
changeFont :: IsWebView webView => webView -> RawId -> IO ()
changeFont webView  sender =
  sendMsg webView (mkSelector "changeFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeAttributes:@
changeAttributes :: IsWebView webView => webView -> RawId -> IO ()
changeAttributes webView  sender =
  sendMsg webView (mkSelector "changeAttributes:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeDocumentBackgroundColor:@
changeDocumentBackgroundColor :: IsWebView webView => webView -> RawId -> IO ()
changeDocumentBackgroundColor webView  sender =
  sendMsg webView (mkSelector "changeDocumentBackgroundColor:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeColor:@
changeColor :: IsWebView webView => webView -> RawId -> IO ()
changeColor webView  sender =
  sendMsg webView (mkSelector "changeColor:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignCenter:@
alignCenter :: IsWebView webView => webView -> RawId -> IO ()
alignCenter webView  sender =
  sendMsg webView (mkSelector "alignCenter:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignJustified:@
alignJustified :: IsWebView webView => webView -> RawId -> IO ()
alignJustified webView  sender =
  sendMsg webView (mkSelector "alignJustified:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignLeft:@
alignLeft :: IsWebView webView => webView -> RawId -> IO ()
alignLeft webView  sender =
  sendMsg webView (mkSelector "alignLeft:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- alignRight:@
alignRight :: IsWebView webView => webView -> RawId -> IO ()
alignRight webView  sender =
  sendMsg webView (mkSelector "alignRight:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkSpelling:@
checkSpelling :: IsWebView webView => webView -> RawId -> IO ()
checkSpelling webView  sender =
  sendMsg webView (mkSelector "checkSpelling:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- showGuessPanel:@
showGuessPanel :: IsWebView webView => webView -> RawId -> IO ()
showGuessPanel webView  sender =
  sendMsg webView (mkSelector "showGuessPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- performFindPanelAction:@
performFindPanelAction :: IsWebView webView => webView -> RawId -> IO ()
performFindPanelAction webView  sender =
  sendMsg webView (mkSelector "performFindPanelAction:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- startSpeaking:@
startSpeaking :: IsWebView webView => webView -> RawId -> IO ()
startSpeaking webView  sender =
  sendMsg webView (mkSelector "startSpeaking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopSpeaking:@
stopSpeaking :: IsWebView webView => webView -> RawId -> IO ()
stopSpeaking webView  sender =
  sendMsg webView (mkSelector "stopSpeaking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveToBeginningOfSentence:@
moveToBeginningOfSentence :: IsWebView webView => webView -> RawId -> IO ()
moveToBeginningOfSentence webView  sender =
  sendMsg webView (mkSelector "moveToBeginningOfSentence:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveToBeginningOfSentenceAndModifySelection:@
moveToBeginningOfSentenceAndModifySelection :: IsWebView webView => webView -> RawId -> IO ()
moveToBeginningOfSentenceAndModifySelection webView  sender =
  sendMsg webView (mkSelector "moveToBeginningOfSentenceAndModifySelection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveToEndOfSentence:@
moveToEndOfSentence :: IsWebView webView => webView -> RawId -> IO ()
moveToEndOfSentence webView  sender =
  sendMsg webView (mkSelector "moveToEndOfSentence:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveToEndOfSentenceAndModifySelection:@
moveToEndOfSentenceAndModifySelection :: IsWebView webView => webView -> RawId -> IO ()
moveToEndOfSentenceAndModifySelection webView  sender =
  sendMsg webView (mkSelector "moveToEndOfSentenceAndModifySelection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- selectSentence:@
selectSentence :: IsWebView webView => webView -> RawId -> IO ()
selectSentence webView  sender =
  sendMsg webView (mkSelector "selectSentence:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- overWrite:@
overWrite :: IsWebView webView => webView -> RawId -> IO ()
overWrite webView  sender =
  sendMsg webView (mkSelector "overWrite:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- replaceSelectionWithNode:@
replaceSelectionWithNode :: (IsWebView webView, IsDOMNode node) => webView -> node -> IO ()
replaceSelectionWithNode webView  node =
withObjCPtr node $ \raw_node ->
    sendMsg webView (mkSelector "replaceSelectionWithNode:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | @- replaceSelectionWithText:@
replaceSelectionWithText :: (IsWebView webView, IsNSString text) => webView -> text -> IO ()
replaceSelectionWithText webView  text =
withObjCPtr text $ \raw_text ->
    sendMsg webView (mkSelector "replaceSelectionWithText:") retVoid [argPtr (castPtr raw_text :: Ptr ())]

-- | @- replaceSelectionWithMarkupString:@
replaceSelectionWithMarkupString :: (IsWebView webView, IsNSString markupString) => webView -> markupString -> IO ()
replaceSelectionWithMarkupString webView  markupString =
withObjCPtr markupString $ \raw_markupString ->
    sendMsg webView (mkSelector "replaceSelectionWithMarkupString:") retVoid [argPtr (castPtr raw_markupString :: Ptr ())]

-- | @- replaceSelectionWithArchive:@
replaceSelectionWithArchive :: (IsWebView webView, IsWebArchive archive) => webView -> archive -> IO ()
replaceSelectionWithArchive webView  archive =
withObjCPtr archive $ \raw_archive ->
    sendMsg webView (mkSelector "replaceSelectionWithArchive:") retVoid [argPtr (castPtr raw_archive :: Ptr ())]

-- | @- deleteSelection@
deleteSelection :: IsWebView webView => webView -> IO ()
deleteSelection webView  =
  sendMsg webView (mkSelector "deleteSelection") retVoid []

-- | @- applyStyle:@
applyStyle :: (IsWebView webView, IsDOMCSSStyleDeclaration style) => webView -> style -> IO ()
applyStyle webView  style =
withObjCPtr style $ \raw_style ->
    sendMsg webView (mkSelector "applyStyle:") retVoid [argPtr (castPtr raw_style :: Ptr ())]

-- | @- editableDOMRangeForPoint:@
editableDOMRangeForPoint :: IsWebView webView => webView -> NSPoint -> IO (Id DOMRange)
editableDOMRangeForPoint webView  point =
  sendMsg webView (mkSelector "editableDOMRangeForPoint:") (retPtr retVoid) [argNSPoint point] >>= retainedObject . castPtr

-- | @- setSelectedDOMRange:affinity:@
setSelectedDOMRange_affinity :: (IsWebView webView, IsDOMRange range) => webView -> range -> NSSelectionAffinity -> IO ()
setSelectedDOMRange_affinity webView  range selectionAffinity =
withObjCPtr range $ \raw_range ->
    sendMsg webView (mkSelector "setSelectedDOMRange:affinity:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argCULong (coerce selectionAffinity)]

-- | @- styleDeclarationWithText:@
styleDeclarationWithText :: (IsWebView webView, IsNSString text) => webView -> text -> IO (Id DOMCSSStyleDeclaration)
styleDeclarationWithText webView  text =
withObjCPtr text $ \raw_text ->
    sendMsg webView (mkSelector "styleDeclarationWithText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= retainedObject . castPtr

-- | @- computedStyleForElement:pseudoElement:@
computedStyleForElement_pseudoElement :: (IsWebView webView, IsDOMElement element, IsNSString pseudoElement) => webView -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
computedStyleForElement_pseudoElement webView  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg webView (mkSelector "computedStyleForElement:pseudoElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- takeStringURLFrom:@
takeStringURLFrom :: IsWebView webView => webView -> RawId -> IO ()
takeStringURLFrom webView  sender =
  sendMsg webView (mkSelector "takeStringURLFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopLoading:@
stopLoading :: IsWebView webView => webView -> RawId -> IO ()
stopLoading webView  sender =
  sendMsg webView (mkSelector "stopLoading:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- reload:@
reload :: IsWebView webView => webView -> RawId -> IO ()
reload webView  sender =
  sendMsg webView (mkSelector "reload:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- reloadFromOrigin:@
reloadFromOrigin :: IsWebView webView => webView -> RawId -> IO ()
reloadFromOrigin webView  sender =
  sendMsg webView (mkSelector "reloadFromOrigin:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- makeTextLarger:@
makeTextLarger :: IsWebView webView => webView -> RawId -> IO ()
makeTextLarger webView  sender =
  sendMsg webView (mkSelector "makeTextLarger:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- makeTextSmaller:@
makeTextSmaller :: IsWebView webView => webView -> RawId -> IO ()
makeTextSmaller webView  sender =
  sendMsg webView (mkSelector "makeTextSmaller:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- makeTextStandardSize:@
makeTextStandardSize :: IsWebView webView => webView -> RawId -> IO ()
makeTextStandardSize webView  sender =
  sendMsg webView (mkSelector "makeTextStandardSize:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleContinuousSpellChecking:@
toggleContinuousSpellChecking :: IsWebView webView => webView -> RawId -> IO ()
toggleContinuousSpellChecking webView  sender =
  sendMsg webView (mkSelector "toggleContinuousSpellChecking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleSmartInsertDelete:@
toggleSmartInsertDelete :: IsWebView webView => webView -> RawId -> IO ()
toggleSmartInsertDelete webView  sender =
  sendMsg webView (mkSelector "toggleSmartInsertDelete:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | shouldCloseWithWindow
--
-- Whether the receiver closes when either it's window or hostWindow closes.
--
-- Defaults to YES in garbage collected applications, otherwise NO to maintain backwards compatibility.
--
-- ObjC selector: @- shouldCloseWithWindow@
shouldCloseWithWindow :: IsWebView webView => webView -> IO Bool
shouldCloseWithWindow webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "shouldCloseWithWindow") retCULong []

-- | shouldCloseWithWindow
--
-- Whether the receiver closes when either it's window or hostWindow closes.
--
-- Defaults to YES in garbage collected applications, otherwise NO to maintain backwards compatibility.
--
-- ObjC selector: @- setShouldCloseWithWindow:@
setShouldCloseWithWindow :: IsWebView webView => webView -> Bool -> IO ()
setShouldCloseWithWindow webView  value =
  sendMsg webView (mkSelector "setShouldCloseWithWindow:") retVoid [argCULong (if value then 1 else 0)]

-- | mainFrame
--
-- The top level frame.
--
-- Note that even documents that are not framesets will have a mainFrame.
--
-- ObjC selector: @- mainFrame@
mainFrame :: IsWebView webView => webView -> IO (Id WebFrame)
mainFrame webView  =
  sendMsg webView (mkSelector "mainFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | selectedFrame
--
-- The frame that has the active selection.
--
-- Returns the frame that contains the first responder, if any. Otherwise returns the    frame that contains a non-zero-length selection, if any. Returns nil if no frame meets these criteria.
--
-- ObjC selector: @- selectedFrame@
selectedFrame :: IsWebView webView => webView -> IO (Id WebFrame)
selectedFrame webView  =
  sendMsg webView (mkSelector "selectedFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | backForwardList
--
-- The backforward list for this WebView.
--
-- ObjC selector: @- backForwardList@
backForwardList :: IsWebView webView => webView -> IO (Id WebBackForwardList)
backForwardList webView  =
  sendMsg webView (mkSelector "backForwardList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textSizeMultiplier
--
-- The text size multipler.
--
-- ObjC selector: @- textSizeMultiplier@
textSizeMultiplier :: IsWebView webView => webView -> IO CFloat
textSizeMultiplier webView  =
  sendMsg webView (mkSelector "textSizeMultiplier") retCFloat []

-- | textSizeMultiplier
--
-- The text size multipler.
--
-- ObjC selector: @- setTextSizeMultiplier:@
setTextSizeMultiplier :: IsWebView webView => webView -> CFloat -> IO ()
setTextSizeMultiplier webView  value =
  sendMsg webView (mkSelector "setTextSizeMultiplier:") retVoid [argCFloat (fromIntegral value)]

-- | applicationNameForUserAgent
--
-- The name of the application as used in the user-agent string.
--
-- ObjC selector: @- applicationNameForUserAgent@
applicationNameForUserAgent :: IsWebView webView => webView -> IO (Id NSString)
applicationNameForUserAgent webView  =
  sendMsg webView (mkSelector "applicationNameForUserAgent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationNameForUserAgent
--
-- The name of the application as used in the user-agent string.
--
-- ObjC selector: @- setApplicationNameForUserAgent:@
setApplicationNameForUserAgent :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setApplicationNameForUserAgent webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setApplicationNameForUserAgent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | customUserAgent
--
-- The custom user-agent string or nil if no custom user-agent string has been set.
--
-- Setting this means that the webView should use this user-agent string instead of constructing a user-agent string for each URL. Setting it to nil causes the webView to construct the user-agent string for each URL for best results rendering web pages
--
-- ObjC selector: @- customUserAgent@
customUserAgent :: IsWebView webView => webView -> IO (Id NSString)
customUserAgent webView  =
  sendMsg webView (mkSelector "customUserAgent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | customUserAgent
--
-- The custom user-agent string or nil if no custom user-agent string has been set.
--
-- Setting this means that the webView should use this user-agent string instead of constructing a user-agent string for each URL. Setting it to nil causes the webView to construct the user-agent string for each URL for best results rendering web pages
--
-- ObjC selector: @- setCustomUserAgent:@
setCustomUserAgent :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setCustomUserAgent webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setCustomUserAgent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | supportsTextEncoding
--
-- If the document view of the current web page can support different text encodings.
--
-- ObjC selector: @- supportsTextEncoding@
supportsTextEncoding :: IsWebView webView => webView -> IO Bool
supportsTextEncoding webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "supportsTextEncoding") retCULong []

-- | customTextEncodingName
--
-- The custom text encoding name or nil if no custom text encoding name has been set.
--
-- Make the page display with a different text encoding; stops any load in progress.    The text encoding passed in overrides the normal text encoding smarts including    what's specified in a web page's header or HTTP response.    The text encoding automatically goes back to the default when the top level frame    changes to a new location.    Setting the text encoding name to nil makes the webView use default encoding rules.
--
-- ObjC selector: @- customTextEncodingName@
customTextEncodingName :: IsWebView webView => webView -> IO (Id NSString)
customTextEncodingName webView  =
  sendMsg webView (mkSelector "customTextEncodingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | customTextEncodingName
--
-- The custom text encoding name or nil if no custom text encoding name has been set.
--
-- Make the page display with a different text encoding; stops any load in progress.    The text encoding passed in overrides the normal text encoding smarts including    what's specified in a web page's header or HTTP response.    The text encoding automatically goes back to the default when the top level frame    changes to a new location.    Setting the text encoding name to nil makes the webView use default encoding rules.
--
-- ObjC selector: @- setCustomTextEncodingName:@
setCustomTextEncodingName :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setCustomTextEncodingName webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setCustomTextEncodingName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | mediaStyle
--
-- The media style for the WebView.
--
-- The mediaStyle will override the normal value    of the CSS media property. Setting the value to nil will restore the normal value. The value will be nil unless explicitly set.
--
-- ObjC selector: @- mediaStyle@
mediaStyle :: IsWebView webView => webView -> IO (Id NSString)
mediaStyle webView  =
  sendMsg webView (mkSelector "mediaStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaStyle
--
-- The media style for the WebView.
--
-- The mediaStyle will override the normal value    of the CSS media property. Setting the value to nil will restore the normal value. The value will be nil unless explicitly set.
--
-- ObjC selector: @- setMediaStyle:@
setMediaStyle :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setMediaStyle webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setMediaStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | windowScriptObject
--
-- A WebScriptObject that represents the    window object from the script environment.
--
-- ObjC selector: @- windowScriptObject@
windowScriptObject :: IsWebView webView => webView -> IO (Id WebScriptObject)
windowScriptObject webView  =
  sendMsg webView (mkSelector "windowScriptObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferences
--
-- The preferences used by this WebView.
--
-- This method will return [WebPreferences standardPreferences] if no    other instance of WebPreferences has been set.
--
-- ObjC selector: @- preferences@
preferences :: IsWebView webView => webView -> IO (Id WebPreferences)
preferences webView  =
  sendMsg webView (mkSelector "preferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferences
--
-- The preferences used by this WebView.
--
-- This method will return [WebPreferences standardPreferences] if no    other instance of WebPreferences has been set.
--
-- ObjC selector: @- setPreferences:@
setPreferences :: (IsWebView webView, IsWebPreferences value) => webView -> value -> IO ()
setPreferences webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setPreferences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | preferencesIdentifier
--
-- The WebPreferences key prefix.
--
-- If the WebPreferences for this WebView are stored in the user defaults database, this string will be used as a key prefix.
--
-- ObjC selector: @- preferencesIdentifier@
preferencesIdentifier :: IsWebView webView => webView -> IO (Id NSString)
preferencesIdentifier webView  =
  sendMsg webView (mkSelector "preferencesIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferencesIdentifier
--
-- The WebPreferences key prefix.
--
-- If the WebPreferences for this WebView are stored in the user defaults database, this string will be used as a key prefix.
--
-- ObjC selector: @- setPreferencesIdentifier:@
setPreferencesIdentifier :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setPreferencesIdentifier webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setPreferencesIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | hostWindow
--
-- The host window for the web view.
--
-- Parts of WebKit (such as plug-ins and JavaScript) depend on a window to function    properly. Set a host window so these parts continue to function even when the web view is    not in an actual window.
--
-- ObjC selector: @- hostWindow@
hostWindow :: IsWebView webView => webView -> IO (Id NSWindow)
hostWindow webView  =
  sendMsg webView (mkSelector "hostWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hostWindow
--
-- The host window for the web view.
--
-- Parts of WebKit (such as plug-ins and JavaScript) depend on a window to function    properly. Set a host window so these parts continue to function even when the web view is    not in an actual window.
--
-- ObjC selector: @- setHostWindow:@
setHostWindow :: (IsWebView webView, IsNSWindow value) => webView -> value -> IO ()
setHostWindow webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setHostWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | groupName
--
-- The group name for this WebView.
--
-- JavaScript may access named frames within the same group.
--
-- ObjC selector: @- groupName@
groupName :: IsWebView webView => webView -> IO (Id NSString)
groupName webView  =
  sendMsg webView (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | groupName
--
-- The group name for this WebView.
--
-- JavaScript may access named frames within the same group.
--
-- ObjC selector: @- setGroupName:@
setGroupName :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setGroupName webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setGroupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | estimatedProgress
--
-- An estimate of the percent complete for a document load.  This    value will range from 0 to 1.0 and, once a load completes, will remain at 1.0     until a new load starts, at which point it will be reset to 0.  The value is an    estimate based on the total number of bytes expected to be received    for a document, including all it's possible subresources.  For more accurate progress    indication it is recommended that you implement a WebFrameLoadDelegate and a    WebResourceLoadDelegate.
--
-- ObjC selector: @- estimatedProgress@
estimatedProgress :: IsWebView webView => webView -> IO CDouble
estimatedProgress webView  =
  sendMsg webView (mkSelector "estimatedProgress") retCDouble []

-- | loading
--
-- Whether there are any pending loads in this WebView.
--
-- ObjC selector: @- loading@
loading :: IsWebView webView => webView -> IO Bool
loading webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "loading") retCULong []

-- | pasteboardTypesForSelection
--
-- The pasteboard types that the WebView can use for the current selection
--
-- ObjC selector: @- pasteboardTypesForSelection@
pasteboardTypesForSelection :: IsWebView webView => webView -> IO (Id NSArray)
pasteboardTypesForSelection webView  =
  sendMsg webView (mkSelector "pasteboardTypesForSelection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | drawsBackground
--
-- Whether the receiver draws a default white background when the loaded page has no background specified.
--
-- ObjC selector: @- drawsBackground@
drawsBackground :: IsWebView webView => webView -> IO Bool
drawsBackground webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "drawsBackground") retCULong []

-- | drawsBackground
--
-- Whether the receiver draws a default white background when the loaded page has no background specified.
--
-- ObjC selector: @- setDrawsBackground:@
setDrawsBackground :: IsWebView webView => webView -> Bool -> IO ()
setDrawsBackground webView  value =
  sendMsg webView (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | shouldUpdateWhileOffscreen
--
-- Whether the WebView is always updated even when it is not in a window that is currently visible.
--
-- If set to NO, then whenever the web view is not in a visible window, updates to the web page will not necessarily be rendered in the view.    However, when the window is made visible, the view will be updated automatically. Not updating while hidden can improve performance. If set to is YES,    hidden web views are always updated. This is the default.
--
-- ObjC selector: @- shouldUpdateWhileOffscreen@
shouldUpdateWhileOffscreen :: IsWebView webView => webView -> IO Bool
shouldUpdateWhileOffscreen webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "shouldUpdateWhileOffscreen") retCULong []

-- | shouldUpdateWhileOffscreen
--
-- Whether the WebView is always updated even when it is not in a window that is currently visible.
--
-- If set to NO, then whenever the web view is not in a visible window, updates to the web page will not necessarily be rendered in the view.    However, when the window is made visible, the view will be updated automatically. Not updating while hidden can improve performance. If set to is YES,    hidden web views are always updated. This is the default.
--
-- ObjC selector: @- setShouldUpdateWhileOffscreen:@
setShouldUpdateWhileOffscreen :: IsWebView webView => webView -> Bool -> IO ()
setShouldUpdateWhileOffscreen webView  value =
  sendMsg webView (mkSelector "setShouldUpdateWhileOffscreen:") retVoid [argCULong (if value then 1 else 0)]

-- | mainFrameURL
--
-- The main frame's current URL.
--
-- ObjC selector: @- mainFrameURL@
mainFrameURL :: IsWebView webView => webView -> IO (Id NSString)
mainFrameURL webView  =
  sendMsg webView (mkSelector "mainFrameURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mainFrameURL
--
-- The main frame's current URL.
--
-- ObjC selector: @- setMainFrameURL:@
setMainFrameURL :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setMainFrameURL webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setMainFrameURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | mainFrameDocument
--
-- The main frame's DOMDocument.
--
-- ObjC selector: @- mainFrameDocument@
mainFrameDocument :: IsWebView webView => webView -> IO (Id DOMDocument)
mainFrameDocument webView  =
  sendMsg webView (mkSelector "mainFrameDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mainFrameTitle
--
-- The main frame's title if any, otherwise an empty string.
--
-- ObjC selector: @- mainFrameTitle@
mainFrameTitle :: IsWebView webView => webView -> IO (Id NSString)
mainFrameTitle webView  =
  sendMsg webView (mkSelector "mainFrameTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mainFrameIcon
--
-- The site icon for the current page loaded in the mainFrame, or nil.
--
-- ObjC selector: @- mainFrameIcon@
mainFrameIcon :: IsWebView webView => webView -> IO (Id NSImage)
mainFrameIcon webView  =
  sendMsg webView (mkSelector "mainFrameIcon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedDOMRange@
selectedDOMRange :: IsWebView webView => webView -> IO (Id DOMRange)
selectedDOMRange webView  =
  sendMsg webView (mkSelector "selectedDOMRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionAffinity@
selectionAffinity :: IsWebView webView => webView -> IO NSSelectionAffinity
selectionAffinity webView  =
  fmap (coerce :: CULong -> NSSelectionAffinity) $ sendMsg webView (mkSelector "selectionAffinity") retCULong []

-- | @- maintainsInactiveSelection@
maintainsInactiveSelection :: IsWebView webView => webView -> IO Bool
maintainsInactiveSelection webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "maintainsInactiveSelection") retCULong []

-- | @- editable@
editable :: IsWebView webView => webView -> IO Bool
editable webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsWebView webView => webView -> Bool -> IO ()
setEditable webView  value =
  sendMsg webView (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- typingStyle@
typingStyle :: IsWebView webView => webView -> IO (Id DOMCSSStyleDeclaration)
typingStyle webView  =
  sendMsg webView (mkSelector "typingStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTypingStyle:@
setTypingStyle :: (IsWebView webView, IsDOMCSSStyleDeclaration value) => webView -> value -> IO ()
setTypingStyle webView  value =
withObjCPtr value $ \raw_value ->
    sendMsg webView (mkSelector "setTypingStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- smartInsertDeleteEnabled@
smartInsertDeleteEnabled :: IsWebView webView => webView -> IO Bool
smartInsertDeleteEnabled webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "smartInsertDeleteEnabled") retCULong []

-- | @- setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabled :: IsWebView webView => webView -> Bool -> IO ()
setSmartInsertDeleteEnabled webView  value =
  sendMsg webView (mkSelector "setSmartInsertDeleteEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- continuousSpellCheckingEnabled@
continuousSpellCheckingEnabled :: IsWebView webView => webView -> IO Bool
continuousSpellCheckingEnabled webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "continuousSpellCheckingEnabled") retCULong []

-- | @- setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabled :: IsWebView webView => webView -> Bool -> IO ()
setContinuousSpellCheckingEnabled webView  value =
  sendMsg webView (mkSelector "setContinuousSpellCheckingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsWebView webView => webView -> IO CLong
spellCheckerDocumentTag webView  =
  sendMsg webView (mkSelector "spellCheckerDocumentTag") retCLong []

-- | @- undoManager@
undoManager :: IsWebView webView => webView -> IO (Id NSUndoManager)
undoManager webView  =
  sendMsg webView (mkSelector "undoManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- canGoBack@
canGoBack :: IsWebView webView => webView -> IO Bool
canGoBack webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "canGoBack") retCULong []

-- | @- canGoForward@
canGoForward :: IsWebView webView => webView -> IO Bool
canGoForward webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "canGoForward") retCULong []

-- | @- canMakeTextLarger@
canMakeTextLarger :: IsWebView webView => webView -> IO Bool
canMakeTextLarger webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "canMakeTextLarger") retCULong []

-- | @- canMakeTextSmaller@
canMakeTextSmaller :: IsWebView webView => webView -> IO Bool
canMakeTextSmaller webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "canMakeTextSmaller") retCULong []

-- | @- canMakeTextStandardSize@
canMakeTextStandardSize :: IsWebView webView => webView -> IO Bool
canMakeTextStandardSize webView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg webView (mkSelector "canMakeTextStandardSize") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canShowMIMEType:@
canShowMIMETypeSelector :: Selector
canShowMIMETypeSelector = mkSelector "canShowMIMEType:"

-- | @Selector@ for @canShowMIMETypeAsHTML:@
canShowMIMETypeAsHTMLSelector :: Selector
canShowMIMETypeAsHTMLSelector = mkSelector "canShowMIMETypeAsHTML:"

-- | @Selector@ for @MIMETypesShownAsHTML@
mimeTypesShownAsHTMLSelector :: Selector
mimeTypesShownAsHTMLSelector = mkSelector "MIMETypesShownAsHTML"

-- | @Selector@ for @setMIMETypesShownAsHTML:@
setMIMETypesShownAsHTMLSelector :: Selector
setMIMETypesShownAsHTMLSelector = mkSelector "setMIMETypesShownAsHTML:"

-- | @Selector@ for @URLFromPasteboard:@
urlFromPasteboardSelector :: Selector
urlFromPasteboardSelector = mkSelector "URLFromPasteboard:"

-- | @Selector@ for @URLTitleFromPasteboard:@
urlTitleFromPasteboardSelector :: Selector
urlTitleFromPasteboardSelector = mkSelector "URLTitleFromPasteboard:"

-- | @Selector@ for @registerURLSchemeAsLocal:@
registerURLSchemeAsLocalSelector :: Selector
registerURLSchemeAsLocalSelector = mkSelector "registerURLSchemeAsLocal:"

-- | @Selector@ for @initWithFrame:frameName:groupName:@
initWithFrame_frameName_groupNameSelector :: Selector
initWithFrame_frameName_groupNameSelector = mkSelector "initWithFrame:frameName:groupName:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @setMaintainsBackForwardList:@
setMaintainsBackForwardListSelector :: Selector
setMaintainsBackForwardListSelector = mkSelector "setMaintainsBackForwardList:"

-- | @Selector@ for @goBack@
goBackSelector :: Selector
goBackSelector = mkSelector "goBack"

-- | @Selector@ for @goForward@
goForwardSelector :: Selector
goForwardSelector = mkSelector "goForward"

-- | @Selector@ for @goToBackForwardItem:@
goToBackForwardItemSelector :: Selector
goToBackForwardItemSelector = mkSelector "goToBackForwardItem:"

-- | @Selector@ for @userAgentForURL:@
userAgentForURLSelector :: Selector
userAgentForURLSelector = mkSelector "userAgentForURL:"

-- | @Selector@ for @stringByEvaluatingJavaScriptFromString:@
stringByEvaluatingJavaScriptFromStringSelector :: Selector
stringByEvaluatingJavaScriptFromStringSelector = mkSelector "stringByEvaluatingJavaScriptFromString:"

-- | @Selector@ for @searchFor:direction:caseSensitive:wrap:@
searchFor_direction_caseSensitive_wrapSelector :: Selector
searchFor_direction_caseSensitive_wrapSelector = mkSelector "searchFor:direction:caseSensitive:wrap:"

-- | @Selector@ for @registerViewClass:representationClass:forMIMEType:@
registerViewClass_representationClass_forMIMETypeSelector :: Selector
registerViewClass_representationClass_forMIMETypeSelector = mkSelector "registerViewClass:representationClass:forMIMEType:"

-- | @Selector@ for @elementAtPoint:@
elementAtPointSelector :: Selector
elementAtPointSelector = mkSelector "elementAtPoint:"

-- | @Selector@ for @writeSelectionWithPasteboardTypes:toPasteboard:@
writeSelectionWithPasteboardTypes_toPasteboardSelector :: Selector
writeSelectionWithPasteboardTypes_toPasteboardSelector = mkSelector "writeSelectionWithPasteboardTypes:toPasteboard:"

-- | @Selector@ for @pasteboardTypesForElement:@
pasteboardTypesForElementSelector :: Selector
pasteboardTypesForElementSelector = mkSelector "pasteboardTypesForElement:"

-- | @Selector@ for @writeElement:withPasteboardTypes:toPasteboard:@
writeElement_withPasteboardTypes_toPasteboardSelector :: Selector
writeElement_withPasteboardTypes_toPasteboardSelector = mkSelector "writeElement:withPasteboardTypes:toPasteboard:"

-- | @Selector@ for @moveDragCaretToPoint:@
moveDragCaretToPointSelector :: Selector
moveDragCaretToPointSelector = mkSelector "moveDragCaretToPoint:"

-- | @Selector@ for @removeDragCaret@
removeDragCaretSelector :: Selector
removeDragCaretSelector = mkSelector "removeDragCaret"

-- | @Selector@ for @copy:@
copySelector :: Selector
copySelector = mkSelector "copy:"

-- | @Selector@ for @cut:@
cutSelector :: Selector
cutSelector = mkSelector "cut:"

-- | @Selector@ for @paste:@
pasteSelector :: Selector
pasteSelector = mkSelector "paste:"

-- | @Selector@ for @copyFont:@
copyFontSelector :: Selector
copyFontSelector = mkSelector "copyFont:"

-- | @Selector@ for @pasteFont:@
pasteFontSelector :: Selector
pasteFontSelector = mkSelector "pasteFont:"

-- | @Selector@ for @delete:@
deleteSelector :: Selector
deleteSelector = mkSelector "delete:"

-- | @Selector@ for @pasteAsPlainText:@
pasteAsPlainTextSelector :: Selector
pasteAsPlainTextSelector = mkSelector "pasteAsPlainText:"

-- | @Selector@ for @pasteAsRichText:@
pasteAsRichTextSelector :: Selector
pasteAsRichTextSelector = mkSelector "pasteAsRichText:"

-- | @Selector@ for @changeFont:@
changeFontSelector :: Selector
changeFontSelector = mkSelector "changeFont:"

-- | @Selector@ for @changeAttributes:@
changeAttributesSelector :: Selector
changeAttributesSelector = mkSelector "changeAttributes:"

-- | @Selector@ for @changeDocumentBackgroundColor:@
changeDocumentBackgroundColorSelector :: Selector
changeDocumentBackgroundColorSelector = mkSelector "changeDocumentBackgroundColor:"

-- | @Selector@ for @changeColor:@
changeColorSelector :: Selector
changeColorSelector = mkSelector "changeColor:"

-- | @Selector@ for @alignCenter:@
alignCenterSelector :: Selector
alignCenterSelector = mkSelector "alignCenter:"

-- | @Selector@ for @alignJustified:@
alignJustifiedSelector :: Selector
alignJustifiedSelector = mkSelector "alignJustified:"

-- | @Selector@ for @alignLeft:@
alignLeftSelector :: Selector
alignLeftSelector = mkSelector "alignLeft:"

-- | @Selector@ for @alignRight:@
alignRightSelector :: Selector
alignRightSelector = mkSelector "alignRight:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @performFindPanelAction:@
performFindPanelActionSelector :: Selector
performFindPanelActionSelector = mkSelector "performFindPanelAction:"

-- | @Selector@ for @startSpeaking:@
startSpeakingSelector :: Selector
startSpeakingSelector = mkSelector "startSpeaking:"

-- | @Selector@ for @stopSpeaking:@
stopSpeakingSelector :: Selector
stopSpeakingSelector = mkSelector "stopSpeaking:"

-- | @Selector@ for @moveToBeginningOfSentence:@
moveToBeginningOfSentenceSelector :: Selector
moveToBeginningOfSentenceSelector = mkSelector "moveToBeginningOfSentence:"

-- | @Selector@ for @moveToBeginningOfSentenceAndModifySelection:@
moveToBeginningOfSentenceAndModifySelectionSelector :: Selector
moveToBeginningOfSentenceAndModifySelectionSelector = mkSelector "moveToBeginningOfSentenceAndModifySelection:"

-- | @Selector@ for @moveToEndOfSentence:@
moveToEndOfSentenceSelector :: Selector
moveToEndOfSentenceSelector = mkSelector "moveToEndOfSentence:"

-- | @Selector@ for @moveToEndOfSentenceAndModifySelection:@
moveToEndOfSentenceAndModifySelectionSelector :: Selector
moveToEndOfSentenceAndModifySelectionSelector = mkSelector "moveToEndOfSentenceAndModifySelection:"

-- | @Selector@ for @selectSentence:@
selectSentenceSelector :: Selector
selectSentenceSelector = mkSelector "selectSentence:"

-- | @Selector@ for @overWrite:@
overWriteSelector :: Selector
overWriteSelector = mkSelector "overWrite:"

-- | @Selector@ for @replaceSelectionWithNode:@
replaceSelectionWithNodeSelector :: Selector
replaceSelectionWithNodeSelector = mkSelector "replaceSelectionWithNode:"

-- | @Selector@ for @replaceSelectionWithText:@
replaceSelectionWithTextSelector :: Selector
replaceSelectionWithTextSelector = mkSelector "replaceSelectionWithText:"

-- | @Selector@ for @replaceSelectionWithMarkupString:@
replaceSelectionWithMarkupStringSelector :: Selector
replaceSelectionWithMarkupStringSelector = mkSelector "replaceSelectionWithMarkupString:"

-- | @Selector@ for @replaceSelectionWithArchive:@
replaceSelectionWithArchiveSelector :: Selector
replaceSelectionWithArchiveSelector = mkSelector "replaceSelectionWithArchive:"

-- | @Selector@ for @deleteSelection@
deleteSelectionSelector :: Selector
deleteSelectionSelector = mkSelector "deleteSelection"

-- | @Selector@ for @applyStyle:@
applyStyleSelector :: Selector
applyStyleSelector = mkSelector "applyStyle:"

-- | @Selector@ for @editableDOMRangeForPoint:@
editableDOMRangeForPointSelector :: Selector
editableDOMRangeForPointSelector = mkSelector "editableDOMRangeForPoint:"

-- | @Selector@ for @setSelectedDOMRange:affinity:@
setSelectedDOMRange_affinitySelector :: Selector
setSelectedDOMRange_affinitySelector = mkSelector "setSelectedDOMRange:affinity:"

-- | @Selector@ for @styleDeclarationWithText:@
styleDeclarationWithTextSelector :: Selector
styleDeclarationWithTextSelector = mkSelector "styleDeclarationWithText:"

-- | @Selector@ for @computedStyleForElement:pseudoElement:@
computedStyleForElement_pseudoElementSelector :: Selector
computedStyleForElement_pseudoElementSelector = mkSelector "computedStyleForElement:pseudoElement:"

-- | @Selector@ for @takeStringURLFrom:@
takeStringURLFromSelector :: Selector
takeStringURLFromSelector = mkSelector "takeStringURLFrom:"

-- | @Selector@ for @stopLoading:@
stopLoadingSelector :: Selector
stopLoadingSelector = mkSelector "stopLoading:"

-- | @Selector@ for @reload:@
reloadSelector :: Selector
reloadSelector = mkSelector "reload:"

-- | @Selector@ for @reloadFromOrigin:@
reloadFromOriginSelector :: Selector
reloadFromOriginSelector = mkSelector "reloadFromOrigin:"

-- | @Selector@ for @makeTextLarger:@
makeTextLargerSelector :: Selector
makeTextLargerSelector = mkSelector "makeTextLarger:"

-- | @Selector@ for @makeTextSmaller:@
makeTextSmallerSelector :: Selector
makeTextSmallerSelector = mkSelector "makeTextSmaller:"

-- | @Selector@ for @makeTextStandardSize:@
makeTextStandardSizeSelector :: Selector
makeTextStandardSizeSelector = mkSelector "makeTextStandardSize:"

-- | @Selector@ for @toggleContinuousSpellChecking:@
toggleContinuousSpellCheckingSelector :: Selector
toggleContinuousSpellCheckingSelector = mkSelector "toggleContinuousSpellChecking:"

-- | @Selector@ for @toggleSmartInsertDelete:@
toggleSmartInsertDeleteSelector :: Selector
toggleSmartInsertDeleteSelector = mkSelector "toggleSmartInsertDelete:"

-- | @Selector@ for @shouldCloseWithWindow@
shouldCloseWithWindowSelector :: Selector
shouldCloseWithWindowSelector = mkSelector "shouldCloseWithWindow"

-- | @Selector@ for @setShouldCloseWithWindow:@
setShouldCloseWithWindowSelector :: Selector
setShouldCloseWithWindowSelector = mkSelector "setShouldCloseWithWindow:"

-- | @Selector@ for @mainFrame@
mainFrameSelector :: Selector
mainFrameSelector = mkSelector "mainFrame"

-- | @Selector@ for @selectedFrame@
selectedFrameSelector :: Selector
selectedFrameSelector = mkSelector "selectedFrame"

-- | @Selector@ for @backForwardList@
backForwardListSelector :: Selector
backForwardListSelector = mkSelector "backForwardList"

-- | @Selector@ for @textSizeMultiplier@
textSizeMultiplierSelector :: Selector
textSizeMultiplierSelector = mkSelector "textSizeMultiplier"

-- | @Selector@ for @setTextSizeMultiplier:@
setTextSizeMultiplierSelector :: Selector
setTextSizeMultiplierSelector = mkSelector "setTextSizeMultiplier:"

-- | @Selector@ for @applicationNameForUserAgent@
applicationNameForUserAgentSelector :: Selector
applicationNameForUserAgentSelector = mkSelector "applicationNameForUserAgent"

-- | @Selector@ for @setApplicationNameForUserAgent:@
setApplicationNameForUserAgentSelector :: Selector
setApplicationNameForUserAgentSelector = mkSelector "setApplicationNameForUserAgent:"

-- | @Selector@ for @customUserAgent@
customUserAgentSelector :: Selector
customUserAgentSelector = mkSelector "customUserAgent"

-- | @Selector@ for @setCustomUserAgent:@
setCustomUserAgentSelector :: Selector
setCustomUserAgentSelector = mkSelector "setCustomUserAgent:"

-- | @Selector@ for @supportsTextEncoding@
supportsTextEncodingSelector :: Selector
supportsTextEncodingSelector = mkSelector "supportsTextEncoding"

-- | @Selector@ for @customTextEncodingName@
customTextEncodingNameSelector :: Selector
customTextEncodingNameSelector = mkSelector "customTextEncodingName"

-- | @Selector@ for @setCustomTextEncodingName:@
setCustomTextEncodingNameSelector :: Selector
setCustomTextEncodingNameSelector = mkSelector "setCustomTextEncodingName:"

-- | @Selector@ for @mediaStyle@
mediaStyleSelector :: Selector
mediaStyleSelector = mkSelector "mediaStyle"

-- | @Selector@ for @setMediaStyle:@
setMediaStyleSelector :: Selector
setMediaStyleSelector = mkSelector "setMediaStyle:"

-- | @Selector@ for @windowScriptObject@
windowScriptObjectSelector :: Selector
windowScriptObjectSelector = mkSelector "windowScriptObject"

-- | @Selector@ for @preferences@
preferencesSelector :: Selector
preferencesSelector = mkSelector "preferences"

-- | @Selector@ for @setPreferences:@
setPreferencesSelector :: Selector
setPreferencesSelector = mkSelector "setPreferences:"

-- | @Selector@ for @preferencesIdentifier@
preferencesIdentifierSelector :: Selector
preferencesIdentifierSelector = mkSelector "preferencesIdentifier"

-- | @Selector@ for @setPreferencesIdentifier:@
setPreferencesIdentifierSelector :: Selector
setPreferencesIdentifierSelector = mkSelector "setPreferencesIdentifier:"

-- | @Selector@ for @hostWindow@
hostWindowSelector :: Selector
hostWindowSelector = mkSelector "hostWindow"

-- | @Selector@ for @setHostWindow:@
setHostWindowSelector :: Selector
setHostWindowSelector = mkSelector "setHostWindow:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector
setGroupNameSelector = mkSelector "setGroupName:"

-- | @Selector@ for @estimatedProgress@
estimatedProgressSelector :: Selector
estimatedProgressSelector = mkSelector "estimatedProgress"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

-- | @Selector@ for @pasteboardTypesForSelection@
pasteboardTypesForSelectionSelector :: Selector
pasteboardTypesForSelectionSelector = mkSelector "pasteboardTypesForSelection"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @shouldUpdateWhileOffscreen@
shouldUpdateWhileOffscreenSelector :: Selector
shouldUpdateWhileOffscreenSelector = mkSelector "shouldUpdateWhileOffscreen"

-- | @Selector@ for @setShouldUpdateWhileOffscreen:@
setShouldUpdateWhileOffscreenSelector :: Selector
setShouldUpdateWhileOffscreenSelector = mkSelector "setShouldUpdateWhileOffscreen:"

-- | @Selector@ for @mainFrameURL@
mainFrameURLSelector :: Selector
mainFrameURLSelector = mkSelector "mainFrameURL"

-- | @Selector@ for @setMainFrameURL:@
setMainFrameURLSelector :: Selector
setMainFrameURLSelector = mkSelector "setMainFrameURL:"

-- | @Selector@ for @mainFrameDocument@
mainFrameDocumentSelector :: Selector
mainFrameDocumentSelector = mkSelector "mainFrameDocument"

-- | @Selector@ for @mainFrameTitle@
mainFrameTitleSelector :: Selector
mainFrameTitleSelector = mkSelector "mainFrameTitle"

-- | @Selector@ for @mainFrameIcon@
mainFrameIconSelector :: Selector
mainFrameIconSelector = mkSelector "mainFrameIcon"

-- | @Selector@ for @selectedDOMRange@
selectedDOMRangeSelector :: Selector
selectedDOMRangeSelector = mkSelector "selectedDOMRange"

-- | @Selector@ for @selectionAffinity@
selectionAffinitySelector :: Selector
selectionAffinitySelector = mkSelector "selectionAffinity"

-- | @Selector@ for @maintainsInactiveSelection@
maintainsInactiveSelectionSelector :: Selector
maintainsInactiveSelectionSelector = mkSelector "maintainsInactiveSelection"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @typingStyle@
typingStyleSelector :: Selector
typingStyleSelector = mkSelector "typingStyle"

-- | @Selector@ for @setTypingStyle:@
setTypingStyleSelector :: Selector
setTypingStyleSelector = mkSelector "setTypingStyle:"

-- | @Selector@ for @smartInsertDeleteEnabled@
smartInsertDeleteEnabledSelector :: Selector
smartInsertDeleteEnabledSelector = mkSelector "smartInsertDeleteEnabled"

-- | @Selector@ for @setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabledSelector :: Selector
setSmartInsertDeleteEnabledSelector = mkSelector "setSmartInsertDeleteEnabled:"

-- | @Selector@ for @continuousSpellCheckingEnabled@
continuousSpellCheckingEnabledSelector :: Selector
continuousSpellCheckingEnabledSelector = mkSelector "continuousSpellCheckingEnabled"

-- | @Selector@ for @setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabledSelector :: Selector
setContinuousSpellCheckingEnabledSelector = mkSelector "setContinuousSpellCheckingEnabled:"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @canMakeTextLarger@
canMakeTextLargerSelector :: Selector
canMakeTextLargerSelector = mkSelector "canMakeTextLarger"

-- | @Selector@ for @canMakeTextSmaller@
canMakeTextSmallerSelector :: Selector
canMakeTextSmallerSelector = mkSelector "canMakeTextSmaller"

-- | @Selector@ for @canMakeTextStandardSize@
canMakeTextStandardSizeSelector :: Selector
canMakeTextStandardSizeSelector = mkSelector "canMakeTextStandardSize"

