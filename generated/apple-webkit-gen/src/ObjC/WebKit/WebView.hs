{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , uiDelegate
  , setUIDelegate
  , resourceLoadDelegate
  , setResourceLoadDelegate
  , downloadDelegate
  , setDownloadDelegate
  , frameLoadDelegate
  , setFrameLoadDelegate
  , policyDelegate
  , setPolicyDelegate
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
  , editingDelegate
  , setEditingDelegate
  , canGoBack
  , canGoForward
  , canMakeTextLarger
  , canMakeTextSmaller
  , canMakeTextStandardSize
  , alignCenterSelector
  , alignJustifiedSelector
  , alignLeftSelector
  , alignRightSelector
  , applicationNameForUserAgentSelector
  , applyStyleSelector
  , backForwardListSelector
  , canGoBackSelector
  , canGoForwardSelector
  , canMakeTextLargerSelector
  , canMakeTextSmallerSelector
  , canMakeTextStandardSizeSelector
  , canShowMIMETypeAsHTMLSelector
  , canShowMIMETypeSelector
  , changeAttributesSelector
  , changeColorSelector
  , changeDocumentBackgroundColorSelector
  , changeFontSelector
  , checkSpellingSelector
  , closeSelector
  , computedStyleForElement_pseudoElementSelector
  , continuousSpellCheckingEnabledSelector
  , copyFontSelector
  , copySelector
  , customTextEncodingNameSelector
  , customUserAgentSelector
  , cutSelector
  , deleteSelectionSelector
  , deleteSelector
  , downloadDelegateSelector
  , drawsBackgroundSelector
  , editableDOMRangeForPointSelector
  , editableSelector
  , editingDelegateSelector
  , elementAtPointSelector
  , estimatedProgressSelector
  , frameLoadDelegateSelector
  , goBackSelector
  , goForwardSelector
  , goToBackForwardItemSelector
  , groupNameSelector
  , hostWindowSelector
  , initWithFrame_frameName_groupNameSelector
  , loadingSelector
  , mainFrameDocumentSelector
  , mainFrameIconSelector
  , mainFrameSelector
  , mainFrameTitleSelector
  , mainFrameURLSelector
  , maintainsInactiveSelectionSelector
  , makeTextLargerSelector
  , makeTextSmallerSelector
  , makeTextStandardSizeSelector
  , mediaStyleSelector
  , mimeTypesShownAsHTMLSelector
  , moveDragCaretToPointSelector
  , moveToBeginningOfSentenceAndModifySelectionSelector
  , moveToBeginningOfSentenceSelector
  , moveToEndOfSentenceAndModifySelectionSelector
  , moveToEndOfSentenceSelector
  , overWriteSelector
  , pasteAsPlainTextSelector
  , pasteAsRichTextSelector
  , pasteFontSelector
  , pasteSelector
  , pasteboardTypesForElementSelector
  , pasteboardTypesForSelectionSelector
  , performFindPanelActionSelector
  , policyDelegateSelector
  , preferencesIdentifierSelector
  , preferencesSelector
  , registerURLSchemeAsLocalSelector
  , registerViewClass_representationClass_forMIMETypeSelector
  , reloadFromOriginSelector
  , reloadSelector
  , removeDragCaretSelector
  , replaceSelectionWithArchiveSelector
  , replaceSelectionWithMarkupStringSelector
  , replaceSelectionWithNodeSelector
  , replaceSelectionWithTextSelector
  , resourceLoadDelegateSelector
  , searchFor_direction_caseSensitive_wrapSelector
  , selectSentenceSelector
  , selectedDOMRangeSelector
  , selectedFrameSelector
  , selectionAffinitySelector
  , setApplicationNameForUserAgentSelector
  , setContinuousSpellCheckingEnabledSelector
  , setCustomTextEncodingNameSelector
  , setCustomUserAgentSelector
  , setDownloadDelegateSelector
  , setDrawsBackgroundSelector
  , setEditableSelector
  , setEditingDelegateSelector
  , setFrameLoadDelegateSelector
  , setGroupNameSelector
  , setHostWindowSelector
  , setMIMETypesShownAsHTMLSelector
  , setMainFrameURLSelector
  , setMaintainsBackForwardListSelector
  , setMediaStyleSelector
  , setPolicyDelegateSelector
  , setPreferencesIdentifierSelector
  , setPreferencesSelector
  , setResourceLoadDelegateSelector
  , setSelectedDOMRange_affinitySelector
  , setShouldCloseWithWindowSelector
  , setShouldUpdateWhileOffscreenSelector
  , setSmartInsertDeleteEnabledSelector
  , setTextSizeMultiplierSelector
  , setTypingStyleSelector
  , setUIDelegateSelector
  , shouldCloseWithWindowSelector
  , shouldUpdateWhileOffscreenSelector
  , showGuessPanelSelector
  , smartInsertDeleteEnabledSelector
  , spellCheckerDocumentTagSelector
  , startSpeakingSelector
  , stopLoadingSelector
  , stopSpeakingSelector
  , stringByEvaluatingJavaScriptFromStringSelector
  , styleDeclarationWithTextSelector
  , supportsTextEncodingSelector
  , takeStringURLFromSelector
  , textSizeMultiplierSelector
  , toggleContinuousSpellCheckingSelector
  , toggleSmartInsertDeleteSelector
  , typingStyleSelector
  , uiDelegateSelector
  , undoManagerSelector
  , urlFromPasteboardSelector
  , urlTitleFromPasteboardSelector
  , userAgentForURLSelector
  , windowScriptObjectSelector
  , writeElement_withPasteboardTypes_toPasteboardSelector
  , writeSelectionWithPasteboardTypes_toPasteboardSelector

  -- * Enum types
  , NSSelectionAffinity(NSSelectionAffinity)
  , pattern NSSelectionAffinityUpstream
  , pattern NSSelectionAffinityDownstream

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' canShowMIMETypeSelector (toNSString mimeType)

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
    sendClassMessage cls' canShowMIMETypeAsHTMLSelector (toNSString mimeType)

-- | MIMETypesShownAsHTML
--
-- Returns: Returns an array of NSStrings that describe the MIME types    WebKit will attempt to render as HTML.
--
-- ObjC selector: @+ MIMETypesShownAsHTML@
mimeTypesShownAsHTML :: IO (Id NSArray)
mimeTypesShownAsHTML  =
  do
    cls' <- getRequiredClass "WebView"
    sendClassMessage cls' mimeTypesShownAsHTMLSelector

-- | setMIMETypesShownAsHTML:
--
-- Sets the array of NSString MIME types that WebKit will    attempt to render as HTML.  Typically you will retrieve the built-in    array using MIMETypesShownAsHTML and add additional MIME types to that    array.
--
-- ObjC selector: @+ setMIMETypesShownAsHTML:@
setMIMETypesShownAsHTML :: IsNSArray mimeTypes => mimeTypes -> IO ()
setMIMETypesShownAsHTML mimeTypes =
  do
    cls' <- getRequiredClass "WebView"
    sendClassMessage cls' setMIMETypesShownAsHTMLSelector (toNSArray mimeTypes)

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
    sendClassMessage cls' urlFromPasteboardSelector (toNSPasteboard pasteboard)

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
    sendClassMessage cls' urlTitleFromPasteboardSelector (toNSPasteboard pasteboard)

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
    sendClassMessage cls' registerURLSchemeAsLocalSelector (toNSString scheme)

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
initWithFrame_frameName_groupName webView frame frameName groupName =
  sendOwnedMessage webView initWithFrame_frameName_groupNameSelector frame (toNSString frameName) (toNSString groupName)

-- | close
--
-- Closes the receiver, unloading its web page and canceling any pending loads.    Once the receiver has closed, it will no longer respond to requests or fire delegate methods.    (However, the -close method itself may fire delegate methods.)
--
-- A garbage collected application is required to call close when the receiver is no longer needed.    The close method will be called automatically when the window or hostWindow closes and shouldCloseWithWindow returns YES.    A non-garbage collected application can still call close, providing a convenient way to prevent receiver    from doing any more loading and firing any future delegate methods.
--
-- ObjC selector: @- close@
close :: IsWebView webView => webView -> IO ()
close webView =
  sendMessage webView closeSelector

-- | setMaintainsBackForwardList:
--
-- Enable or disable the use of a backforward list for this webView.
--
-- @flag@ — Turns use of the back forward list on or off
--
-- ObjC selector: @- setMaintainsBackForwardList:@
setMaintainsBackForwardList :: IsWebView webView => webView -> Bool -> IO ()
setMaintainsBackForwardList webView flag =
  sendMessage webView setMaintainsBackForwardListSelector flag

-- | goBack
--
-- Go back to the previous URL in the backforward list.
--
-- Returns: YES if able to go back in the backforward list, NO otherwise.
--
-- ObjC selector: @- goBack@
goBack :: IsWebView webView => webView -> IO Bool
goBack webView =
  sendMessage webView goBackSelector

-- | goForward
--
-- Go forward to the next URL in the backforward list.
--
-- Returns: YES if able to go forward in the backforward list, NO otherwise.
--
-- ObjC selector: @- goForward@
goForward :: IsWebView webView => webView -> IO Bool
goForward webView =
  sendMessage webView goForwardSelector

-- | goToBackForwardItem:
--
-- Go back or forward to an item in the backforward list.
--
-- Returns: YES if able to go to the item, NO otherwise.
--
-- ObjC selector: @- goToBackForwardItem:@
goToBackForwardItem :: (IsWebView webView, IsWebHistoryItem item) => webView -> item -> IO Bool
goToBackForwardItem webView item =
  sendMessage webView goToBackForwardItemSelector (toWebHistoryItem item)

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
userAgentForURL webView url =
  sendMessage webView userAgentForURLSelector (toNSURL url)

-- | stringByEvaluatingJavaScriptFromString:
--
-- @script@ — The text of the JavaScript.
--
-- Returns: The result of the script, converted to a string, or nil for failure.
--
-- ObjC selector: @- stringByEvaluatingJavaScriptFromString:@
stringByEvaluatingJavaScriptFromString :: (IsWebView webView, IsNSString script) => webView -> script -> IO (Id NSString)
stringByEvaluatingJavaScriptFromString webView script =
  sendMessage webView stringByEvaluatingJavaScriptFromStringSelector (toNSString script)

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
searchFor_direction_caseSensitive_wrap webView string forward caseFlag wrapFlag =
  sendMessage webView searchFor_direction_caseSensitive_wrapSelector (toNSString string) forward caseFlag wrapFlag

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
    sendClassMessage cls' registerViewClass_representationClass_forMIMETypeSelector viewClass representationClass (toNSString mimeType)

-- | elementAtPoint:
--
-- @point@ — A point in the coordinates of the WebView
--
-- Returns: An element dictionary describing the point
--
-- ObjC selector: @- elementAtPoint:@
elementAtPoint :: IsWebView webView => webView -> NSPoint -> IO (Id NSDictionary)
elementAtPoint webView point =
  sendMessage webView elementAtPointSelector point

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
writeSelectionWithPasteboardTypes_toPasteboard webView types pasteboard =
  sendMessage webView writeSelectionWithPasteboardTypes_toPasteboardSelector (toNSArray types) (toNSPasteboard pasteboard)

-- | pasteboardTypesForElement:
--
-- Returns the pasteboard types that WebView can use for an element
--
-- @element@ — The element
--
-- ObjC selector: @- pasteboardTypesForElement:@
pasteboardTypesForElement :: (IsWebView webView, IsNSDictionary element) => webView -> element -> IO (Id NSArray)
pasteboardTypesForElement webView element =
  sendMessage webView pasteboardTypesForElementSelector (toNSDictionary element)

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
writeElement_withPasteboardTypes_toPasteboard webView element types pasteboard =
  sendMessage webView writeElement_withPasteboardTypes_toPasteboardSelector (toNSDictionary element) (toNSArray types) (toNSPasteboard pasteboard)

-- | moveDragCaretToPoint:
--
-- @point@ — A point in the coordinates of the WebView
--
-- This method moves the caret that shows where something being dragged will be dropped. It may cause the WebView to scroll    to make the new position of the drag caret visible.
--
-- ObjC selector: @- moveDragCaretToPoint:@
moveDragCaretToPoint :: IsWebView webView => webView -> NSPoint -> IO ()
moveDragCaretToPoint webView point =
  sendMessage webView moveDragCaretToPointSelector point

-- | removeDragCaret
--
-- Removes the drag caret from the WebView
--
-- ObjC selector: @- removeDragCaret@
removeDragCaret :: IsWebView webView => webView -> IO ()
removeDragCaret webView =
  sendMessage webView removeDragCaretSelector

-- | @- copy:@
copy :: IsWebView webView => webView -> RawId -> IO ()
copy webView sender =
  sendOwnedMessage webView copySelector sender

-- | @- cut:@
cut :: IsWebView webView => webView -> RawId -> IO ()
cut webView sender =
  sendMessage webView cutSelector sender

-- | @- paste:@
paste :: IsWebView webView => webView -> RawId -> IO ()
paste webView sender =
  sendMessage webView pasteSelector sender

-- | @- copyFont:@
copyFont :: IsWebView webView => webView -> RawId -> IO ()
copyFont webView sender =
  sendOwnedMessage webView copyFontSelector sender

-- | @- pasteFont:@
pasteFont :: IsWebView webView => webView -> RawId -> IO ()
pasteFont webView sender =
  sendMessage webView pasteFontSelector sender

-- | @- delete:@
delete :: IsWebView webView => webView -> RawId -> IO ()
delete webView sender =
  sendMessage webView deleteSelector sender

-- | @- pasteAsPlainText:@
pasteAsPlainText :: IsWebView webView => webView -> RawId -> IO ()
pasteAsPlainText webView sender =
  sendMessage webView pasteAsPlainTextSelector sender

-- | @- pasteAsRichText:@
pasteAsRichText :: IsWebView webView => webView -> RawId -> IO ()
pasteAsRichText webView sender =
  sendMessage webView pasteAsRichTextSelector sender

-- | @- changeFont:@
changeFont :: IsWebView webView => webView -> RawId -> IO ()
changeFont webView sender =
  sendMessage webView changeFontSelector sender

-- | @- changeAttributes:@
changeAttributes :: IsWebView webView => webView -> RawId -> IO ()
changeAttributes webView sender =
  sendMessage webView changeAttributesSelector sender

-- | @- changeDocumentBackgroundColor:@
changeDocumentBackgroundColor :: IsWebView webView => webView -> RawId -> IO ()
changeDocumentBackgroundColor webView sender =
  sendMessage webView changeDocumentBackgroundColorSelector sender

-- | @- changeColor:@
changeColor :: IsWebView webView => webView -> RawId -> IO ()
changeColor webView sender =
  sendMessage webView changeColorSelector sender

-- | @- alignCenter:@
alignCenter :: IsWebView webView => webView -> RawId -> IO ()
alignCenter webView sender =
  sendMessage webView alignCenterSelector sender

-- | @- alignJustified:@
alignJustified :: IsWebView webView => webView -> RawId -> IO ()
alignJustified webView sender =
  sendMessage webView alignJustifiedSelector sender

-- | @- alignLeft:@
alignLeft :: IsWebView webView => webView -> RawId -> IO ()
alignLeft webView sender =
  sendMessage webView alignLeftSelector sender

-- | @- alignRight:@
alignRight :: IsWebView webView => webView -> RawId -> IO ()
alignRight webView sender =
  sendMessage webView alignRightSelector sender

-- | @- checkSpelling:@
checkSpelling :: IsWebView webView => webView -> RawId -> IO ()
checkSpelling webView sender =
  sendMessage webView checkSpellingSelector sender

-- | @- showGuessPanel:@
showGuessPanel :: IsWebView webView => webView -> RawId -> IO ()
showGuessPanel webView sender =
  sendMessage webView showGuessPanelSelector sender

-- | @- performFindPanelAction:@
performFindPanelAction :: IsWebView webView => webView -> RawId -> IO ()
performFindPanelAction webView sender =
  sendMessage webView performFindPanelActionSelector sender

-- | @- startSpeaking:@
startSpeaking :: IsWebView webView => webView -> RawId -> IO ()
startSpeaking webView sender =
  sendMessage webView startSpeakingSelector sender

-- | @- stopSpeaking:@
stopSpeaking :: IsWebView webView => webView -> RawId -> IO ()
stopSpeaking webView sender =
  sendMessage webView stopSpeakingSelector sender

-- | @- moveToBeginningOfSentence:@
moveToBeginningOfSentence :: IsWebView webView => webView -> RawId -> IO ()
moveToBeginningOfSentence webView sender =
  sendMessage webView moveToBeginningOfSentenceSelector sender

-- | @- moveToBeginningOfSentenceAndModifySelection:@
moveToBeginningOfSentenceAndModifySelection :: IsWebView webView => webView -> RawId -> IO ()
moveToBeginningOfSentenceAndModifySelection webView sender =
  sendMessage webView moveToBeginningOfSentenceAndModifySelectionSelector sender

-- | @- moveToEndOfSentence:@
moveToEndOfSentence :: IsWebView webView => webView -> RawId -> IO ()
moveToEndOfSentence webView sender =
  sendMessage webView moveToEndOfSentenceSelector sender

-- | @- moveToEndOfSentenceAndModifySelection:@
moveToEndOfSentenceAndModifySelection :: IsWebView webView => webView -> RawId -> IO ()
moveToEndOfSentenceAndModifySelection webView sender =
  sendMessage webView moveToEndOfSentenceAndModifySelectionSelector sender

-- | @- selectSentence:@
selectSentence :: IsWebView webView => webView -> RawId -> IO ()
selectSentence webView sender =
  sendMessage webView selectSentenceSelector sender

-- | @- overWrite:@
overWrite :: IsWebView webView => webView -> RawId -> IO ()
overWrite webView sender =
  sendMessage webView overWriteSelector sender

-- | @- replaceSelectionWithNode:@
replaceSelectionWithNode :: (IsWebView webView, IsDOMNode node) => webView -> node -> IO ()
replaceSelectionWithNode webView node =
  sendMessage webView replaceSelectionWithNodeSelector (toDOMNode node)

-- | @- replaceSelectionWithText:@
replaceSelectionWithText :: (IsWebView webView, IsNSString text) => webView -> text -> IO ()
replaceSelectionWithText webView text =
  sendMessage webView replaceSelectionWithTextSelector (toNSString text)

-- | @- replaceSelectionWithMarkupString:@
replaceSelectionWithMarkupString :: (IsWebView webView, IsNSString markupString) => webView -> markupString -> IO ()
replaceSelectionWithMarkupString webView markupString =
  sendMessage webView replaceSelectionWithMarkupStringSelector (toNSString markupString)

-- | @- replaceSelectionWithArchive:@
replaceSelectionWithArchive :: (IsWebView webView, IsWebArchive archive) => webView -> archive -> IO ()
replaceSelectionWithArchive webView archive =
  sendMessage webView replaceSelectionWithArchiveSelector (toWebArchive archive)

-- | @- deleteSelection@
deleteSelection :: IsWebView webView => webView -> IO ()
deleteSelection webView =
  sendMessage webView deleteSelectionSelector

-- | @- applyStyle:@
applyStyle :: (IsWebView webView, IsDOMCSSStyleDeclaration style) => webView -> style -> IO ()
applyStyle webView style =
  sendMessage webView applyStyleSelector (toDOMCSSStyleDeclaration style)

-- | @- editableDOMRangeForPoint:@
editableDOMRangeForPoint :: IsWebView webView => webView -> NSPoint -> IO (Id DOMRange)
editableDOMRangeForPoint webView point =
  sendMessage webView editableDOMRangeForPointSelector point

-- | @- setSelectedDOMRange:affinity:@
setSelectedDOMRange_affinity :: (IsWebView webView, IsDOMRange range) => webView -> range -> NSSelectionAffinity -> IO ()
setSelectedDOMRange_affinity webView range selectionAffinity =
  sendMessage webView setSelectedDOMRange_affinitySelector (toDOMRange range) selectionAffinity

-- | @- styleDeclarationWithText:@
styleDeclarationWithText :: (IsWebView webView, IsNSString text) => webView -> text -> IO (Id DOMCSSStyleDeclaration)
styleDeclarationWithText webView text =
  sendMessage webView styleDeclarationWithTextSelector (toNSString text)

-- | @- computedStyleForElement:pseudoElement:@
computedStyleForElement_pseudoElement :: (IsWebView webView, IsDOMElement element, IsNSString pseudoElement) => webView -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
computedStyleForElement_pseudoElement webView element pseudoElement =
  sendMessage webView computedStyleForElement_pseudoElementSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- takeStringURLFrom:@
takeStringURLFrom :: IsWebView webView => webView -> RawId -> IO ()
takeStringURLFrom webView sender =
  sendMessage webView takeStringURLFromSelector sender

-- | @- stopLoading:@
stopLoading :: IsWebView webView => webView -> RawId -> IO ()
stopLoading webView sender =
  sendMessage webView stopLoadingSelector sender

-- | @- reload:@
reload :: IsWebView webView => webView -> RawId -> IO ()
reload webView sender =
  sendMessage webView reloadSelector sender

-- | @- reloadFromOrigin:@
reloadFromOrigin :: IsWebView webView => webView -> RawId -> IO ()
reloadFromOrigin webView sender =
  sendMessage webView reloadFromOriginSelector sender

-- | @- makeTextLarger:@
makeTextLarger :: IsWebView webView => webView -> RawId -> IO ()
makeTextLarger webView sender =
  sendMessage webView makeTextLargerSelector sender

-- | @- makeTextSmaller:@
makeTextSmaller :: IsWebView webView => webView -> RawId -> IO ()
makeTextSmaller webView sender =
  sendMessage webView makeTextSmallerSelector sender

-- | @- makeTextStandardSize:@
makeTextStandardSize :: IsWebView webView => webView -> RawId -> IO ()
makeTextStandardSize webView sender =
  sendMessage webView makeTextStandardSizeSelector sender

-- | @- toggleContinuousSpellChecking:@
toggleContinuousSpellChecking :: IsWebView webView => webView -> RawId -> IO ()
toggleContinuousSpellChecking webView sender =
  sendMessage webView toggleContinuousSpellCheckingSelector sender

-- | @- toggleSmartInsertDelete:@
toggleSmartInsertDelete :: IsWebView webView => webView -> RawId -> IO ()
toggleSmartInsertDelete webView sender =
  sendMessage webView toggleSmartInsertDeleteSelector sender

-- | shouldCloseWithWindow
--
-- Whether the receiver closes when either it's window or hostWindow closes.
--
-- Defaults to YES in garbage collected applications, otherwise NO to maintain backwards compatibility.
--
-- ObjC selector: @- shouldCloseWithWindow@
shouldCloseWithWindow :: IsWebView webView => webView -> IO Bool
shouldCloseWithWindow webView =
  sendMessage webView shouldCloseWithWindowSelector

-- | shouldCloseWithWindow
--
-- Whether the receiver closes when either it's window or hostWindow closes.
--
-- Defaults to YES in garbage collected applications, otherwise NO to maintain backwards compatibility.
--
-- ObjC selector: @- setShouldCloseWithWindow:@
setShouldCloseWithWindow :: IsWebView webView => webView -> Bool -> IO ()
setShouldCloseWithWindow webView value =
  sendMessage webView setShouldCloseWithWindowSelector value

-- | UIDelegate
--
-- The WebView's WebUIDelegate.
--
-- ObjC selector: @- UIDelegate@
uiDelegate :: IsWebView webView => webView -> IO RawId
uiDelegate webView =
  sendMessage webView uiDelegateSelector

-- | UIDelegate
--
-- The WebView's WebUIDelegate.
--
-- ObjC selector: @- setUIDelegate:@
setUIDelegate :: IsWebView webView => webView -> RawId -> IO ()
setUIDelegate webView value =
  sendMessage webView setUIDelegateSelector value

-- | resourceLoadDelegate
--
-- The WebView's WebResourceLoadDelegate.
--
-- ObjC selector: @- resourceLoadDelegate@
resourceLoadDelegate :: IsWebView webView => webView -> IO RawId
resourceLoadDelegate webView =
  sendMessage webView resourceLoadDelegateSelector

-- | resourceLoadDelegate
--
-- The WebView's WebResourceLoadDelegate.
--
-- ObjC selector: @- setResourceLoadDelegate:@
setResourceLoadDelegate :: IsWebView webView => webView -> RawId -> IO ()
setResourceLoadDelegate webView value =
  sendMessage webView setResourceLoadDelegateSelector value

-- | downloadDelegate
--
-- The WebView's WebDownloadDelegate.
--
-- ObjC selector: @- downloadDelegate@
downloadDelegate :: IsWebView webView => webView -> IO RawId
downloadDelegate webView =
  sendMessage webView downloadDelegateSelector

-- | downloadDelegate
--
-- The WebView's WebDownloadDelegate.
--
-- ObjC selector: @- setDownloadDelegate:@
setDownloadDelegate :: IsWebView webView => webView -> RawId -> IO ()
setDownloadDelegate webView value =
  sendMessage webView setDownloadDelegateSelector value

-- | frameLoadDelegate
--
-- The WebView's WebFrameLoadDelegate delegate.
--
-- ObjC selector: @- frameLoadDelegate@
frameLoadDelegate :: IsWebView webView => webView -> IO RawId
frameLoadDelegate webView =
  sendMessage webView frameLoadDelegateSelector

-- | frameLoadDelegate
--
-- The WebView's WebFrameLoadDelegate delegate.
--
-- ObjC selector: @- setFrameLoadDelegate:@
setFrameLoadDelegate :: IsWebView webView => webView -> RawId -> IO ()
setFrameLoadDelegate webView value =
  sendMessage webView setFrameLoadDelegateSelector value

-- | policyDelegate
--
-- The WebView's WebPolicyDelegate.
--
-- ObjC selector: @- policyDelegate@
policyDelegate :: IsWebView webView => webView -> IO RawId
policyDelegate webView =
  sendMessage webView policyDelegateSelector

-- | policyDelegate
--
-- The WebView's WebPolicyDelegate.
--
-- ObjC selector: @- setPolicyDelegate:@
setPolicyDelegate :: IsWebView webView => webView -> RawId -> IO ()
setPolicyDelegate webView value =
  sendMessage webView setPolicyDelegateSelector value

-- | mainFrame
--
-- The top level frame.
--
-- Note that even documents that are not framesets will have a mainFrame.
--
-- ObjC selector: @- mainFrame@
mainFrame :: IsWebView webView => webView -> IO (Id WebFrame)
mainFrame webView =
  sendMessage webView mainFrameSelector

-- | selectedFrame
--
-- The frame that has the active selection.
--
-- Returns the frame that contains the first responder, if any. Otherwise returns the    frame that contains a non-zero-length selection, if any. Returns nil if no frame meets these criteria.
--
-- ObjC selector: @- selectedFrame@
selectedFrame :: IsWebView webView => webView -> IO (Id WebFrame)
selectedFrame webView =
  sendMessage webView selectedFrameSelector

-- | backForwardList
--
-- The backforward list for this WebView.
--
-- ObjC selector: @- backForwardList@
backForwardList :: IsWebView webView => webView -> IO (Id WebBackForwardList)
backForwardList webView =
  sendMessage webView backForwardListSelector

-- | textSizeMultiplier
--
-- The text size multipler.
--
-- ObjC selector: @- textSizeMultiplier@
textSizeMultiplier :: IsWebView webView => webView -> IO CFloat
textSizeMultiplier webView =
  sendMessage webView textSizeMultiplierSelector

-- | textSizeMultiplier
--
-- The text size multipler.
--
-- ObjC selector: @- setTextSizeMultiplier:@
setTextSizeMultiplier :: IsWebView webView => webView -> CFloat -> IO ()
setTextSizeMultiplier webView value =
  sendMessage webView setTextSizeMultiplierSelector value

-- | applicationNameForUserAgent
--
-- The name of the application as used in the user-agent string.
--
-- ObjC selector: @- applicationNameForUserAgent@
applicationNameForUserAgent :: IsWebView webView => webView -> IO (Id NSString)
applicationNameForUserAgent webView =
  sendMessage webView applicationNameForUserAgentSelector

-- | applicationNameForUserAgent
--
-- The name of the application as used in the user-agent string.
--
-- ObjC selector: @- setApplicationNameForUserAgent:@
setApplicationNameForUserAgent :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setApplicationNameForUserAgent webView value =
  sendMessage webView setApplicationNameForUserAgentSelector (toNSString value)

-- | customUserAgent
--
-- The custom user-agent string or nil if no custom user-agent string has been set.
--
-- Setting this means that the webView should use this user-agent string instead of constructing a user-agent string for each URL. Setting it to nil causes the webView to construct the user-agent string for each URL for best results rendering web pages
--
-- ObjC selector: @- customUserAgent@
customUserAgent :: IsWebView webView => webView -> IO (Id NSString)
customUserAgent webView =
  sendMessage webView customUserAgentSelector

-- | customUserAgent
--
-- The custom user-agent string or nil if no custom user-agent string has been set.
--
-- Setting this means that the webView should use this user-agent string instead of constructing a user-agent string for each URL. Setting it to nil causes the webView to construct the user-agent string for each URL for best results rendering web pages
--
-- ObjC selector: @- setCustomUserAgent:@
setCustomUserAgent :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setCustomUserAgent webView value =
  sendMessage webView setCustomUserAgentSelector (toNSString value)

-- | supportsTextEncoding
--
-- If the document view of the current web page can support different text encodings.
--
-- ObjC selector: @- supportsTextEncoding@
supportsTextEncoding :: IsWebView webView => webView -> IO Bool
supportsTextEncoding webView =
  sendMessage webView supportsTextEncodingSelector

-- | customTextEncodingName
--
-- The custom text encoding name or nil if no custom text encoding name has been set.
--
-- Make the page display with a different text encoding; stops any load in progress.    The text encoding passed in overrides the normal text encoding smarts including    what's specified in a web page's header or HTTP response.    The text encoding automatically goes back to the default when the top level frame    changes to a new location.    Setting the text encoding name to nil makes the webView use default encoding rules.
--
-- ObjC selector: @- customTextEncodingName@
customTextEncodingName :: IsWebView webView => webView -> IO (Id NSString)
customTextEncodingName webView =
  sendMessage webView customTextEncodingNameSelector

-- | customTextEncodingName
--
-- The custom text encoding name or nil if no custom text encoding name has been set.
--
-- Make the page display with a different text encoding; stops any load in progress.    The text encoding passed in overrides the normal text encoding smarts including    what's specified in a web page's header or HTTP response.    The text encoding automatically goes back to the default when the top level frame    changes to a new location.    Setting the text encoding name to nil makes the webView use default encoding rules.
--
-- ObjC selector: @- setCustomTextEncodingName:@
setCustomTextEncodingName :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setCustomTextEncodingName webView value =
  sendMessage webView setCustomTextEncodingNameSelector (toNSString value)

-- | mediaStyle
--
-- The media style for the WebView.
--
-- The mediaStyle will override the normal value    of the CSS media property. Setting the value to nil will restore the normal value. The value will be nil unless explicitly set.
--
-- ObjC selector: @- mediaStyle@
mediaStyle :: IsWebView webView => webView -> IO (Id NSString)
mediaStyle webView =
  sendMessage webView mediaStyleSelector

-- | mediaStyle
--
-- The media style for the WebView.
--
-- The mediaStyle will override the normal value    of the CSS media property. Setting the value to nil will restore the normal value. The value will be nil unless explicitly set.
--
-- ObjC selector: @- setMediaStyle:@
setMediaStyle :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setMediaStyle webView value =
  sendMessage webView setMediaStyleSelector (toNSString value)

-- | windowScriptObject
--
-- A WebScriptObject that represents the    window object from the script environment.
--
-- ObjC selector: @- windowScriptObject@
windowScriptObject :: IsWebView webView => webView -> IO (Id WebScriptObject)
windowScriptObject webView =
  sendMessage webView windowScriptObjectSelector

-- | preferences
--
-- The preferences used by this WebView.
--
-- This method will return [WebPreferences standardPreferences] if no    other instance of WebPreferences has been set.
--
-- ObjC selector: @- preferences@
preferences :: IsWebView webView => webView -> IO (Id WebPreferences)
preferences webView =
  sendMessage webView preferencesSelector

-- | preferences
--
-- The preferences used by this WebView.
--
-- This method will return [WebPreferences standardPreferences] if no    other instance of WebPreferences has been set.
--
-- ObjC selector: @- setPreferences:@
setPreferences :: (IsWebView webView, IsWebPreferences value) => webView -> value -> IO ()
setPreferences webView value =
  sendMessage webView setPreferencesSelector (toWebPreferences value)

-- | preferencesIdentifier
--
-- The WebPreferences key prefix.
--
-- If the WebPreferences for this WebView are stored in the user defaults database, this string will be used as a key prefix.
--
-- ObjC selector: @- preferencesIdentifier@
preferencesIdentifier :: IsWebView webView => webView -> IO (Id NSString)
preferencesIdentifier webView =
  sendMessage webView preferencesIdentifierSelector

-- | preferencesIdentifier
--
-- The WebPreferences key prefix.
--
-- If the WebPreferences for this WebView are stored in the user defaults database, this string will be used as a key prefix.
--
-- ObjC selector: @- setPreferencesIdentifier:@
setPreferencesIdentifier :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setPreferencesIdentifier webView value =
  sendMessage webView setPreferencesIdentifierSelector (toNSString value)

-- | hostWindow
--
-- The host window for the web view.
--
-- Parts of WebKit (such as plug-ins and JavaScript) depend on a window to function    properly. Set a host window so these parts continue to function even when the web view is    not in an actual window.
--
-- ObjC selector: @- hostWindow@
hostWindow :: IsWebView webView => webView -> IO (Id NSWindow)
hostWindow webView =
  sendMessage webView hostWindowSelector

-- | hostWindow
--
-- The host window for the web view.
--
-- Parts of WebKit (such as plug-ins and JavaScript) depend on a window to function    properly. Set a host window so these parts continue to function even when the web view is    not in an actual window.
--
-- ObjC selector: @- setHostWindow:@
setHostWindow :: (IsWebView webView, IsNSWindow value) => webView -> value -> IO ()
setHostWindow webView value =
  sendMessage webView setHostWindowSelector (toNSWindow value)

-- | groupName
--
-- The group name for this WebView.
--
-- JavaScript may access named frames within the same group.
--
-- ObjC selector: @- groupName@
groupName :: IsWebView webView => webView -> IO (Id NSString)
groupName webView =
  sendMessage webView groupNameSelector

-- | groupName
--
-- The group name for this WebView.
--
-- JavaScript may access named frames within the same group.
--
-- ObjC selector: @- setGroupName:@
setGroupName :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setGroupName webView value =
  sendMessage webView setGroupNameSelector (toNSString value)

-- | estimatedProgress
--
-- An estimate of the percent complete for a document load.  This    value will range from 0 to 1.0 and, once a load completes, will remain at 1.0     until a new load starts, at which point it will be reset to 0.  The value is an    estimate based on the total number of bytes expected to be received    for a document, including all it's possible subresources.  For more accurate progress    indication it is recommended that you implement a WebFrameLoadDelegate and a    WebResourceLoadDelegate.
--
-- ObjC selector: @- estimatedProgress@
estimatedProgress :: IsWebView webView => webView -> IO CDouble
estimatedProgress webView =
  sendMessage webView estimatedProgressSelector

-- | loading
--
-- Whether there are any pending loads in this WebView.
--
-- ObjC selector: @- loading@
loading :: IsWebView webView => webView -> IO Bool
loading webView =
  sendMessage webView loadingSelector

-- | pasteboardTypesForSelection
--
-- The pasteboard types that the WebView can use for the current selection
--
-- ObjC selector: @- pasteboardTypesForSelection@
pasteboardTypesForSelection :: IsWebView webView => webView -> IO (Id NSArray)
pasteboardTypesForSelection webView =
  sendMessage webView pasteboardTypesForSelectionSelector

-- | drawsBackground
--
-- Whether the receiver draws a default white background when the loaded page has no background specified.
--
-- ObjC selector: @- drawsBackground@
drawsBackground :: IsWebView webView => webView -> IO Bool
drawsBackground webView =
  sendMessage webView drawsBackgroundSelector

-- | drawsBackground
--
-- Whether the receiver draws a default white background when the loaded page has no background specified.
--
-- ObjC selector: @- setDrawsBackground:@
setDrawsBackground :: IsWebView webView => webView -> Bool -> IO ()
setDrawsBackground webView value =
  sendMessage webView setDrawsBackgroundSelector value

-- | shouldUpdateWhileOffscreen
--
-- Whether the WebView is always updated even when it is not in a window that is currently visible.
--
-- If set to NO, then whenever the web view is not in a visible window, updates to the web page will not necessarily be rendered in the view.    However, when the window is made visible, the view will be updated automatically. Not updating while hidden can improve performance. If set to is YES,    hidden web views are always updated. This is the default.
--
-- ObjC selector: @- shouldUpdateWhileOffscreen@
shouldUpdateWhileOffscreen :: IsWebView webView => webView -> IO Bool
shouldUpdateWhileOffscreen webView =
  sendMessage webView shouldUpdateWhileOffscreenSelector

-- | shouldUpdateWhileOffscreen
--
-- Whether the WebView is always updated even when it is not in a window that is currently visible.
--
-- If set to NO, then whenever the web view is not in a visible window, updates to the web page will not necessarily be rendered in the view.    However, when the window is made visible, the view will be updated automatically. Not updating while hidden can improve performance. If set to is YES,    hidden web views are always updated. This is the default.
--
-- ObjC selector: @- setShouldUpdateWhileOffscreen:@
setShouldUpdateWhileOffscreen :: IsWebView webView => webView -> Bool -> IO ()
setShouldUpdateWhileOffscreen webView value =
  sendMessage webView setShouldUpdateWhileOffscreenSelector value

-- | mainFrameURL
--
-- The main frame's current URL.
--
-- ObjC selector: @- mainFrameURL@
mainFrameURL :: IsWebView webView => webView -> IO (Id NSString)
mainFrameURL webView =
  sendMessage webView mainFrameURLSelector

-- | mainFrameURL
--
-- The main frame's current URL.
--
-- ObjC selector: @- setMainFrameURL:@
setMainFrameURL :: (IsWebView webView, IsNSString value) => webView -> value -> IO ()
setMainFrameURL webView value =
  sendMessage webView setMainFrameURLSelector (toNSString value)

-- | mainFrameDocument
--
-- The main frame's DOMDocument.
--
-- ObjC selector: @- mainFrameDocument@
mainFrameDocument :: IsWebView webView => webView -> IO (Id DOMDocument)
mainFrameDocument webView =
  sendMessage webView mainFrameDocumentSelector

-- | mainFrameTitle
--
-- The main frame's title if any, otherwise an empty string.
--
-- ObjC selector: @- mainFrameTitle@
mainFrameTitle :: IsWebView webView => webView -> IO (Id NSString)
mainFrameTitle webView =
  sendMessage webView mainFrameTitleSelector

-- | mainFrameIcon
--
-- The site icon for the current page loaded in the mainFrame, or nil.
--
-- ObjC selector: @- mainFrameIcon@
mainFrameIcon :: IsWebView webView => webView -> IO (Id NSImage)
mainFrameIcon webView =
  sendMessage webView mainFrameIconSelector

-- | @- selectedDOMRange@
selectedDOMRange :: IsWebView webView => webView -> IO (Id DOMRange)
selectedDOMRange webView =
  sendMessage webView selectedDOMRangeSelector

-- | @- selectionAffinity@
selectionAffinity :: IsWebView webView => webView -> IO NSSelectionAffinity
selectionAffinity webView =
  sendMessage webView selectionAffinitySelector

-- | @- maintainsInactiveSelection@
maintainsInactiveSelection :: IsWebView webView => webView -> IO Bool
maintainsInactiveSelection webView =
  sendMessage webView maintainsInactiveSelectionSelector

-- | @- editable@
editable :: IsWebView webView => webView -> IO Bool
editable webView =
  sendMessage webView editableSelector

-- | @- setEditable:@
setEditable :: IsWebView webView => webView -> Bool -> IO ()
setEditable webView value =
  sendMessage webView setEditableSelector value

-- | @- typingStyle@
typingStyle :: IsWebView webView => webView -> IO (Id DOMCSSStyleDeclaration)
typingStyle webView =
  sendMessage webView typingStyleSelector

-- | @- setTypingStyle:@
setTypingStyle :: (IsWebView webView, IsDOMCSSStyleDeclaration value) => webView -> value -> IO ()
setTypingStyle webView value =
  sendMessage webView setTypingStyleSelector (toDOMCSSStyleDeclaration value)

-- | @- smartInsertDeleteEnabled@
smartInsertDeleteEnabled :: IsWebView webView => webView -> IO Bool
smartInsertDeleteEnabled webView =
  sendMessage webView smartInsertDeleteEnabledSelector

-- | @- setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabled :: IsWebView webView => webView -> Bool -> IO ()
setSmartInsertDeleteEnabled webView value =
  sendMessage webView setSmartInsertDeleteEnabledSelector value

-- | @- continuousSpellCheckingEnabled@
continuousSpellCheckingEnabled :: IsWebView webView => webView -> IO Bool
continuousSpellCheckingEnabled webView =
  sendMessage webView continuousSpellCheckingEnabledSelector

-- | @- setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabled :: IsWebView webView => webView -> Bool -> IO ()
setContinuousSpellCheckingEnabled webView value =
  sendMessage webView setContinuousSpellCheckingEnabledSelector value

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsWebView webView => webView -> IO CLong
spellCheckerDocumentTag webView =
  sendMessage webView spellCheckerDocumentTagSelector

-- | @- undoManager@
undoManager :: IsWebView webView => webView -> IO (Id NSUndoManager)
undoManager webView =
  sendMessage webView undoManagerSelector

-- | @- editingDelegate@
editingDelegate :: IsWebView webView => webView -> IO RawId
editingDelegate webView =
  sendMessage webView editingDelegateSelector

-- | @- setEditingDelegate:@
setEditingDelegate :: IsWebView webView => webView -> RawId -> IO ()
setEditingDelegate webView value =
  sendMessage webView setEditingDelegateSelector value

-- | @- canGoBack@
canGoBack :: IsWebView webView => webView -> IO Bool
canGoBack webView =
  sendMessage webView canGoBackSelector

-- | @- canGoForward@
canGoForward :: IsWebView webView => webView -> IO Bool
canGoForward webView =
  sendMessage webView canGoForwardSelector

-- | @- canMakeTextLarger@
canMakeTextLarger :: IsWebView webView => webView -> IO Bool
canMakeTextLarger webView =
  sendMessage webView canMakeTextLargerSelector

-- | @- canMakeTextSmaller@
canMakeTextSmaller :: IsWebView webView => webView -> IO Bool
canMakeTextSmaller webView =
  sendMessage webView canMakeTextSmallerSelector

-- | @- canMakeTextStandardSize@
canMakeTextStandardSize :: IsWebView webView => webView -> IO Bool
canMakeTextStandardSize webView =
  sendMessage webView canMakeTextStandardSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canShowMIMEType:@
canShowMIMETypeSelector :: Selector '[Id NSString] Bool
canShowMIMETypeSelector = mkSelector "canShowMIMEType:"

-- | @Selector@ for @canShowMIMETypeAsHTML:@
canShowMIMETypeAsHTMLSelector :: Selector '[Id NSString] Bool
canShowMIMETypeAsHTMLSelector = mkSelector "canShowMIMETypeAsHTML:"

-- | @Selector@ for @MIMETypesShownAsHTML@
mimeTypesShownAsHTMLSelector :: Selector '[] (Id NSArray)
mimeTypesShownAsHTMLSelector = mkSelector "MIMETypesShownAsHTML"

-- | @Selector@ for @setMIMETypesShownAsHTML:@
setMIMETypesShownAsHTMLSelector :: Selector '[Id NSArray] ()
setMIMETypesShownAsHTMLSelector = mkSelector "setMIMETypesShownAsHTML:"

-- | @Selector@ for @URLFromPasteboard:@
urlFromPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSURL)
urlFromPasteboardSelector = mkSelector "URLFromPasteboard:"

-- | @Selector@ for @URLTitleFromPasteboard:@
urlTitleFromPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSString)
urlTitleFromPasteboardSelector = mkSelector "URLTitleFromPasteboard:"

-- | @Selector@ for @registerURLSchemeAsLocal:@
registerURLSchemeAsLocalSelector :: Selector '[Id NSString] ()
registerURLSchemeAsLocalSelector = mkSelector "registerURLSchemeAsLocal:"

-- | @Selector@ for @initWithFrame:frameName:groupName:@
initWithFrame_frameName_groupNameSelector :: Selector '[NSRect, Id NSString, Id NSString] (Id WebView)
initWithFrame_frameName_groupNameSelector = mkSelector "initWithFrame:frameName:groupName:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @setMaintainsBackForwardList:@
setMaintainsBackForwardListSelector :: Selector '[Bool] ()
setMaintainsBackForwardListSelector = mkSelector "setMaintainsBackForwardList:"

-- | @Selector@ for @goBack@
goBackSelector :: Selector '[] Bool
goBackSelector = mkSelector "goBack"

-- | @Selector@ for @goForward@
goForwardSelector :: Selector '[] Bool
goForwardSelector = mkSelector "goForward"

-- | @Selector@ for @goToBackForwardItem:@
goToBackForwardItemSelector :: Selector '[Id WebHistoryItem] Bool
goToBackForwardItemSelector = mkSelector "goToBackForwardItem:"

-- | @Selector@ for @userAgentForURL:@
userAgentForURLSelector :: Selector '[Id NSURL] (Id NSString)
userAgentForURLSelector = mkSelector "userAgentForURL:"

-- | @Selector@ for @stringByEvaluatingJavaScriptFromString:@
stringByEvaluatingJavaScriptFromStringSelector :: Selector '[Id NSString] (Id NSString)
stringByEvaluatingJavaScriptFromStringSelector = mkSelector "stringByEvaluatingJavaScriptFromString:"

-- | @Selector@ for @searchFor:direction:caseSensitive:wrap:@
searchFor_direction_caseSensitive_wrapSelector :: Selector '[Id NSString, Bool, Bool, Bool] Bool
searchFor_direction_caseSensitive_wrapSelector = mkSelector "searchFor:direction:caseSensitive:wrap:"

-- | @Selector@ for @registerViewClass:representationClass:forMIMEType:@
registerViewClass_representationClass_forMIMETypeSelector :: Selector '[Class, Class, Id NSString] ()
registerViewClass_representationClass_forMIMETypeSelector = mkSelector "registerViewClass:representationClass:forMIMEType:"

-- | @Selector@ for @elementAtPoint:@
elementAtPointSelector :: Selector '[NSPoint] (Id NSDictionary)
elementAtPointSelector = mkSelector "elementAtPoint:"

-- | @Selector@ for @writeSelectionWithPasteboardTypes:toPasteboard:@
writeSelectionWithPasteboardTypes_toPasteboardSelector :: Selector '[Id NSArray, Id NSPasteboard] ()
writeSelectionWithPasteboardTypes_toPasteboardSelector = mkSelector "writeSelectionWithPasteboardTypes:toPasteboard:"

-- | @Selector@ for @pasteboardTypesForElement:@
pasteboardTypesForElementSelector :: Selector '[Id NSDictionary] (Id NSArray)
pasteboardTypesForElementSelector = mkSelector "pasteboardTypesForElement:"

-- | @Selector@ for @writeElement:withPasteboardTypes:toPasteboard:@
writeElement_withPasteboardTypes_toPasteboardSelector :: Selector '[Id NSDictionary, Id NSArray, Id NSPasteboard] ()
writeElement_withPasteboardTypes_toPasteboardSelector = mkSelector "writeElement:withPasteboardTypes:toPasteboard:"

-- | @Selector@ for @moveDragCaretToPoint:@
moveDragCaretToPointSelector :: Selector '[NSPoint] ()
moveDragCaretToPointSelector = mkSelector "moveDragCaretToPoint:"

-- | @Selector@ for @removeDragCaret@
removeDragCaretSelector :: Selector '[] ()
removeDragCaretSelector = mkSelector "removeDragCaret"

-- | @Selector@ for @copy:@
copySelector :: Selector '[RawId] ()
copySelector = mkSelector "copy:"

-- | @Selector@ for @cut:@
cutSelector :: Selector '[RawId] ()
cutSelector = mkSelector "cut:"

-- | @Selector@ for @paste:@
pasteSelector :: Selector '[RawId] ()
pasteSelector = mkSelector "paste:"

-- | @Selector@ for @copyFont:@
copyFontSelector :: Selector '[RawId] ()
copyFontSelector = mkSelector "copyFont:"

-- | @Selector@ for @pasteFont:@
pasteFontSelector :: Selector '[RawId] ()
pasteFontSelector = mkSelector "pasteFont:"

-- | @Selector@ for @delete:@
deleteSelector :: Selector '[RawId] ()
deleteSelector = mkSelector "delete:"

-- | @Selector@ for @pasteAsPlainText:@
pasteAsPlainTextSelector :: Selector '[RawId] ()
pasteAsPlainTextSelector = mkSelector "pasteAsPlainText:"

-- | @Selector@ for @pasteAsRichText:@
pasteAsRichTextSelector :: Selector '[RawId] ()
pasteAsRichTextSelector = mkSelector "pasteAsRichText:"

-- | @Selector@ for @changeFont:@
changeFontSelector :: Selector '[RawId] ()
changeFontSelector = mkSelector "changeFont:"

-- | @Selector@ for @changeAttributes:@
changeAttributesSelector :: Selector '[RawId] ()
changeAttributesSelector = mkSelector "changeAttributes:"

-- | @Selector@ for @changeDocumentBackgroundColor:@
changeDocumentBackgroundColorSelector :: Selector '[RawId] ()
changeDocumentBackgroundColorSelector = mkSelector "changeDocumentBackgroundColor:"

-- | @Selector@ for @changeColor:@
changeColorSelector :: Selector '[RawId] ()
changeColorSelector = mkSelector "changeColor:"

-- | @Selector@ for @alignCenter:@
alignCenterSelector :: Selector '[RawId] ()
alignCenterSelector = mkSelector "alignCenter:"

-- | @Selector@ for @alignJustified:@
alignJustifiedSelector :: Selector '[RawId] ()
alignJustifiedSelector = mkSelector "alignJustified:"

-- | @Selector@ for @alignLeft:@
alignLeftSelector :: Selector '[RawId] ()
alignLeftSelector = mkSelector "alignLeft:"

-- | @Selector@ for @alignRight:@
alignRightSelector :: Selector '[RawId] ()
alignRightSelector = mkSelector "alignRight:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector '[RawId] ()
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector '[RawId] ()
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @performFindPanelAction:@
performFindPanelActionSelector :: Selector '[RawId] ()
performFindPanelActionSelector = mkSelector "performFindPanelAction:"

-- | @Selector@ for @startSpeaking:@
startSpeakingSelector :: Selector '[RawId] ()
startSpeakingSelector = mkSelector "startSpeaking:"

-- | @Selector@ for @stopSpeaking:@
stopSpeakingSelector :: Selector '[RawId] ()
stopSpeakingSelector = mkSelector "stopSpeaking:"

-- | @Selector@ for @moveToBeginningOfSentence:@
moveToBeginningOfSentenceSelector :: Selector '[RawId] ()
moveToBeginningOfSentenceSelector = mkSelector "moveToBeginningOfSentence:"

-- | @Selector@ for @moveToBeginningOfSentenceAndModifySelection:@
moveToBeginningOfSentenceAndModifySelectionSelector :: Selector '[RawId] ()
moveToBeginningOfSentenceAndModifySelectionSelector = mkSelector "moveToBeginningOfSentenceAndModifySelection:"

-- | @Selector@ for @moveToEndOfSentence:@
moveToEndOfSentenceSelector :: Selector '[RawId] ()
moveToEndOfSentenceSelector = mkSelector "moveToEndOfSentence:"

-- | @Selector@ for @moveToEndOfSentenceAndModifySelection:@
moveToEndOfSentenceAndModifySelectionSelector :: Selector '[RawId] ()
moveToEndOfSentenceAndModifySelectionSelector = mkSelector "moveToEndOfSentenceAndModifySelection:"

-- | @Selector@ for @selectSentence:@
selectSentenceSelector :: Selector '[RawId] ()
selectSentenceSelector = mkSelector "selectSentence:"

-- | @Selector@ for @overWrite:@
overWriteSelector :: Selector '[RawId] ()
overWriteSelector = mkSelector "overWrite:"

-- | @Selector@ for @replaceSelectionWithNode:@
replaceSelectionWithNodeSelector :: Selector '[Id DOMNode] ()
replaceSelectionWithNodeSelector = mkSelector "replaceSelectionWithNode:"

-- | @Selector@ for @replaceSelectionWithText:@
replaceSelectionWithTextSelector :: Selector '[Id NSString] ()
replaceSelectionWithTextSelector = mkSelector "replaceSelectionWithText:"

-- | @Selector@ for @replaceSelectionWithMarkupString:@
replaceSelectionWithMarkupStringSelector :: Selector '[Id NSString] ()
replaceSelectionWithMarkupStringSelector = mkSelector "replaceSelectionWithMarkupString:"

-- | @Selector@ for @replaceSelectionWithArchive:@
replaceSelectionWithArchiveSelector :: Selector '[Id WebArchive] ()
replaceSelectionWithArchiveSelector = mkSelector "replaceSelectionWithArchive:"

-- | @Selector@ for @deleteSelection@
deleteSelectionSelector :: Selector '[] ()
deleteSelectionSelector = mkSelector "deleteSelection"

-- | @Selector@ for @applyStyle:@
applyStyleSelector :: Selector '[Id DOMCSSStyleDeclaration] ()
applyStyleSelector = mkSelector "applyStyle:"

-- | @Selector@ for @editableDOMRangeForPoint:@
editableDOMRangeForPointSelector :: Selector '[NSPoint] (Id DOMRange)
editableDOMRangeForPointSelector = mkSelector "editableDOMRangeForPoint:"

-- | @Selector@ for @setSelectedDOMRange:affinity:@
setSelectedDOMRange_affinitySelector :: Selector '[Id DOMRange, NSSelectionAffinity] ()
setSelectedDOMRange_affinitySelector = mkSelector "setSelectedDOMRange:affinity:"

-- | @Selector@ for @styleDeclarationWithText:@
styleDeclarationWithTextSelector :: Selector '[Id NSString] (Id DOMCSSStyleDeclaration)
styleDeclarationWithTextSelector = mkSelector "styleDeclarationWithText:"

-- | @Selector@ for @computedStyleForElement:pseudoElement:@
computedStyleForElement_pseudoElementSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSStyleDeclaration)
computedStyleForElement_pseudoElementSelector = mkSelector "computedStyleForElement:pseudoElement:"

-- | @Selector@ for @takeStringURLFrom:@
takeStringURLFromSelector :: Selector '[RawId] ()
takeStringURLFromSelector = mkSelector "takeStringURLFrom:"

-- | @Selector@ for @stopLoading:@
stopLoadingSelector :: Selector '[RawId] ()
stopLoadingSelector = mkSelector "stopLoading:"

-- | @Selector@ for @reload:@
reloadSelector :: Selector '[RawId] ()
reloadSelector = mkSelector "reload:"

-- | @Selector@ for @reloadFromOrigin:@
reloadFromOriginSelector :: Selector '[RawId] ()
reloadFromOriginSelector = mkSelector "reloadFromOrigin:"

-- | @Selector@ for @makeTextLarger:@
makeTextLargerSelector :: Selector '[RawId] ()
makeTextLargerSelector = mkSelector "makeTextLarger:"

-- | @Selector@ for @makeTextSmaller:@
makeTextSmallerSelector :: Selector '[RawId] ()
makeTextSmallerSelector = mkSelector "makeTextSmaller:"

-- | @Selector@ for @makeTextStandardSize:@
makeTextStandardSizeSelector :: Selector '[RawId] ()
makeTextStandardSizeSelector = mkSelector "makeTextStandardSize:"

-- | @Selector@ for @toggleContinuousSpellChecking:@
toggleContinuousSpellCheckingSelector :: Selector '[RawId] ()
toggleContinuousSpellCheckingSelector = mkSelector "toggleContinuousSpellChecking:"

-- | @Selector@ for @toggleSmartInsertDelete:@
toggleSmartInsertDeleteSelector :: Selector '[RawId] ()
toggleSmartInsertDeleteSelector = mkSelector "toggleSmartInsertDelete:"

-- | @Selector@ for @shouldCloseWithWindow@
shouldCloseWithWindowSelector :: Selector '[] Bool
shouldCloseWithWindowSelector = mkSelector "shouldCloseWithWindow"

-- | @Selector@ for @setShouldCloseWithWindow:@
setShouldCloseWithWindowSelector :: Selector '[Bool] ()
setShouldCloseWithWindowSelector = mkSelector "setShouldCloseWithWindow:"

-- | @Selector@ for @UIDelegate@
uiDelegateSelector :: Selector '[] RawId
uiDelegateSelector = mkSelector "UIDelegate"

-- | @Selector@ for @setUIDelegate:@
setUIDelegateSelector :: Selector '[RawId] ()
setUIDelegateSelector = mkSelector "setUIDelegate:"

-- | @Selector@ for @resourceLoadDelegate@
resourceLoadDelegateSelector :: Selector '[] RawId
resourceLoadDelegateSelector = mkSelector "resourceLoadDelegate"

-- | @Selector@ for @setResourceLoadDelegate:@
setResourceLoadDelegateSelector :: Selector '[RawId] ()
setResourceLoadDelegateSelector = mkSelector "setResourceLoadDelegate:"

-- | @Selector@ for @downloadDelegate@
downloadDelegateSelector :: Selector '[] RawId
downloadDelegateSelector = mkSelector "downloadDelegate"

-- | @Selector@ for @setDownloadDelegate:@
setDownloadDelegateSelector :: Selector '[RawId] ()
setDownloadDelegateSelector = mkSelector "setDownloadDelegate:"

-- | @Selector@ for @frameLoadDelegate@
frameLoadDelegateSelector :: Selector '[] RawId
frameLoadDelegateSelector = mkSelector "frameLoadDelegate"

-- | @Selector@ for @setFrameLoadDelegate:@
setFrameLoadDelegateSelector :: Selector '[RawId] ()
setFrameLoadDelegateSelector = mkSelector "setFrameLoadDelegate:"

-- | @Selector@ for @policyDelegate@
policyDelegateSelector :: Selector '[] RawId
policyDelegateSelector = mkSelector "policyDelegate"

-- | @Selector@ for @setPolicyDelegate:@
setPolicyDelegateSelector :: Selector '[RawId] ()
setPolicyDelegateSelector = mkSelector "setPolicyDelegate:"

-- | @Selector@ for @mainFrame@
mainFrameSelector :: Selector '[] (Id WebFrame)
mainFrameSelector = mkSelector "mainFrame"

-- | @Selector@ for @selectedFrame@
selectedFrameSelector :: Selector '[] (Id WebFrame)
selectedFrameSelector = mkSelector "selectedFrame"

-- | @Selector@ for @backForwardList@
backForwardListSelector :: Selector '[] (Id WebBackForwardList)
backForwardListSelector = mkSelector "backForwardList"

-- | @Selector@ for @textSizeMultiplier@
textSizeMultiplierSelector :: Selector '[] CFloat
textSizeMultiplierSelector = mkSelector "textSizeMultiplier"

-- | @Selector@ for @setTextSizeMultiplier:@
setTextSizeMultiplierSelector :: Selector '[CFloat] ()
setTextSizeMultiplierSelector = mkSelector "setTextSizeMultiplier:"

-- | @Selector@ for @applicationNameForUserAgent@
applicationNameForUserAgentSelector :: Selector '[] (Id NSString)
applicationNameForUserAgentSelector = mkSelector "applicationNameForUserAgent"

-- | @Selector@ for @setApplicationNameForUserAgent:@
setApplicationNameForUserAgentSelector :: Selector '[Id NSString] ()
setApplicationNameForUserAgentSelector = mkSelector "setApplicationNameForUserAgent:"

-- | @Selector@ for @customUserAgent@
customUserAgentSelector :: Selector '[] (Id NSString)
customUserAgentSelector = mkSelector "customUserAgent"

-- | @Selector@ for @setCustomUserAgent:@
setCustomUserAgentSelector :: Selector '[Id NSString] ()
setCustomUserAgentSelector = mkSelector "setCustomUserAgent:"

-- | @Selector@ for @supportsTextEncoding@
supportsTextEncodingSelector :: Selector '[] Bool
supportsTextEncodingSelector = mkSelector "supportsTextEncoding"

-- | @Selector@ for @customTextEncodingName@
customTextEncodingNameSelector :: Selector '[] (Id NSString)
customTextEncodingNameSelector = mkSelector "customTextEncodingName"

-- | @Selector@ for @setCustomTextEncodingName:@
setCustomTextEncodingNameSelector :: Selector '[Id NSString] ()
setCustomTextEncodingNameSelector = mkSelector "setCustomTextEncodingName:"

-- | @Selector@ for @mediaStyle@
mediaStyleSelector :: Selector '[] (Id NSString)
mediaStyleSelector = mkSelector "mediaStyle"

-- | @Selector@ for @setMediaStyle:@
setMediaStyleSelector :: Selector '[Id NSString] ()
setMediaStyleSelector = mkSelector "setMediaStyle:"

-- | @Selector@ for @windowScriptObject@
windowScriptObjectSelector :: Selector '[] (Id WebScriptObject)
windowScriptObjectSelector = mkSelector "windowScriptObject"

-- | @Selector@ for @preferences@
preferencesSelector :: Selector '[] (Id WebPreferences)
preferencesSelector = mkSelector "preferences"

-- | @Selector@ for @setPreferences:@
setPreferencesSelector :: Selector '[Id WebPreferences] ()
setPreferencesSelector = mkSelector "setPreferences:"

-- | @Selector@ for @preferencesIdentifier@
preferencesIdentifierSelector :: Selector '[] (Id NSString)
preferencesIdentifierSelector = mkSelector "preferencesIdentifier"

-- | @Selector@ for @setPreferencesIdentifier:@
setPreferencesIdentifierSelector :: Selector '[Id NSString] ()
setPreferencesIdentifierSelector = mkSelector "setPreferencesIdentifier:"

-- | @Selector@ for @hostWindow@
hostWindowSelector :: Selector '[] (Id NSWindow)
hostWindowSelector = mkSelector "hostWindow"

-- | @Selector@ for @setHostWindow:@
setHostWindowSelector :: Selector '[Id NSWindow] ()
setHostWindowSelector = mkSelector "setHostWindow:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id NSString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector '[Id NSString] ()
setGroupNameSelector = mkSelector "setGroupName:"

-- | @Selector@ for @estimatedProgress@
estimatedProgressSelector :: Selector '[] CDouble
estimatedProgressSelector = mkSelector "estimatedProgress"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

-- | @Selector@ for @pasteboardTypesForSelection@
pasteboardTypesForSelectionSelector :: Selector '[] (Id NSArray)
pasteboardTypesForSelectionSelector = mkSelector "pasteboardTypesForSelection"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @shouldUpdateWhileOffscreen@
shouldUpdateWhileOffscreenSelector :: Selector '[] Bool
shouldUpdateWhileOffscreenSelector = mkSelector "shouldUpdateWhileOffscreen"

-- | @Selector@ for @setShouldUpdateWhileOffscreen:@
setShouldUpdateWhileOffscreenSelector :: Selector '[Bool] ()
setShouldUpdateWhileOffscreenSelector = mkSelector "setShouldUpdateWhileOffscreen:"

-- | @Selector@ for @mainFrameURL@
mainFrameURLSelector :: Selector '[] (Id NSString)
mainFrameURLSelector = mkSelector "mainFrameURL"

-- | @Selector@ for @setMainFrameURL:@
setMainFrameURLSelector :: Selector '[Id NSString] ()
setMainFrameURLSelector = mkSelector "setMainFrameURL:"

-- | @Selector@ for @mainFrameDocument@
mainFrameDocumentSelector :: Selector '[] (Id DOMDocument)
mainFrameDocumentSelector = mkSelector "mainFrameDocument"

-- | @Selector@ for @mainFrameTitle@
mainFrameTitleSelector :: Selector '[] (Id NSString)
mainFrameTitleSelector = mkSelector "mainFrameTitle"

-- | @Selector@ for @mainFrameIcon@
mainFrameIconSelector :: Selector '[] (Id NSImage)
mainFrameIconSelector = mkSelector "mainFrameIcon"

-- | @Selector@ for @selectedDOMRange@
selectedDOMRangeSelector :: Selector '[] (Id DOMRange)
selectedDOMRangeSelector = mkSelector "selectedDOMRange"

-- | @Selector@ for @selectionAffinity@
selectionAffinitySelector :: Selector '[] NSSelectionAffinity
selectionAffinitySelector = mkSelector "selectionAffinity"

-- | @Selector@ for @maintainsInactiveSelection@
maintainsInactiveSelectionSelector :: Selector '[] Bool
maintainsInactiveSelectionSelector = mkSelector "maintainsInactiveSelection"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @typingStyle@
typingStyleSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
typingStyleSelector = mkSelector "typingStyle"

-- | @Selector@ for @setTypingStyle:@
setTypingStyleSelector :: Selector '[Id DOMCSSStyleDeclaration] ()
setTypingStyleSelector = mkSelector "setTypingStyle:"

-- | @Selector@ for @smartInsertDeleteEnabled@
smartInsertDeleteEnabledSelector :: Selector '[] Bool
smartInsertDeleteEnabledSelector = mkSelector "smartInsertDeleteEnabled"

-- | @Selector@ for @setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabledSelector :: Selector '[Bool] ()
setSmartInsertDeleteEnabledSelector = mkSelector "setSmartInsertDeleteEnabled:"

-- | @Selector@ for @continuousSpellCheckingEnabled@
continuousSpellCheckingEnabledSelector :: Selector '[] Bool
continuousSpellCheckingEnabledSelector = mkSelector "continuousSpellCheckingEnabled"

-- | @Selector@ for @setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabledSelector :: Selector '[Bool] ()
setContinuousSpellCheckingEnabledSelector = mkSelector "setContinuousSpellCheckingEnabled:"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector '[] CLong
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector '[] (Id NSUndoManager)
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @editingDelegate@
editingDelegateSelector :: Selector '[] RawId
editingDelegateSelector = mkSelector "editingDelegate"

-- | @Selector@ for @setEditingDelegate:@
setEditingDelegateSelector :: Selector '[RawId] ()
setEditingDelegateSelector = mkSelector "setEditingDelegate:"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector '[] Bool
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector '[] Bool
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @canMakeTextLarger@
canMakeTextLargerSelector :: Selector '[] Bool
canMakeTextLargerSelector = mkSelector "canMakeTextLarger"

-- | @Selector@ for @canMakeTextSmaller@
canMakeTextSmallerSelector :: Selector '[] Bool
canMakeTextSmallerSelector = mkSelector "canMakeTextSmaller"

-- | @Selector@ for @canMakeTextStandardSize@
canMakeTextStandardSizeSelector :: Selector '[] Bool
canMakeTextStandardSizeSelector = mkSelector "canMakeTextStandardSize"

