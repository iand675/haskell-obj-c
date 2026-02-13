{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IMKCandidates@.
module ObjC.InputMethodKit.IMKCandidates
  ( IMKCandidates
  , IsIMKCandidates(..)
  , initWithServer_panelType
  , initWithServer_panelType_styleType
  , panelType
  , setPanelType
  , show_
  , hide
  , isVisible
  , updateCandidates
  , showAnnotation
  , showSublist_subListDelegate
  , candidateFrame
  , setSelectionKeys
  , selectionKeys
  , setSelectionKeysKeylayout
  , selectionKeysKeylayout
  , setAttributes
  , attributes
  , setDismissesAutomatically
  , dismissesAutomatically
  , selectedCandidate
  , setCandidateFrameTopLeft
  , showChild
  , hideChild
  , attachChild_toCandidate_type
  , detachChild
  , setCandidateData
  , selectCandidateWithIdentifier
  , selectCandidate
  , showCandidates
  , candidateStringIdentifier
  , selectedCandidateString
  , candidateIdentifierAtLineNumber
  , lineNumberForCandidateWithIdentifier
  , clearSelection
  , attachChild_toCandidate_typeSelector
  , attributesSelector
  , candidateFrameSelector
  , candidateIdentifierAtLineNumberSelector
  , candidateStringIdentifierSelector
  , clearSelectionSelector
  , detachChildSelector
  , dismissesAutomaticallySelector
  , hideChildSelector
  , hideSelector
  , initWithServer_panelTypeSelector
  , initWithServer_panelType_styleTypeSelector
  , isVisibleSelector
  , lineNumberForCandidateWithIdentifierSelector
  , panelTypeSelector
  , selectCandidateSelector
  , selectCandidateWithIdentifierSelector
  , selectedCandidateSelector
  , selectedCandidateStringSelector
  , selectionKeysKeylayoutSelector
  , selectionKeysSelector
  , setAttributesSelector
  , setCandidateDataSelector
  , setCandidateFrameTopLeftSelector
  , setDismissesAutomaticallySelector
  , setPanelTypeSelector
  , setSelectionKeysKeylayoutSelector
  , setSelectionKeysSelector
  , showAnnotationSelector
  , showCandidatesSelector
  , showChildSelector
  , showSelector
  , showSublist_subListDelegateSelector
  , updateCandidatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.InputMethodKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Default initializer for the class.
--
-- When an input method allocates an IMKCandidate object it should initialize that object by calling this method passing the IMKServer that will manage the candidates and the initial panel type.
--
-- ObjC selector: @- initWithServer:panelType:@
initWithServer_panelType :: (IsIMKCandidates imkCandidates, IsIMKServer server) => imkCandidates -> server -> CULong -> IO RawId
initWithServer_panelType imkCandidates server panelType =
  sendOwnedMessage imkCandidates initWithServer_panelTypeSelector (toIMKServer server) panelType

-- | @- initWithServer:panelType:styleType:@
initWithServer_panelType_styleType :: (IsIMKCandidates imkCandidates, IsIMKServer server) => imkCandidates -> server -> CULong -> CULong -> IO RawId
initWithServer_panelType_styleType imkCandidates server panelType style =
  sendOwnedMessage imkCandidates initWithServer_panelType_styleTypeSelector (toIMKServer server) panelType style

-- | Return the panel type.
--
-- ObjC selector: @- panelType@
panelType :: IsIMKCandidates imkCandidates => imkCandidates -> IO CULong
panelType imkCandidates =
  sendMessage imkCandidates panelTypeSelector

-- | Change the panel type.
--
-- ObjC selector: @- setPanelType:@
setPanelType :: IsIMKCandidates imkCandidates => imkCandidates -> CULong -> IO ()
setPanelType imkCandidates panelType =
  sendMessage imkCandidates setPanelTypeSelector panelType

-- | If a candidate window type has been provided, show the candidate window. The caller provides a location hint that is used to position the window.
--
-- Input methods call this method when it is appropriate, during text conversion, to display a list of candidates.
--
-- ObjC selector: @- show:@
show_ :: IsIMKCandidates imkCandidates => imkCandidates -> CULong -> IO ()
show_ imkCandidates locationHint =
  sendMessage imkCandidates showSelector locationHint

-- | If the candidate window is visible, hide it.
--
-- ObjC selector: @- hide@
hide :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
hide imkCandidates =
  sendMessage imkCandidates hideSelector

-- | Utility method returns YES if a candidate display is visible.
--
-- ObjC selector: @- isVisible@
isVisible :: IsIMKCandidates imkCandidates => imkCandidates -> IO Bool
isVisible imkCandidates =
  sendMessage imkCandidates isVisibleSelector

-- | Call this method to update the candidates displayed in the candidate window.
--
-- Calling this method will result in a call being made to the IMKInputController's candidates method. Note that the candidate list will be updated, but the window's visible state will not change; that is to say, if the window is hidden it will remain hidden, and vice versa.
--
-- ObjC selector: @- updateCandidates@
updateCandidates :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
updateCandidates imkCandidates =
  sendMessage imkCandidates updateCandidatesSelector

-- | Displays an annotation window whose contents are the annotationString.
--
-- An annotation is additional text that explains or somehow adds to the candidate string in a candidate window. Annotations are displayed in a small borderless window that is aligned with the current candidate panel. An input method calls showAnnotation: when the method [IMKInputController candidateSelectionChanged:] is called, and the candidateString has annotations.
--
-- ObjC selector: @- showAnnotation:@
showAnnotation :: (IsIMKCandidates imkCandidates, IsNSAttributedString annotationString) => imkCandidates -> annotationString -> IO ()
showAnnotation imkCandidates annotationString =
  sendMessage imkCandidates showAnnotationSelector (toNSAttributedString annotationString)

-- | @- showSublist:subListDelegate:@
showSublist_subListDelegate :: (IsIMKCandidates imkCandidates, IsNSArray candidates) => imkCandidates -> candidates -> RawId -> IO ()
showSublist_subListDelegate imkCandidates candidates delegate =
  sendMessage imkCandidates showSublist_subListDelegateSelector (toNSArray candidates) delegate

-- | @- candidateFrame@
candidateFrame :: IsIMKCandidates imkCandidates => imkCandidates -> IO NSRect
candidateFrame imkCandidates =
  sendMessage imkCandidates candidateFrameSelector

-- | Set the selection keys for the candidates.
--
-- Selection keys are an array of NSNumbers where each NSNumber is a virtual key code that the controller will map to characters that are displayed either across the top of the candidates, if the candidates are laid out horizontally, or along the left edge of the candidates, if they are aligned vertically.
--
-- The number of selection keys determines how many candidates are displayed per page.  For example, if you passed an array of 4 key codes, then 4 candidates are displayed per page.  If you passed 11 key codes, then 11 candidates would be displayed.
--
-- By default the key codes are mapped using the keyboard layout whose source id is com.apple.keylayout.US.  The default layout can be replaced by calling setSelectionKeysKeylayout (see below).
--
-- The default selection keys are the digits 1 through 9, or in terms of key codes: 18-21,23,22, 26, 28, 25.
--
-- ObjC selector: @- setSelectionKeys:@
setSelectionKeys :: (IsIMKCandidates imkCandidates, IsNSArray keyCodes) => imkCandidates -> keyCodes -> IO ()
setSelectionKeys imkCandidates keyCodes =
  sendMessage imkCandidates setSelectionKeysSelector (toNSArray keyCodes)

-- | Returns an NSArray of NSNumbers where each NSNumber is a virtual key code.
--
-- The NSArray is an autoreleased object. Do not release unless it is first retained.
--
-- ObjC selector: @- selectionKeys@
selectionKeys :: IsIMKCandidates imkCandidates => imkCandidates -> IO (Id NSArray)
selectionKeys imkCandidates =
  sendMessage imkCandidates selectionKeysSelector

-- | Sets the key layout that is used to map virtual key codes to characters.
--
-- ObjC selector: @- setSelectionKeysKeylayout:@
setSelectionKeysKeylayout :: IsIMKCandidates imkCandidates => imkCandidates -> Ptr () -> IO ()
setSelectionKeysKeylayout imkCandidates layout =
  sendMessage imkCandidates setSelectionKeysKeylayoutSelector layout

-- | Returns the key layout that is used to map virtual key codes for the selection keys.  By default this is the key layout whose source id is com.apple.keylayout.US.
--
-- This is an autoreleased object.  Retain it if you need to keep it.
--
-- ObjC selector: @- selectionKeysKeylayout@
selectionKeysKeylayout :: IsIMKCandidates imkCandidates => imkCandidates -> IO (Ptr ())
selectionKeysKeylayout imkCandidates =
  sendMessage imkCandidates selectionKeysKeylayoutSelector

-- | Sets the "style" attributes for the candidates window.  The keys for the attributes dictionary and the values are:
--
-- NSFontAttributeName (value = NSFont)  Setting the font attribute sets the font that is used to draw Candidates.  It does not effect the selection keys which are always drawn in the same font.  Note that to set the font size you should use this key/value pair.
--
-- IMKCandidatesOpacityAttributeName (value = NSNumber with a float value between 0 and 1).  Sets the opacity level to transparent (0.0) to completely opaque (1.0). The default opacity is 1.0.  This constant is declared above.
--
-- NSForegroundColorAttributeName (value = NSColor) Sets the text color used for the candidate text.  By default it is black.
--
-- NSBackgroundColorDocumentAttribute (value = NSColor).  Set the background color that is drawn behind the candidate text.
--
-- IMKCandidatesSendServerKeyEventFirst (value = NSNumber).  NO (default) gives the candidate window first chance at key events.  YES causes events to first be routed to the current IMKInputController.  In that case, if the event is not handled, it will then be sent to the candidate window.
--
-- ObjC selector: @- setAttributes:@
setAttributes :: (IsIMKCandidates imkCandidates, IsNSDictionary attributes) => imkCandidates -> attributes -> IO ()
setAttributes imkCandidates attributes =
  sendMessage imkCandidates setAttributesSelector (toNSDictionary attributes)

-- | Returns the attributes dictionary.
--
-- ObjC selector: @- attributes@
attributes :: IsIMKCandidates imkCandidates => imkCandidates -> IO (Id NSDictionary)
attributes imkCandidates =
  sendMessage imkCandidates attributesSelector

-- | Setting the dismissesAutomatically flag determines what happens to displayed candidates when the return key or enter key is typed.
--
-- By default, if a return or enter key is typed, the candidates are dismissed and a candidateSelected: message is sent to the input controller.  However  if setDismissesAutomatically is passed a NO flag  the candidate display will not be dismissed when a return or enter key is typed.  The input controller will still be sent the candidatesSelected: message, but, as stated, the candidates display will not be dismissed.
--
-- Setting this flag to NO lets an input method process text input while keeping a dynamically changing candidates display in view throughout the text input process.
--
-- When you set this to NO the candidate display will still be hidden when when a session deactivates.
--
-- ObjC selector: @- setDismissesAutomatically:@
setDismissesAutomatically :: IsIMKCandidates imkCandidates => imkCandidates -> Bool -> IO ()
setDismissesAutomatically imkCandidates flag =
  sendMessage imkCandidates setDismissesAutomaticallySelector flag

-- | Returns the dismissesAutomatically flag.
--
-- ObjC selector: @- dismissesAutomatically@
dismissesAutomatically :: IsIMKCandidates imkCandidates => imkCandidates -> IO Bool
dismissesAutomatically imkCandidates =
  sendMessage imkCandidates dismissesAutomaticallySelector

-- | Returns the currently selected candidate identifer.
--
-- Attempts to determine the identifier for the selected candidate.  If there is no selection the return value will be NSNotFound.
--
-- ObjC selector: @- selectedCandidate@
selectedCandidate :: IsIMKCandidates imkCandidates => imkCandidates -> IO CLong
selectedCandidate imkCandidates =
  sendMessage imkCandidates selectedCandidateSelector

-- | Positions the top-left corner of the candidate window’s frame rectangle at a given point in screen coordinates.
--
-- ObjC selector: @- setCandidateFrameTopLeft:@
setCandidateFrameTopLeft :: IsIMKCandidates imkCandidates => imkCandidates -> NSPoint -> IO ()
setCandidateFrameTopLeft imkCandidates point =
  sendMessage imkCandidates setCandidateFrameTopLeftSelector point

-- | If the current selection has a child IMKCandidates object that will be shown.
--
-- If there is a failure in showing the child this method will throw an exception.
--
-- ObjC selector: @- showChild@
showChild :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
showChild imkCandidates =
  sendMessage imkCandidates showChildSelector

-- | If the current selection has a child IMKCandidates that is being shown hide it.
--
-- Typically a client will not need to call this as IMKCandidates automatically hides and shows children.
--
-- ObjC selector: @- hideChild@
hideChild :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
hideChild imkCandidates =
  sendMessage imkCandidates hideChildSelector

-- | Attach an IMKCandidates object to the specified selection.
--
-- The IMKCandidate can be a sublist or an annotation.
--
-- ObjC selector: @- attachChild:toCandidate:type:@
attachChild_toCandidate_type :: (IsIMKCandidates imkCandidates, IsIMKCandidates child) => imkCandidates -> child -> CLong -> CULong -> IO ()
attachChild_toCandidate_type imkCandidates child candidateIdentifier theType =
  sendMessage imkCandidates attachChild_toCandidate_typeSelector (toIMKCandidates child) candidateIdentifier theType

-- | Detach the IMKCandidates object attached to candidate
--
-- ObjC selector: @- detachChild:@
detachChild :: IsIMKCandidates imkCandidates => imkCandidates -> CLong -> IO ()
detachChild imkCandidates candidateIdentifier =
  sendMessage imkCandidates detachChildSelector candidateIdentifier

-- | Set the candidates data directly rather than supplying data via [IMKInputContoller candidates:].
--
-- The elements of the array can be strings or attributed strings.
--
-- ObjC selector: @- setCandidateData:@
setCandidateData :: (IsIMKCandidates imkCandidates, IsNSArray candidatesArray) => imkCandidates -> candidatesArray -> IO ()
setCandidateData imkCandidates candidatesArray =
  sendMessage imkCandidates setCandidateDataSelector (toNSArray candidatesArray)

-- | Select the candidate whose identifier matches the identifier parameter.
--
-- Returns: YES if the candidateIdentifier is valid an the selection was made.  NO if canidateIdentifier is invalid or it was not possible make the selection.
--
-- @An@ — identifier for a candidate.  You can obtain an identifier by mapping a candidate to an identifier via the [IMKCandidates candidateStringIdentifier:].
--
-- ObjC selector: @- selectCandidateWithIdentifier:@
selectCandidateWithIdentifier :: IsIMKCandidates imkCandidates => imkCandidates -> CLong -> IO Bool
selectCandidateWithIdentifier imkCandidates candidateIdentifier =
  sendMessage imkCandidates selectCandidateWithIdentifierSelector candidateIdentifier

-- | @- selectCandidate:@
selectCandidate :: IsIMKCandidates imkCandidates => imkCandidates -> CLong -> IO ()
selectCandidate imkCandidates candidateIdentifier =
  sendMessage imkCandidates selectCandidateSelector candidateIdentifier

-- | Show the candidate window.
--
-- This simply shows the candidates.  No effort is made to position the candidate.  The caller should move the candidate window to an appropriate location prior to showing.
--
-- ObjC selector: @- showCandidates@
showCandidates :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
showCandidates imkCandidates =
  sendMessage imkCandidates showCandidatesSelector

-- | Map a candidateString to an identifier.
--
-- Beginning with MacOS 10.7, candidate strings are mapped internally to an unique identifier of type NSInteger.  Using identifiers to identify a particular candidate is the first stage of enabling data types other than NSString and NSAttributedString for containing the contents of a candidate.
--
-- ObjC selector: @- candidateStringIdentifier:@
candidateStringIdentifier :: IsIMKCandidates imkCandidates => imkCandidates -> RawId -> IO CLong
candidateStringIdentifier imkCandidates candidateString =
  sendMessage imkCandidates candidateStringIdentifierSelector candidateString

-- | Returns the currently selected candidate string.
--
-- Attempts to determine the string for the selected candidate.  If there is no selection the return value can be nil.  The attributed string is an autoreleased object.
--
-- ObjC selector: @- selectedCandidateString@
selectedCandidateString :: IsIMKCandidates imkCandidates => imkCandidates -> IO (Id NSAttributedString)
selectedCandidateString imkCandidates =
  sendMessage imkCandidates selectedCandidateStringSelector

-- | Returns the candidate identifier for a given line in the candidate window display.
--
-- Maps the lineNumber to a candidate identifier.  Line number 0 corresponds to the candidate in the cell currently in the first (top for vertical) line of the candidate window.  This is convienient for input methods that support selecting a candidate by a number key. Line Number values depend on the column arrangement of your candidate.  If you are displaying a single column candidate window, lines that have been scrolled out of view will have negative values.  For a single row grid line, numbers will correspond to the cell's position in the row (i.e. the first cell will be 0, the second 1, etc).  Finally, for a grid, the line numbers correspond to the grid row.  If the line number is invalid, NSNotFound is returned.
--
-- @lineNumber@ — a number representing a cells position in the candidate window.
--
-- ObjC selector: @- candidateIdentifierAtLineNumber:@
candidateIdentifierAtLineNumber :: IsIMKCandidates imkCandidates => imkCandidates -> CLong -> IO CLong
candidateIdentifierAtLineNumber imkCandidates lineNumber =
  sendMessage imkCandidates candidateIdentifierAtLineNumberSelector lineNumber

-- | Returns the line number for a given CandidateID.
--
-- Returns: The line number.  NSNotFound if the candidateID is invalid.
--
-- @candidateIdentifier@ — - A valid identifier for a candidate.
--
-- If the cell that contains the candidate is at the top line of the candidate window, the return value will be 0.
--
-- ObjC selector: @- lineNumberForCandidateWithIdentifier:@
lineNumberForCandidateWithIdentifier :: IsIMKCandidates imkCandidates => imkCandidates -> CLong -> IO CLong
lineNumberForCandidateWithIdentifier imkCandidates candidateIdentifier =
  sendMessage imkCandidates lineNumberForCandidateWithIdentifierSelector candidateIdentifier

-- | Clears the current selection.
--
-- ObjC selector: @- clearSelection@
clearSelection :: IsIMKCandidates imkCandidates => imkCandidates -> IO ()
clearSelection imkCandidates =
  sendMessage imkCandidates clearSelectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServer:panelType:@
initWithServer_panelTypeSelector :: Selector '[Id IMKServer, CULong] RawId
initWithServer_panelTypeSelector = mkSelector "initWithServer:panelType:"

-- | @Selector@ for @initWithServer:panelType:styleType:@
initWithServer_panelType_styleTypeSelector :: Selector '[Id IMKServer, CULong, CULong] RawId
initWithServer_panelType_styleTypeSelector = mkSelector "initWithServer:panelType:styleType:"

-- | @Selector@ for @panelType@
panelTypeSelector :: Selector '[] CULong
panelTypeSelector = mkSelector "panelType"

-- | @Selector@ for @setPanelType:@
setPanelTypeSelector :: Selector '[CULong] ()
setPanelTypeSelector = mkSelector "setPanelType:"

-- | @Selector@ for @show:@
showSelector :: Selector '[CULong] ()
showSelector = mkSelector "show:"

-- | @Selector@ for @hide@
hideSelector :: Selector '[] ()
hideSelector = mkSelector "hide"

-- | @Selector@ for @isVisible@
isVisibleSelector :: Selector '[] Bool
isVisibleSelector = mkSelector "isVisible"

-- | @Selector@ for @updateCandidates@
updateCandidatesSelector :: Selector '[] ()
updateCandidatesSelector = mkSelector "updateCandidates"

-- | @Selector@ for @showAnnotation:@
showAnnotationSelector :: Selector '[Id NSAttributedString] ()
showAnnotationSelector = mkSelector "showAnnotation:"

-- | @Selector@ for @showSublist:subListDelegate:@
showSublist_subListDelegateSelector :: Selector '[Id NSArray, RawId] ()
showSublist_subListDelegateSelector = mkSelector "showSublist:subListDelegate:"

-- | @Selector@ for @candidateFrame@
candidateFrameSelector :: Selector '[] NSRect
candidateFrameSelector = mkSelector "candidateFrame"

-- | @Selector@ for @setSelectionKeys:@
setSelectionKeysSelector :: Selector '[Id NSArray] ()
setSelectionKeysSelector = mkSelector "setSelectionKeys:"

-- | @Selector@ for @selectionKeys@
selectionKeysSelector :: Selector '[] (Id NSArray)
selectionKeysSelector = mkSelector "selectionKeys"

-- | @Selector@ for @setSelectionKeysKeylayout:@
setSelectionKeysKeylayoutSelector :: Selector '[Ptr ()] ()
setSelectionKeysKeylayoutSelector = mkSelector "setSelectionKeysKeylayout:"

-- | @Selector@ for @selectionKeysKeylayout@
selectionKeysKeylayoutSelector :: Selector '[] (Ptr ())
selectionKeysKeylayoutSelector = mkSelector "selectionKeysKeylayout"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSDictionary] ()
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setDismissesAutomatically:@
setDismissesAutomaticallySelector :: Selector '[Bool] ()
setDismissesAutomaticallySelector = mkSelector "setDismissesAutomatically:"

-- | @Selector@ for @dismissesAutomatically@
dismissesAutomaticallySelector :: Selector '[] Bool
dismissesAutomaticallySelector = mkSelector "dismissesAutomatically"

-- | @Selector@ for @selectedCandidate@
selectedCandidateSelector :: Selector '[] CLong
selectedCandidateSelector = mkSelector "selectedCandidate"

-- | @Selector@ for @setCandidateFrameTopLeft:@
setCandidateFrameTopLeftSelector :: Selector '[NSPoint] ()
setCandidateFrameTopLeftSelector = mkSelector "setCandidateFrameTopLeft:"

-- | @Selector@ for @showChild@
showChildSelector :: Selector '[] ()
showChildSelector = mkSelector "showChild"

-- | @Selector@ for @hideChild@
hideChildSelector :: Selector '[] ()
hideChildSelector = mkSelector "hideChild"

-- | @Selector@ for @attachChild:toCandidate:type:@
attachChild_toCandidate_typeSelector :: Selector '[Id IMKCandidates, CLong, CULong] ()
attachChild_toCandidate_typeSelector = mkSelector "attachChild:toCandidate:type:"

-- | @Selector@ for @detachChild:@
detachChildSelector :: Selector '[CLong] ()
detachChildSelector = mkSelector "detachChild:"

-- | @Selector@ for @setCandidateData:@
setCandidateDataSelector :: Selector '[Id NSArray] ()
setCandidateDataSelector = mkSelector "setCandidateData:"

-- | @Selector@ for @selectCandidateWithIdentifier:@
selectCandidateWithIdentifierSelector :: Selector '[CLong] Bool
selectCandidateWithIdentifierSelector = mkSelector "selectCandidateWithIdentifier:"

-- | @Selector@ for @selectCandidate:@
selectCandidateSelector :: Selector '[CLong] ()
selectCandidateSelector = mkSelector "selectCandidate:"

-- | @Selector@ for @showCandidates@
showCandidatesSelector :: Selector '[] ()
showCandidatesSelector = mkSelector "showCandidates"

-- | @Selector@ for @candidateStringIdentifier:@
candidateStringIdentifierSelector :: Selector '[RawId] CLong
candidateStringIdentifierSelector = mkSelector "candidateStringIdentifier:"

-- | @Selector@ for @selectedCandidateString@
selectedCandidateStringSelector :: Selector '[] (Id NSAttributedString)
selectedCandidateStringSelector = mkSelector "selectedCandidateString"

-- | @Selector@ for @candidateIdentifierAtLineNumber:@
candidateIdentifierAtLineNumberSelector :: Selector '[CLong] CLong
candidateIdentifierAtLineNumberSelector = mkSelector "candidateIdentifierAtLineNumber:"

-- | @Selector@ for @lineNumberForCandidateWithIdentifier:@
lineNumberForCandidateWithIdentifierSelector :: Selector '[CLong] CLong
lineNumberForCandidateWithIdentifierSelector = mkSelector "lineNumberForCandidateWithIdentifier:"

-- | @Selector@ for @clearSelection@
clearSelectionSelector :: Selector '[] ()
clearSelectionSelector = mkSelector "clearSelection"

