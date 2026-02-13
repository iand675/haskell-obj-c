{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSResponder@.
module ObjC.AppKit.NSResponder
  ( NSResponder
  , IsNSResponder(..)
  , init_
  , initWithCoder
  , tryToPerform_with
  , performKeyEquivalent
  , validRequestorForSendType_returnType
  , mouseDown
  , rightMouseDown
  , otherMouseDown
  , mouseUp
  , rightMouseUp
  , otherMouseUp
  , mouseMoved
  , mouseDragged
  , mouseCancelled
  , scrollWheel
  , rightMouseDragged
  , otherMouseDragged
  , mouseEntered
  , mouseExited
  , keyDown
  , keyUp
  , flagsChanged
  , tabletPoint
  , tabletProximity
  , cursorUpdate
  , magnifyWithEvent
  , rotateWithEvent
  , swipeWithEvent
  , beginGestureWithEvent
  , endGestureWithEvent
  , smartMagnifyWithEvent
  , changeModeWithEvent
  , touchesBeganWithEvent
  , touchesMovedWithEvent
  , touchesEndedWithEvent
  , touchesCancelledWithEvent
  , quickLookWithEvent
  , pressureChangeWithEvent
  , contextMenuKeyDown
  , noResponderFor
  , becomeFirstResponder
  , resignFirstResponder
  , interpretKeyEvents
  , flushBufferedKeyEvents
  , showContextHelp
  , helpRequested
  , shouldBeTreatedAsInkEvent
  , wantsScrollEventsForSwipeTrackingOnAxis
  , wantsForwardedScrollEventsForAxis
  , supplementalTargetForAction_sender
  , encodeRestorableStateWithCoder
  , encodeRestorableStateWithCoder_backgroundQueue
  , restoreStateWithCoder
  , invalidateRestorableState
  , allowedClassesForRestorableStateKeyPath
  , interfaceStyle
  , setInterfaceStyle
  , makeTouchBar
  , updateUserActivityState
  , performMnemonic
  , showWritingTools
  , newWindowForTab
  , performTextFinderAction
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfo
  , presentError
  , willPresentError
  , validateProposedFirstResponder_forEvent
  , nextResponder
  , setNextResponder
  , acceptsFirstResponder
  , menu
  , setMenu
  , restorableStateKeyPaths
  , touchBar
  , setTouchBar
  , userActivity
  , setUserActivity
  , undoManager
  , acceptsFirstResponderSelector
  , allowedClassesForRestorableStateKeyPathSelector
  , becomeFirstResponderSelector
  , beginGestureWithEventSelector
  , changeModeWithEventSelector
  , contextMenuKeyDownSelector
  , cursorUpdateSelector
  , encodeRestorableStateWithCoderSelector
  , encodeRestorableStateWithCoder_backgroundQueueSelector
  , endGestureWithEventSelector
  , flagsChangedSelector
  , flushBufferedKeyEventsSelector
  , helpRequestedSelector
  , initSelector
  , initWithCoderSelector
  , interfaceStyleSelector
  , interpretKeyEventsSelector
  , invalidateRestorableStateSelector
  , keyDownSelector
  , keyUpSelector
  , magnifyWithEventSelector
  , makeTouchBarSelector
  , menuSelector
  , mouseCancelledSelector
  , mouseDownSelector
  , mouseDraggedSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , mouseMovedSelector
  , mouseUpSelector
  , newWindowForTabSelector
  , nextResponderSelector
  , noResponderForSelector
  , otherMouseDownSelector
  , otherMouseDraggedSelector
  , otherMouseUpSelector
  , performKeyEquivalentSelector
  , performMnemonicSelector
  , performTextFinderActionSelector
  , presentErrorSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , pressureChangeWithEventSelector
  , quickLookWithEventSelector
  , resignFirstResponderSelector
  , restorableStateKeyPathsSelector
  , restoreStateWithCoderSelector
  , rightMouseDownSelector
  , rightMouseDraggedSelector
  , rightMouseUpSelector
  , rotateWithEventSelector
  , scrollWheelSelector
  , setInterfaceStyleSelector
  , setMenuSelector
  , setNextResponderSelector
  , setTouchBarSelector
  , setUserActivitySelector
  , shouldBeTreatedAsInkEventSelector
  , showContextHelpSelector
  , showWritingToolsSelector
  , smartMagnifyWithEventSelector
  , supplementalTargetForAction_senderSelector
  , swipeWithEventSelector
  , tabletPointSelector
  , tabletProximitySelector
  , touchBarSelector
  , touchesBeganWithEventSelector
  , touchesCancelledWithEventSelector
  , touchesEndedWithEventSelector
  , touchesMovedWithEventSelector
  , tryToPerform_withSelector
  , undoManagerSelector
  , updateUserActivityStateSelector
  , userActivitySelector
  , validRequestorForSendType_returnTypeSelector
  , validateProposedFirstResponder_forEventSelector
  , wantsForwardedScrollEventsForAxisSelector
  , wantsScrollEventsForSwipeTrackingOnAxisSelector
  , willPresentErrorSelector

  -- * Enum types
  , NSEventGestureAxis(NSEventGestureAxis)
  , pattern NSEventGestureAxisNone
  , pattern NSEventGestureAxisHorizontal
  , pattern NSEventGestureAxisVertical

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSResponder nsResponder => nsResponder -> IO (Id NSResponder)
init_ nsResponder =
  sendOwnedMessage nsResponder initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO (Id NSResponder)
initWithCoder nsResponder coder =
  sendOwnedMessage nsResponder initWithCoderSelector (toNSCoder coder)

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSResponder nsResponder => nsResponder -> Sel -> RawId -> IO Bool
tryToPerform_with nsResponder action object =
  sendMessage nsResponder tryToPerform_withSelector action object

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO Bool
performKeyEquivalent nsResponder event =
  sendMessage nsResponder performKeyEquivalentSelector (toNSEvent event)

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSResponder nsResponder, IsNSString sendType, IsNSString returnType) => nsResponder -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsResponder sendType returnType =
  sendMessage nsResponder validRequestorForSendType_returnTypeSelector (toNSString sendType) (toNSString returnType)

-- | @- mouseDown:@
mouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseDown nsResponder event =
  sendMessage nsResponder mouseDownSelector (toNSEvent event)

-- | @- rightMouseDown:@
rightMouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseDown nsResponder event =
  sendMessage nsResponder rightMouseDownSelector (toNSEvent event)

-- | @- otherMouseDown:@
otherMouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseDown nsResponder event =
  sendMessage nsResponder otherMouseDownSelector (toNSEvent event)

-- | @- mouseUp:@
mouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseUp nsResponder event =
  sendMessage nsResponder mouseUpSelector (toNSEvent event)

-- | @- rightMouseUp:@
rightMouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseUp nsResponder event =
  sendMessage nsResponder rightMouseUpSelector (toNSEvent event)

-- | @- otherMouseUp:@
otherMouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseUp nsResponder event =
  sendMessage nsResponder otherMouseUpSelector (toNSEvent event)

-- | @- mouseMoved:@
mouseMoved :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseMoved nsResponder event =
  sendMessage nsResponder mouseMovedSelector (toNSEvent event)

-- | @- mouseDragged:@
mouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseDragged nsResponder event =
  sendMessage nsResponder mouseDraggedSelector (toNSEvent event)

-- | @- mouseCancelled:@
mouseCancelled :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseCancelled nsResponder event =
  sendMessage nsResponder mouseCancelledSelector (toNSEvent event)

-- | @- scrollWheel:@
scrollWheel :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
scrollWheel nsResponder event =
  sendMessage nsResponder scrollWheelSelector (toNSEvent event)

-- | @- rightMouseDragged:@
rightMouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseDragged nsResponder event =
  sendMessage nsResponder rightMouseDraggedSelector (toNSEvent event)

-- | @- otherMouseDragged:@
otherMouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseDragged nsResponder event =
  sendMessage nsResponder otherMouseDraggedSelector (toNSEvent event)

-- | @- mouseEntered:@
mouseEntered :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseEntered nsResponder event =
  sendMessage nsResponder mouseEnteredSelector (toNSEvent event)

-- | @- mouseExited:@
mouseExited :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseExited nsResponder event =
  sendMessage nsResponder mouseExitedSelector (toNSEvent event)

-- | @- keyDown:@
keyDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
keyDown nsResponder event =
  sendMessage nsResponder keyDownSelector (toNSEvent event)

-- | @- keyUp:@
keyUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
keyUp nsResponder event =
  sendMessage nsResponder keyUpSelector (toNSEvent event)

-- | @- flagsChanged:@
flagsChanged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
flagsChanged nsResponder event =
  sendMessage nsResponder flagsChangedSelector (toNSEvent event)

-- | @- tabletPoint:@
tabletPoint :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
tabletPoint nsResponder event =
  sendMessage nsResponder tabletPointSelector (toNSEvent event)

-- | @- tabletProximity:@
tabletProximity :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
tabletProximity nsResponder event =
  sendMessage nsResponder tabletProximitySelector (toNSEvent event)

-- | @- cursorUpdate:@
cursorUpdate :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
cursorUpdate nsResponder event =
  sendMessage nsResponder cursorUpdateSelector (toNSEvent event)

-- | @- magnifyWithEvent:@
magnifyWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
magnifyWithEvent nsResponder event =
  sendMessage nsResponder magnifyWithEventSelector (toNSEvent event)

-- | @- rotateWithEvent:@
rotateWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rotateWithEvent nsResponder event =
  sendMessage nsResponder rotateWithEventSelector (toNSEvent event)

-- | @- swipeWithEvent:@
swipeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
swipeWithEvent nsResponder event =
  sendMessage nsResponder swipeWithEventSelector (toNSEvent event)

-- | @- beginGestureWithEvent:@
beginGestureWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
beginGestureWithEvent nsResponder event =
  sendMessage nsResponder beginGestureWithEventSelector (toNSEvent event)

-- | @- endGestureWithEvent:@
endGestureWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
endGestureWithEvent nsResponder event =
  sendMessage nsResponder endGestureWithEventSelector (toNSEvent event)

-- | @- smartMagnifyWithEvent:@
smartMagnifyWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
smartMagnifyWithEvent nsResponder event =
  sendMessage nsResponder smartMagnifyWithEventSelector (toNSEvent event)

-- | @- changeModeWithEvent:@
changeModeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
changeModeWithEvent nsResponder event =
  sendMessage nsResponder changeModeWithEventSelector (toNSEvent event)

-- | @- touchesBeganWithEvent:@
touchesBeganWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesBeganWithEvent nsResponder event =
  sendMessage nsResponder touchesBeganWithEventSelector (toNSEvent event)

-- | @- touchesMovedWithEvent:@
touchesMovedWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesMovedWithEvent nsResponder event =
  sendMessage nsResponder touchesMovedWithEventSelector (toNSEvent event)

-- | @- touchesEndedWithEvent:@
touchesEndedWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesEndedWithEvent nsResponder event =
  sendMessage nsResponder touchesEndedWithEventSelector (toNSEvent event)

-- | @- touchesCancelledWithEvent:@
touchesCancelledWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesCancelledWithEvent nsResponder event =
  sendMessage nsResponder touchesCancelledWithEventSelector (toNSEvent event)

-- | @- quickLookWithEvent:@
quickLookWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
quickLookWithEvent nsResponder event =
  sendMessage nsResponder quickLookWithEventSelector (toNSEvent event)

-- | @- pressureChangeWithEvent:@
pressureChangeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
pressureChangeWithEvent nsResponder event =
  sendMessage nsResponder pressureChangeWithEventSelector (toNSEvent event)

-- | `contextMenuKeyDown`
--
-- Handle a key event that should present a context menu at the user focus.
--
-- Most applications should not override this method. Instead, you should customize the context menu displayed from a keyboard event by implementing @menuForEvent:@ and @selectionAnchorRect@, or @showContextMenuForSelection:@, rather than this method.
--
-- You should only override this method when you do not want the system-provided default behavior for the context menu hotkey, either for a specific key combination, or for the hotkey in general. For example, if your application already provides a different behavior for control-Return (the default context menu hotkey definition), and you want to preserve that behavior, you should override this method to handle that specific key combination, and then return without calling @super@. Note that the user may customize the hotkey to a different key combination, so in this example, if any other key combination is passed to your method, you would call @super@.
--
-- An implementation of this method should call @[super contextMenuKeyDown:event]@ to pass the request up the responder chain. If the message reaches the application object, NSApplication's implementation of this method will send @showContextMenuForSelection:@ to the responder chain. If you do not call @super@, then no further handling of the key event will be performed.
--
-- Note: In some cases, @showContextMenuForSelection:@ will be called without a prior call to @contextMenuKeyDown:@. This occurs when a view receives an Accessibility ShowMenu action, or when the user has created a Cocoa Text key binding to map a different key combination to the @showContextMenuForSelection:@ action.
--
-- @event@ — The key down event that matches the system-wide context menu hotkey combination.
--
-- @showContextMenuForSelection:@
--
-- ObjC selector: @- contextMenuKeyDown:@
contextMenuKeyDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
contextMenuKeyDown nsResponder event =
  sendMessage nsResponder contextMenuKeyDownSelector (toNSEvent event)

-- | @- noResponderFor:@
noResponderFor :: IsNSResponder nsResponder => nsResponder -> Sel -> IO ()
noResponderFor nsResponder eventSelector =
  sendMessage nsResponder noResponderForSelector eventSelector

-- | @- becomeFirstResponder@
becomeFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
becomeFirstResponder nsResponder =
  sendMessage nsResponder becomeFirstResponderSelector

-- | @- resignFirstResponder@
resignFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
resignFirstResponder nsResponder =
  sendMessage nsResponder resignFirstResponderSelector

-- | @- interpretKeyEvents:@
interpretKeyEvents :: (IsNSResponder nsResponder, IsNSArray eventArray) => nsResponder -> eventArray -> IO ()
interpretKeyEvents nsResponder eventArray =
  sendMessage nsResponder interpretKeyEventsSelector (toNSArray eventArray)

-- | @- flushBufferedKeyEvents@
flushBufferedKeyEvents :: IsNSResponder nsResponder => nsResponder -> IO ()
flushBufferedKeyEvents nsResponder =
  sendMessage nsResponder flushBufferedKeyEventsSelector

-- | @- showContextHelp:@
showContextHelp :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
showContextHelp nsResponder sender =
  sendMessage nsResponder showContextHelpSelector sender

-- | @- helpRequested:@
helpRequested :: (IsNSResponder nsResponder, IsNSEvent eventPtr) => nsResponder -> eventPtr -> IO ()
helpRequested nsResponder eventPtr =
  sendMessage nsResponder helpRequestedSelector (toNSEvent eventPtr)

-- | @- shouldBeTreatedAsInkEvent:@
shouldBeTreatedAsInkEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO Bool
shouldBeTreatedAsInkEvent nsResponder event =
  sendMessage nsResponder shouldBeTreatedAsInkEventSelector (toNSEvent event)

-- | @- wantsScrollEventsForSwipeTrackingOnAxis:@
wantsScrollEventsForSwipeTrackingOnAxis :: IsNSResponder nsResponder => nsResponder -> NSEventGestureAxis -> IO Bool
wantsScrollEventsForSwipeTrackingOnAxis nsResponder axis =
  sendMessage nsResponder wantsScrollEventsForSwipeTrackingOnAxisSelector axis

-- | @- wantsForwardedScrollEventsForAxis:@
wantsForwardedScrollEventsForAxis :: IsNSResponder nsResponder => nsResponder -> NSEventGestureAxis -> IO Bool
wantsForwardedScrollEventsForAxis nsResponder axis =
  sendMessage nsResponder wantsForwardedScrollEventsForAxisSelector axis

-- | @- supplementalTargetForAction:sender:@
supplementalTargetForAction_sender :: IsNSResponder nsResponder => nsResponder -> Sel -> RawId -> IO RawId
supplementalTargetForAction_sender nsResponder action sender =
  sendMessage nsResponder supplementalTargetForAction_senderSelector action sender

-- | @- encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO ()
encodeRestorableStateWithCoder nsResponder coder =
  sendMessage nsResponder encodeRestorableStateWithCoderSelector (toNSCoder coder)

-- | @- encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueue :: (IsNSResponder nsResponder, IsNSCoder coder, IsNSOperationQueue queue) => nsResponder -> coder -> queue -> IO ()
encodeRestorableStateWithCoder_backgroundQueue nsResponder coder queue =
  sendMessage nsResponder encodeRestorableStateWithCoder_backgroundQueueSelector (toNSCoder coder) (toNSOperationQueue queue)

-- | @- restoreStateWithCoder:@
restoreStateWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO ()
restoreStateWithCoder nsResponder coder =
  sendMessage nsResponder restoreStateWithCoderSelector (toNSCoder coder)

-- | @- invalidateRestorableState@
invalidateRestorableState :: IsNSResponder nsResponder => nsResponder -> IO ()
invalidateRestorableState nsResponder =
  sendMessage nsResponder invalidateRestorableStateSelector

-- | When secure state restoration is used, values at restorableStateKeyPaths must support NSSecureCoding and this method will be consulted when restoring state. Any values not of an allowed class will not be set.
--
-- ObjC selector: @+ allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSArray)
allowedClassesForRestorableStateKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSResponder"
    sendClassMessage cls' allowedClassesForRestorableStateKeyPathSelector (toNSString keyPath)

-- | @- interfaceStyle@
interfaceStyle :: IsNSResponder nsResponder => nsResponder -> IO CULong
interfaceStyle nsResponder =
  sendMessage nsResponder interfaceStyleSelector

-- | @- setInterfaceStyle:@
setInterfaceStyle :: IsNSResponder nsResponder => nsResponder -> CULong -> IO ()
setInterfaceStyle nsResponder interfaceStyle =
  sendMessage nsResponder setInterfaceStyleSelector interfaceStyle

-- | @- makeTouchBar@
makeTouchBar :: IsNSResponder nsResponder => nsResponder -> IO (Id NSTouchBar)
makeTouchBar nsResponder =
  sendMessage nsResponder makeTouchBarSelector

-- | @- updateUserActivityState:@
updateUserActivityState :: (IsNSResponder nsResponder, IsNSUserActivity userActivity) => nsResponder -> userActivity -> IO ()
updateUserActivityState nsResponder userActivity =
  sendMessage nsResponder updateUserActivityStateSelector (toNSUserActivity userActivity)

-- | @- performMnemonic:@
performMnemonic :: (IsNSResponder nsResponder, IsNSString string) => nsResponder -> string -> IO Bool
performMnemonic nsResponder string =
  sendMessage nsResponder performMnemonicSelector (toNSString string)

-- | @- showWritingTools:@
showWritingTools :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
showWritingTools nsResponder sender =
  sendMessage nsResponder showWritingToolsSelector sender

-- | @- newWindowForTab:@
newWindowForTab :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
newWindowForTab nsResponder sender =
  sendOwnedMessage nsResponder newWindowForTabSelector sender

-- | @- performTextFinderAction:@
performTextFinderAction :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
performTextFinderAction nsResponder sender =
  sendMessage nsResponder performTextFinderActionSelector sender

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSResponder nsResponder, IsNSError error_, IsNSWindow window) => nsResponder -> error_ -> window -> RawId -> Sel -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsResponder error_ window delegate didPresentSelector contextInfo =
  sendMessage nsResponder presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector (toNSError error_) (toNSWindow window) delegate didPresentSelector contextInfo

-- | @- presentError:@
presentError :: (IsNSResponder nsResponder, IsNSError error_) => nsResponder -> error_ -> IO Bool
presentError nsResponder error_ =
  sendMessage nsResponder presentErrorSelector (toNSError error_)

-- | @- willPresentError:@
willPresentError :: (IsNSResponder nsResponder, IsNSError error_) => nsResponder -> error_ -> IO (Id NSError)
willPresentError nsResponder error_ =
  sendMessage nsResponder willPresentErrorSelector (toNSError error_)

-- | @- validateProposedFirstResponder:forEvent:@
validateProposedFirstResponder_forEvent :: (IsNSResponder nsResponder, IsNSResponder responder, IsNSEvent event) => nsResponder -> responder -> event -> IO Bool
validateProposedFirstResponder_forEvent nsResponder responder event =
  sendMessage nsResponder validateProposedFirstResponder_forEventSelector (toNSResponder responder) (toNSEvent event)

-- | @- nextResponder@
nextResponder :: IsNSResponder nsResponder => nsResponder -> IO (Id NSResponder)
nextResponder nsResponder =
  sendMessage nsResponder nextResponderSelector

-- | @- setNextResponder:@
setNextResponder :: (IsNSResponder nsResponder, IsNSResponder value) => nsResponder -> value -> IO ()
setNextResponder nsResponder value =
  sendMessage nsResponder setNextResponderSelector (toNSResponder value)

-- | @- acceptsFirstResponder@
acceptsFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
acceptsFirstResponder nsResponder =
  sendMessage nsResponder acceptsFirstResponderSelector

-- | @- menu@
menu :: IsNSResponder nsResponder => nsResponder -> IO (Id NSMenu)
menu nsResponder =
  sendMessage nsResponder menuSelector

-- | @- setMenu:@
setMenu :: (IsNSResponder nsResponder, IsNSMenu value) => nsResponder -> value -> IO ()
setMenu nsResponder value =
  sendMessage nsResponder setMenuSelector (toNSMenu value)

-- | @+ restorableStateKeyPaths@
restorableStateKeyPaths :: IO (Id NSArray)
restorableStateKeyPaths  =
  do
    cls' <- getRequiredClass "NSResponder"
    sendClassMessage cls' restorableStateKeyPathsSelector

-- | @- touchBar@
touchBar :: IsNSResponder nsResponder => nsResponder -> IO (Id NSTouchBar)
touchBar nsResponder =
  sendMessage nsResponder touchBarSelector

-- | @- setTouchBar:@
setTouchBar :: (IsNSResponder nsResponder, IsNSTouchBar value) => nsResponder -> value -> IO ()
setTouchBar nsResponder value =
  sendMessage nsResponder setTouchBarSelector (toNSTouchBar value)

-- | @- userActivity@
userActivity :: IsNSResponder nsResponder => nsResponder -> IO (Id NSUserActivity)
userActivity nsResponder =
  sendMessage nsResponder userActivitySelector

-- | @- setUserActivity:@
setUserActivity :: (IsNSResponder nsResponder, IsNSUserActivity value) => nsResponder -> value -> IO ()
setUserActivity nsResponder value =
  sendMessage nsResponder setUserActivitySelector (toNSUserActivity value)

-- | @- undoManager@
undoManager :: IsNSResponder nsResponder => nsResponder -> IO (Id NSUndoManager)
undoManager nsResponder =
  sendMessage nsResponder undoManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSResponder)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSResponder)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector '[Sel, RawId] Bool
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector '[Id NSEvent] Bool
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector '[Id NSString, Id NSString] RawId
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector '[Id NSEvent] ()
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @rightMouseDown:@
rightMouseDownSelector :: Selector '[Id NSEvent] ()
rightMouseDownSelector = mkSelector "rightMouseDown:"

-- | @Selector@ for @otherMouseDown:@
otherMouseDownSelector :: Selector '[Id NSEvent] ()
otherMouseDownSelector = mkSelector "otherMouseDown:"

-- | @Selector@ for @mouseUp:@
mouseUpSelector :: Selector '[Id NSEvent] ()
mouseUpSelector = mkSelector "mouseUp:"

-- | @Selector@ for @rightMouseUp:@
rightMouseUpSelector :: Selector '[Id NSEvent] ()
rightMouseUpSelector = mkSelector "rightMouseUp:"

-- | @Selector@ for @otherMouseUp:@
otherMouseUpSelector :: Selector '[Id NSEvent] ()
otherMouseUpSelector = mkSelector "otherMouseUp:"

-- | @Selector@ for @mouseMoved:@
mouseMovedSelector :: Selector '[Id NSEvent] ()
mouseMovedSelector = mkSelector "mouseMoved:"

-- | @Selector@ for @mouseDragged:@
mouseDraggedSelector :: Selector '[Id NSEvent] ()
mouseDraggedSelector = mkSelector "mouseDragged:"

-- | @Selector@ for @mouseCancelled:@
mouseCancelledSelector :: Selector '[Id NSEvent] ()
mouseCancelledSelector = mkSelector "mouseCancelled:"

-- | @Selector@ for @scrollWheel:@
scrollWheelSelector :: Selector '[Id NSEvent] ()
scrollWheelSelector = mkSelector "scrollWheel:"

-- | @Selector@ for @rightMouseDragged:@
rightMouseDraggedSelector :: Selector '[Id NSEvent] ()
rightMouseDraggedSelector = mkSelector "rightMouseDragged:"

-- | @Selector@ for @otherMouseDragged:@
otherMouseDraggedSelector :: Selector '[Id NSEvent] ()
otherMouseDraggedSelector = mkSelector "otherMouseDragged:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector '[Id NSEvent] ()
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector '[Id NSEvent] ()
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @keyDown:@
keyDownSelector :: Selector '[Id NSEvent] ()
keyDownSelector = mkSelector "keyDown:"

-- | @Selector@ for @keyUp:@
keyUpSelector :: Selector '[Id NSEvent] ()
keyUpSelector = mkSelector "keyUp:"

-- | @Selector@ for @flagsChanged:@
flagsChangedSelector :: Selector '[Id NSEvent] ()
flagsChangedSelector = mkSelector "flagsChanged:"

-- | @Selector@ for @tabletPoint:@
tabletPointSelector :: Selector '[Id NSEvent] ()
tabletPointSelector = mkSelector "tabletPoint:"

-- | @Selector@ for @tabletProximity:@
tabletProximitySelector :: Selector '[Id NSEvent] ()
tabletProximitySelector = mkSelector "tabletProximity:"

-- | @Selector@ for @cursorUpdate:@
cursorUpdateSelector :: Selector '[Id NSEvent] ()
cursorUpdateSelector = mkSelector "cursorUpdate:"

-- | @Selector@ for @magnifyWithEvent:@
magnifyWithEventSelector :: Selector '[Id NSEvent] ()
magnifyWithEventSelector = mkSelector "magnifyWithEvent:"

-- | @Selector@ for @rotateWithEvent:@
rotateWithEventSelector :: Selector '[Id NSEvent] ()
rotateWithEventSelector = mkSelector "rotateWithEvent:"

-- | @Selector@ for @swipeWithEvent:@
swipeWithEventSelector :: Selector '[Id NSEvent] ()
swipeWithEventSelector = mkSelector "swipeWithEvent:"

-- | @Selector@ for @beginGestureWithEvent:@
beginGestureWithEventSelector :: Selector '[Id NSEvent] ()
beginGestureWithEventSelector = mkSelector "beginGestureWithEvent:"

-- | @Selector@ for @endGestureWithEvent:@
endGestureWithEventSelector :: Selector '[Id NSEvent] ()
endGestureWithEventSelector = mkSelector "endGestureWithEvent:"

-- | @Selector@ for @smartMagnifyWithEvent:@
smartMagnifyWithEventSelector :: Selector '[Id NSEvent] ()
smartMagnifyWithEventSelector = mkSelector "smartMagnifyWithEvent:"

-- | @Selector@ for @changeModeWithEvent:@
changeModeWithEventSelector :: Selector '[Id NSEvent] ()
changeModeWithEventSelector = mkSelector "changeModeWithEvent:"

-- | @Selector@ for @touchesBeganWithEvent:@
touchesBeganWithEventSelector :: Selector '[Id NSEvent] ()
touchesBeganWithEventSelector = mkSelector "touchesBeganWithEvent:"

-- | @Selector@ for @touchesMovedWithEvent:@
touchesMovedWithEventSelector :: Selector '[Id NSEvent] ()
touchesMovedWithEventSelector = mkSelector "touchesMovedWithEvent:"

-- | @Selector@ for @touchesEndedWithEvent:@
touchesEndedWithEventSelector :: Selector '[Id NSEvent] ()
touchesEndedWithEventSelector = mkSelector "touchesEndedWithEvent:"

-- | @Selector@ for @touchesCancelledWithEvent:@
touchesCancelledWithEventSelector :: Selector '[Id NSEvent] ()
touchesCancelledWithEventSelector = mkSelector "touchesCancelledWithEvent:"

-- | @Selector@ for @quickLookWithEvent:@
quickLookWithEventSelector :: Selector '[Id NSEvent] ()
quickLookWithEventSelector = mkSelector "quickLookWithEvent:"

-- | @Selector@ for @pressureChangeWithEvent:@
pressureChangeWithEventSelector :: Selector '[Id NSEvent] ()
pressureChangeWithEventSelector = mkSelector "pressureChangeWithEvent:"

-- | @Selector@ for @contextMenuKeyDown:@
contextMenuKeyDownSelector :: Selector '[Id NSEvent] ()
contextMenuKeyDownSelector = mkSelector "contextMenuKeyDown:"

-- | @Selector@ for @noResponderFor:@
noResponderForSelector :: Selector '[Sel] ()
noResponderForSelector = mkSelector "noResponderFor:"

-- | @Selector@ for @becomeFirstResponder@
becomeFirstResponderSelector :: Selector '[] Bool
becomeFirstResponderSelector = mkSelector "becomeFirstResponder"

-- | @Selector@ for @resignFirstResponder@
resignFirstResponderSelector :: Selector '[] Bool
resignFirstResponderSelector = mkSelector "resignFirstResponder"

-- | @Selector@ for @interpretKeyEvents:@
interpretKeyEventsSelector :: Selector '[Id NSArray] ()
interpretKeyEventsSelector = mkSelector "interpretKeyEvents:"

-- | @Selector@ for @flushBufferedKeyEvents@
flushBufferedKeyEventsSelector :: Selector '[] ()
flushBufferedKeyEventsSelector = mkSelector "flushBufferedKeyEvents"

-- | @Selector@ for @showContextHelp:@
showContextHelpSelector :: Selector '[RawId] ()
showContextHelpSelector = mkSelector "showContextHelp:"

-- | @Selector@ for @helpRequested:@
helpRequestedSelector :: Selector '[Id NSEvent] ()
helpRequestedSelector = mkSelector "helpRequested:"

-- | @Selector@ for @shouldBeTreatedAsInkEvent:@
shouldBeTreatedAsInkEventSelector :: Selector '[Id NSEvent] Bool
shouldBeTreatedAsInkEventSelector = mkSelector "shouldBeTreatedAsInkEvent:"

-- | @Selector@ for @wantsScrollEventsForSwipeTrackingOnAxis:@
wantsScrollEventsForSwipeTrackingOnAxisSelector :: Selector '[NSEventGestureAxis] Bool
wantsScrollEventsForSwipeTrackingOnAxisSelector = mkSelector "wantsScrollEventsForSwipeTrackingOnAxis:"

-- | @Selector@ for @wantsForwardedScrollEventsForAxis:@
wantsForwardedScrollEventsForAxisSelector :: Selector '[NSEventGestureAxis] Bool
wantsForwardedScrollEventsForAxisSelector = mkSelector "wantsForwardedScrollEventsForAxis:"

-- | @Selector@ for @supplementalTargetForAction:sender:@
supplementalTargetForAction_senderSelector :: Selector '[Sel, RawId] RawId
supplementalTargetForAction_senderSelector = mkSelector "supplementalTargetForAction:sender:"

-- | @Selector@ for @encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoderSelector :: Selector '[Id NSCoder] ()
encodeRestorableStateWithCoderSelector = mkSelector "encodeRestorableStateWithCoder:"

-- | @Selector@ for @encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueueSelector :: Selector '[Id NSCoder, Id NSOperationQueue] ()
encodeRestorableStateWithCoder_backgroundQueueSelector = mkSelector "encodeRestorableStateWithCoder:backgroundQueue:"

-- | @Selector@ for @restoreStateWithCoder:@
restoreStateWithCoderSelector :: Selector '[Id NSCoder] ()
restoreStateWithCoderSelector = mkSelector "restoreStateWithCoder:"

-- | @Selector@ for @invalidateRestorableState@
invalidateRestorableStateSelector :: Selector '[] ()
invalidateRestorableStateSelector = mkSelector "invalidateRestorableState"

-- | @Selector@ for @allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPathSelector :: Selector '[Id NSString] (Id NSArray)
allowedClassesForRestorableStateKeyPathSelector = mkSelector "allowedClassesForRestorableStateKeyPath:"

-- | @Selector@ for @interfaceStyle@
interfaceStyleSelector :: Selector '[] CULong
interfaceStyleSelector = mkSelector "interfaceStyle"

-- | @Selector@ for @setInterfaceStyle:@
setInterfaceStyleSelector :: Selector '[CULong] ()
setInterfaceStyleSelector = mkSelector "setInterfaceStyle:"

-- | @Selector@ for @makeTouchBar@
makeTouchBarSelector :: Selector '[] (Id NSTouchBar)
makeTouchBarSelector = mkSelector "makeTouchBar"

-- | @Selector@ for @updateUserActivityState:@
updateUserActivityStateSelector :: Selector '[Id NSUserActivity] ()
updateUserActivityStateSelector = mkSelector "updateUserActivityState:"

-- | @Selector@ for @performMnemonic:@
performMnemonicSelector :: Selector '[Id NSString] Bool
performMnemonicSelector = mkSelector "performMnemonic:"

-- | @Selector@ for @showWritingTools:@
showWritingToolsSelector :: Selector '[RawId] ()
showWritingToolsSelector = mkSelector "showWritingTools:"

-- | @Selector@ for @newWindowForTab:@
newWindowForTabSelector :: Selector '[RawId] ()
newWindowForTabSelector = mkSelector "newWindowForTab:"

-- | @Selector@ for @performTextFinderAction:@
performTextFinderActionSelector :: Selector '[RawId] ()
performTextFinderActionSelector = mkSelector "performTextFinderAction:"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector '[Id NSError, Id NSWindow, RawId, Sel, Ptr ()] ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector '[Id NSError] Bool
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector '[Id NSError] (Id NSError)
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @validateProposedFirstResponder:forEvent:@
validateProposedFirstResponder_forEventSelector :: Selector '[Id NSResponder, Id NSEvent] Bool
validateProposedFirstResponder_forEventSelector = mkSelector "validateProposedFirstResponder:forEvent:"

-- | @Selector@ for @nextResponder@
nextResponderSelector :: Selector '[] (Id NSResponder)
nextResponderSelector = mkSelector "nextResponder"

-- | @Selector@ for @setNextResponder:@
setNextResponderSelector :: Selector '[Id NSResponder] ()
setNextResponderSelector = mkSelector "setNextResponder:"

-- | @Selector@ for @acceptsFirstResponder@
acceptsFirstResponderSelector :: Selector '[] Bool
acceptsFirstResponderSelector = mkSelector "acceptsFirstResponder"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @restorableStateKeyPaths@
restorableStateKeyPathsSelector :: Selector '[] (Id NSArray)
restorableStateKeyPathsSelector = mkSelector "restorableStateKeyPaths"

-- | @Selector@ for @touchBar@
touchBarSelector :: Selector '[] (Id NSTouchBar)
touchBarSelector = mkSelector "touchBar"

-- | @Selector@ for @setTouchBar:@
setTouchBarSelector :: Selector '[Id NSTouchBar] ()
setTouchBarSelector = mkSelector "setTouchBar:"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector '[] (Id NSUserActivity)
userActivitySelector = mkSelector "userActivity"

-- | @Selector@ for @setUserActivity:@
setUserActivitySelector :: Selector '[Id NSUserActivity] ()
setUserActivitySelector = mkSelector "setUserActivity:"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector '[] (Id NSUndoManager)
undoManagerSelector = mkSelector "undoManager"

