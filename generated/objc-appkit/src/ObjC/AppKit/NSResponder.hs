{-# LANGUAGE PatternSynonyms #-}
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
  , undoManager
  , initSelector
  , initWithCoderSelector
  , tryToPerform_withSelector
  , performKeyEquivalentSelector
  , validRequestorForSendType_returnTypeSelector
  , mouseDownSelector
  , rightMouseDownSelector
  , otherMouseDownSelector
  , mouseUpSelector
  , rightMouseUpSelector
  , otherMouseUpSelector
  , mouseMovedSelector
  , mouseDraggedSelector
  , mouseCancelledSelector
  , scrollWheelSelector
  , rightMouseDraggedSelector
  , otherMouseDraggedSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , keyDownSelector
  , keyUpSelector
  , flagsChangedSelector
  , tabletPointSelector
  , tabletProximitySelector
  , cursorUpdateSelector
  , magnifyWithEventSelector
  , rotateWithEventSelector
  , swipeWithEventSelector
  , beginGestureWithEventSelector
  , endGestureWithEventSelector
  , smartMagnifyWithEventSelector
  , changeModeWithEventSelector
  , touchesBeganWithEventSelector
  , touchesMovedWithEventSelector
  , touchesEndedWithEventSelector
  , touchesCancelledWithEventSelector
  , quickLookWithEventSelector
  , pressureChangeWithEventSelector
  , contextMenuKeyDownSelector
  , noResponderForSelector
  , becomeFirstResponderSelector
  , resignFirstResponderSelector
  , interpretKeyEventsSelector
  , flushBufferedKeyEventsSelector
  , showContextHelpSelector
  , helpRequestedSelector
  , shouldBeTreatedAsInkEventSelector
  , wantsScrollEventsForSwipeTrackingOnAxisSelector
  , wantsForwardedScrollEventsForAxisSelector
  , supplementalTargetForAction_senderSelector
  , encodeRestorableStateWithCoderSelector
  , encodeRestorableStateWithCoder_backgroundQueueSelector
  , restoreStateWithCoderSelector
  , invalidateRestorableStateSelector
  , allowedClassesForRestorableStateKeyPathSelector
  , interfaceStyleSelector
  , setInterfaceStyleSelector
  , makeTouchBarSelector
  , updateUserActivityStateSelector
  , performMnemonicSelector
  , showWritingToolsSelector
  , newWindowForTabSelector
  , performTextFinderActionSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , presentErrorSelector
  , willPresentErrorSelector
  , validateProposedFirstResponder_forEventSelector
  , nextResponderSelector
  , setNextResponderSelector
  , acceptsFirstResponderSelector
  , menuSelector
  , setMenuSelector
  , undoManagerSelector

  -- * Enum types
  , NSEventGestureAxis(NSEventGestureAxis)
  , pattern NSEventGestureAxisNone
  , pattern NSEventGestureAxisHorizontal
  , pattern NSEventGestureAxisVertical

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSResponder nsResponder => nsResponder -> IO (Id NSResponder)
init_ nsResponder  =
  sendMsg nsResponder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO (Id NSResponder)
initWithCoder nsResponder  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsResponder (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- tryToPerform:with:@
tryToPerform_with :: IsNSResponder nsResponder => nsResponder -> Selector -> RawId -> IO Bool
tryToPerform_with nsResponder  action object =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "tryToPerform:with:") retCULong [argPtr (unSelector action), argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO Bool
performKeyEquivalent nsResponder  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "performKeyEquivalent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSResponder nsResponder, IsNSString sendType, IsNSString returnType) => nsResponder -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsResponder  sendType returnType =
withObjCPtr sendType $ \raw_sendType ->
  withObjCPtr returnType $ \raw_returnType ->
      fmap (RawId . castPtr) $ sendMsg nsResponder (mkSelector "validRequestorForSendType:returnType:") (retPtr retVoid) [argPtr (castPtr raw_sendType :: Ptr ()), argPtr (castPtr raw_returnType :: Ptr ())]

-- | @- mouseDown:@
mouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseDown nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseDown:@
rightMouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseDown nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "rightMouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseDown:@
otherMouseDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseDown nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "otherMouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseUp:@
mouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseUp nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseUp:@
rightMouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseUp nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "rightMouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseUp:@
otherMouseUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseUp nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "otherMouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseMoved:@
mouseMoved :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseMoved nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseMoved:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseDragged:@
mouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseDragged nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseCancelled:@
mouseCancelled :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseCancelled nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseCancelled:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- scrollWheel:@
scrollWheel :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
scrollWheel nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "scrollWheel:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseDragged:@
rightMouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rightMouseDragged nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "rightMouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseDragged:@
otherMouseDragged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
otherMouseDragged nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "otherMouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseEntered:@
mouseEntered :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseEntered nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseEntered:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseExited:@
mouseExited :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
mouseExited nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "mouseExited:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- keyDown:@
keyDown :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
keyDown nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "keyDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- keyUp:@
keyUp :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
keyUp nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "keyUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- flagsChanged:@
flagsChanged :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
flagsChanged nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "flagsChanged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- tabletPoint:@
tabletPoint :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
tabletPoint nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "tabletPoint:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- tabletProximity:@
tabletProximity :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
tabletProximity nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "tabletProximity:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- cursorUpdate:@
cursorUpdate :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
cursorUpdate nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "cursorUpdate:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- magnifyWithEvent:@
magnifyWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
magnifyWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "magnifyWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rotateWithEvent:@
rotateWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
rotateWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "rotateWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- swipeWithEvent:@
swipeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
swipeWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "swipeWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- beginGestureWithEvent:@
beginGestureWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
beginGestureWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "beginGestureWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- endGestureWithEvent:@
endGestureWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
endGestureWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "endGestureWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- smartMagnifyWithEvent:@
smartMagnifyWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
smartMagnifyWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "smartMagnifyWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- changeModeWithEvent:@
changeModeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
changeModeWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "changeModeWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesBeganWithEvent:@
touchesBeganWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesBeganWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "touchesBeganWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesMovedWithEvent:@
touchesMovedWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesMovedWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "touchesMovedWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesEndedWithEvent:@
touchesEndedWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesEndedWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "touchesEndedWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesCancelledWithEvent:@
touchesCancelledWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
touchesCancelledWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "touchesCancelledWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- quickLookWithEvent:@
quickLookWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
quickLookWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "quickLookWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- pressureChangeWithEvent:@
pressureChangeWithEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO ()
pressureChangeWithEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "pressureChangeWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

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
contextMenuKeyDown nsResponder  event =
withObjCPtr event $ \raw_event ->
    sendMsg nsResponder (mkSelector "contextMenuKeyDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- noResponderFor:@
noResponderFor :: IsNSResponder nsResponder => nsResponder -> Selector -> IO ()
noResponderFor nsResponder  eventSelector =
  sendMsg nsResponder (mkSelector "noResponderFor:") retVoid [argPtr (unSelector eventSelector)]

-- | @- becomeFirstResponder@
becomeFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
becomeFirstResponder nsResponder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "becomeFirstResponder") retCULong []

-- | @- resignFirstResponder@
resignFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
resignFirstResponder nsResponder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "resignFirstResponder") retCULong []

-- | @- interpretKeyEvents:@
interpretKeyEvents :: (IsNSResponder nsResponder, IsNSArray eventArray) => nsResponder -> eventArray -> IO ()
interpretKeyEvents nsResponder  eventArray =
withObjCPtr eventArray $ \raw_eventArray ->
    sendMsg nsResponder (mkSelector "interpretKeyEvents:") retVoid [argPtr (castPtr raw_eventArray :: Ptr ())]

-- | @- flushBufferedKeyEvents@
flushBufferedKeyEvents :: IsNSResponder nsResponder => nsResponder -> IO ()
flushBufferedKeyEvents nsResponder  =
  sendMsg nsResponder (mkSelector "flushBufferedKeyEvents") retVoid []

-- | @- showContextHelp:@
showContextHelp :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
showContextHelp nsResponder  sender =
  sendMsg nsResponder (mkSelector "showContextHelp:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- helpRequested:@
helpRequested :: (IsNSResponder nsResponder, IsNSEvent eventPtr) => nsResponder -> eventPtr -> IO ()
helpRequested nsResponder  eventPtr =
withObjCPtr eventPtr $ \raw_eventPtr ->
    sendMsg nsResponder (mkSelector "helpRequested:") retVoid [argPtr (castPtr raw_eventPtr :: Ptr ())]

-- | @- shouldBeTreatedAsInkEvent:@
shouldBeTreatedAsInkEvent :: (IsNSResponder nsResponder, IsNSEvent event) => nsResponder -> event -> IO Bool
shouldBeTreatedAsInkEvent nsResponder  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "shouldBeTreatedAsInkEvent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- wantsScrollEventsForSwipeTrackingOnAxis:@
wantsScrollEventsForSwipeTrackingOnAxis :: IsNSResponder nsResponder => nsResponder -> NSEventGestureAxis -> IO Bool
wantsScrollEventsForSwipeTrackingOnAxis nsResponder  axis =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "wantsScrollEventsForSwipeTrackingOnAxis:") retCULong [argCLong (coerce axis)]

-- | @- wantsForwardedScrollEventsForAxis:@
wantsForwardedScrollEventsForAxis :: IsNSResponder nsResponder => nsResponder -> NSEventGestureAxis -> IO Bool
wantsForwardedScrollEventsForAxis nsResponder  axis =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "wantsForwardedScrollEventsForAxis:") retCULong [argCLong (coerce axis)]

-- | @- supplementalTargetForAction:sender:@
supplementalTargetForAction_sender :: IsNSResponder nsResponder => nsResponder -> Selector -> RawId -> IO RawId
supplementalTargetForAction_sender nsResponder  action sender =
  fmap (RawId . castPtr) $ sendMsg nsResponder (mkSelector "supplementalTargetForAction:sender:") (retPtr retVoid) [argPtr (unSelector action), argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO ()
encodeRestorableStateWithCoder nsResponder  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsResponder (mkSelector "encodeRestorableStateWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueue :: (IsNSResponder nsResponder, IsNSCoder coder, IsNSOperationQueue queue) => nsResponder -> coder -> queue -> IO ()
encodeRestorableStateWithCoder_backgroundQueue nsResponder  coder queue =
withObjCPtr coder $ \raw_coder ->
  withObjCPtr queue $ \raw_queue ->
      sendMsg nsResponder (mkSelector "encodeRestorableStateWithCoder:backgroundQueue:") retVoid [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | @- restoreStateWithCoder:@
restoreStateWithCoder :: (IsNSResponder nsResponder, IsNSCoder coder) => nsResponder -> coder -> IO ()
restoreStateWithCoder nsResponder  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsResponder (mkSelector "restoreStateWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- invalidateRestorableState@
invalidateRestorableState :: IsNSResponder nsResponder => nsResponder -> IO ()
invalidateRestorableState nsResponder  =
  sendMsg nsResponder (mkSelector "invalidateRestorableState") retVoid []

-- | When secure state restoration is used, values at restorableStateKeyPaths must support NSSecureCoding and this method will be consulted when restoring state. Any values not of an allowed class will not be set.
--
-- ObjC selector: @+ allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSArray)
allowedClassesForRestorableStateKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSResponder"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "allowedClassesForRestorableStateKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- interfaceStyle@
interfaceStyle :: IsNSResponder nsResponder => nsResponder -> IO CULong
interfaceStyle nsResponder  =
  sendMsg nsResponder (mkSelector "interfaceStyle") retCULong []

-- | @- setInterfaceStyle:@
setInterfaceStyle :: IsNSResponder nsResponder => nsResponder -> CULong -> IO ()
setInterfaceStyle nsResponder  interfaceStyle =
  sendMsg nsResponder (mkSelector "setInterfaceStyle:") retVoid [argCULong (fromIntegral interfaceStyle)]

-- | @- makeTouchBar@
makeTouchBar :: IsNSResponder nsResponder => nsResponder -> IO (Id NSTouchBar)
makeTouchBar nsResponder  =
  sendMsg nsResponder (mkSelector "makeTouchBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updateUserActivityState:@
updateUserActivityState :: (IsNSResponder nsResponder, IsNSUserActivity userActivity) => nsResponder -> userActivity -> IO ()
updateUserActivityState nsResponder  userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg nsResponder (mkSelector "updateUserActivityState:") retVoid [argPtr (castPtr raw_userActivity :: Ptr ())]

-- | @- performMnemonic:@
performMnemonic :: (IsNSResponder nsResponder, IsNSString string) => nsResponder -> string -> IO Bool
performMnemonic nsResponder  string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "performMnemonic:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- showWritingTools:@
showWritingTools :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
showWritingTools nsResponder  sender =
  sendMsg nsResponder (mkSelector "showWritingTools:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- newWindowForTab:@
newWindowForTab :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
newWindowForTab nsResponder  sender =
  sendMsg nsResponder (mkSelector "newWindowForTab:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- performTextFinderAction:@
performTextFinderAction :: IsNSResponder nsResponder => nsResponder -> RawId -> IO ()
performTextFinderAction nsResponder  sender =
  sendMsg nsResponder (mkSelector "performTextFinderAction:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSResponder nsResponder, IsNSError error_, IsNSWindow window) => nsResponder -> error_ -> window -> RawId -> Selector -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsResponder  error_ window delegate didPresentSelector contextInfo =
withObjCPtr error_ $ \raw_error_ ->
  withObjCPtr window $ \raw_window ->
      sendMsg nsResponder (mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:") retVoid [argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didPresentSelector), argPtr contextInfo]

-- | @- presentError:@
presentError :: (IsNSResponder nsResponder, IsNSError error_) => nsResponder -> error_ -> IO Bool
presentError nsResponder  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "presentError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- willPresentError:@
willPresentError :: (IsNSResponder nsResponder, IsNSError error_) => nsResponder -> error_ -> IO (Id NSError)
willPresentError nsResponder  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsResponder (mkSelector "willPresentError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- validateProposedFirstResponder:forEvent:@
validateProposedFirstResponder_forEvent :: (IsNSResponder nsResponder, IsNSResponder responder, IsNSEvent event) => nsResponder -> responder -> event -> IO Bool
validateProposedFirstResponder_forEvent nsResponder  responder event =
withObjCPtr responder $ \raw_responder ->
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "validateProposedFirstResponder:forEvent:") retCULong [argPtr (castPtr raw_responder :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- nextResponder@
nextResponder :: IsNSResponder nsResponder => nsResponder -> IO (Id NSResponder)
nextResponder nsResponder  =
  sendMsg nsResponder (mkSelector "nextResponder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextResponder:@
setNextResponder :: (IsNSResponder nsResponder, IsNSResponder value) => nsResponder -> value -> IO ()
setNextResponder nsResponder  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsResponder (mkSelector "setNextResponder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- acceptsFirstResponder@
acceptsFirstResponder :: IsNSResponder nsResponder => nsResponder -> IO Bool
acceptsFirstResponder nsResponder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsResponder (mkSelector "acceptsFirstResponder") retCULong []

-- | @- menu@
menu :: IsNSResponder nsResponder => nsResponder -> IO (Id NSMenu)
menu nsResponder  =
  sendMsg nsResponder (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenu:@
setMenu :: (IsNSResponder nsResponder, IsNSMenu value) => nsResponder -> value -> IO ()
setMenu nsResponder  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsResponder (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- undoManager@
undoManager :: IsNSResponder nsResponder => nsResponder -> IO (Id NSUndoManager)
undoManager nsResponder  =
  sendMsg nsResponder (mkSelector "undoManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @tryToPerform:with:@
tryToPerform_withSelector :: Selector
tryToPerform_withSelector = mkSelector "tryToPerform:with:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @rightMouseDown:@
rightMouseDownSelector :: Selector
rightMouseDownSelector = mkSelector "rightMouseDown:"

-- | @Selector@ for @otherMouseDown:@
otherMouseDownSelector :: Selector
otherMouseDownSelector = mkSelector "otherMouseDown:"

-- | @Selector@ for @mouseUp:@
mouseUpSelector :: Selector
mouseUpSelector = mkSelector "mouseUp:"

-- | @Selector@ for @rightMouseUp:@
rightMouseUpSelector :: Selector
rightMouseUpSelector = mkSelector "rightMouseUp:"

-- | @Selector@ for @otherMouseUp:@
otherMouseUpSelector :: Selector
otherMouseUpSelector = mkSelector "otherMouseUp:"

-- | @Selector@ for @mouseMoved:@
mouseMovedSelector :: Selector
mouseMovedSelector = mkSelector "mouseMoved:"

-- | @Selector@ for @mouseDragged:@
mouseDraggedSelector :: Selector
mouseDraggedSelector = mkSelector "mouseDragged:"

-- | @Selector@ for @mouseCancelled:@
mouseCancelledSelector :: Selector
mouseCancelledSelector = mkSelector "mouseCancelled:"

-- | @Selector@ for @scrollWheel:@
scrollWheelSelector :: Selector
scrollWheelSelector = mkSelector "scrollWheel:"

-- | @Selector@ for @rightMouseDragged:@
rightMouseDraggedSelector :: Selector
rightMouseDraggedSelector = mkSelector "rightMouseDragged:"

-- | @Selector@ for @otherMouseDragged:@
otherMouseDraggedSelector :: Selector
otherMouseDraggedSelector = mkSelector "otherMouseDragged:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @keyDown:@
keyDownSelector :: Selector
keyDownSelector = mkSelector "keyDown:"

-- | @Selector@ for @keyUp:@
keyUpSelector :: Selector
keyUpSelector = mkSelector "keyUp:"

-- | @Selector@ for @flagsChanged:@
flagsChangedSelector :: Selector
flagsChangedSelector = mkSelector "flagsChanged:"

-- | @Selector@ for @tabletPoint:@
tabletPointSelector :: Selector
tabletPointSelector = mkSelector "tabletPoint:"

-- | @Selector@ for @tabletProximity:@
tabletProximitySelector :: Selector
tabletProximitySelector = mkSelector "tabletProximity:"

-- | @Selector@ for @cursorUpdate:@
cursorUpdateSelector :: Selector
cursorUpdateSelector = mkSelector "cursorUpdate:"

-- | @Selector@ for @magnifyWithEvent:@
magnifyWithEventSelector :: Selector
magnifyWithEventSelector = mkSelector "magnifyWithEvent:"

-- | @Selector@ for @rotateWithEvent:@
rotateWithEventSelector :: Selector
rotateWithEventSelector = mkSelector "rotateWithEvent:"

-- | @Selector@ for @swipeWithEvent:@
swipeWithEventSelector :: Selector
swipeWithEventSelector = mkSelector "swipeWithEvent:"

-- | @Selector@ for @beginGestureWithEvent:@
beginGestureWithEventSelector :: Selector
beginGestureWithEventSelector = mkSelector "beginGestureWithEvent:"

-- | @Selector@ for @endGestureWithEvent:@
endGestureWithEventSelector :: Selector
endGestureWithEventSelector = mkSelector "endGestureWithEvent:"

-- | @Selector@ for @smartMagnifyWithEvent:@
smartMagnifyWithEventSelector :: Selector
smartMagnifyWithEventSelector = mkSelector "smartMagnifyWithEvent:"

-- | @Selector@ for @changeModeWithEvent:@
changeModeWithEventSelector :: Selector
changeModeWithEventSelector = mkSelector "changeModeWithEvent:"

-- | @Selector@ for @touchesBeganWithEvent:@
touchesBeganWithEventSelector :: Selector
touchesBeganWithEventSelector = mkSelector "touchesBeganWithEvent:"

-- | @Selector@ for @touchesMovedWithEvent:@
touchesMovedWithEventSelector :: Selector
touchesMovedWithEventSelector = mkSelector "touchesMovedWithEvent:"

-- | @Selector@ for @touchesEndedWithEvent:@
touchesEndedWithEventSelector :: Selector
touchesEndedWithEventSelector = mkSelector "touchesEndedWithEvent:"

-- | @Selector@ for @touchesCancelledWithEvent:@
touchesCancelledWithEventSelector :: Selector
touchesCancelledWithEventSelector = mkSelector "touchesCancelledWithEvent:"

-- | @Selector@ for @quickLookWithEvent:@
quickLookWithEventSelector :: Selector
quickLookWithEventSelector = mkSelector "quickLookWithEvent:"

-- | @Selector@ for @pressureChangeWithEvent:@
pressureChangeWithEventSelector :: Selector
pressureChangeWithEventSelector = mkSelector "pressureChangeWithEvent:"

-- | @Selector@ for @contextMenuKeyDown:@
contextMenuKeyDownSelector :: Selector
contextMenuKeyDownSelector = mkSelector "contextMenuKeyDown:"

-- | @Selector@ for @noResponderFor:@
noResponderForSelector :: Selector
noResponderForSelector = mkSelector "noResponderFor:"

-- | @Selector@ for @becomeFirstResponder@
becomeFirstResponderSelector :: Selector
becomeFirstResponderSelector = mkSelector "becomeFirstResponder"

-- | @Selector@ for @resignFirstResponder@
resignFirstResponderSelector :: Selector
resignFirstResponderSelector = mkSelector "resignFirstResponder"

-- | @Selector@ for @interpretKeyEvents:@
interpretKeyEventsSelector :: Selector
interpretKeyEventsSelector = mkSelector "interpretKeyEvents:"

-- | @Selector@ for @flushBufferedKeyEvents@
flushBufferedKeyEventsSelector :: Selector
flushBufferedKeyEventsSelector = mkSelector "flushBufferedKeyEvents"

-- | @Selector@ for @showContextHelp:@
showContextHelpSelector :: Selector
showContextHelpSelector = mkSelector "showContextHelp:"

-- | @Selector@ for @helpRequested:@
helpRequestedSelector :: Selector
helpRequestedSelector = mkSelector "helpRequested:"

-- | @Selector@ for @shouldBeTreatedAsInkEvent:@
shouldBeTreatedAsInkEventSelector :: Selector
shouldBeTreatedAsInkEventSelector = mkSelector "shouldBeTreatedAsInkEvent:"

-- | @Selector@ for @wantsScrollEventsForSwipeTrackingOnAxis:@
wantsScrollEventsForSwipeTrackingOnAxisSelector :: Selector
wantsScrollEventsForSwipeTrackingOnAxisSelector = mkSelector "wantsScrollEventsForSwipeTrackingOnAxis:"

-- | @Selector@ for @wantsForwardedScrollEventsForAxis:@
wantsForwardedScrollEventsForAxisSelector :: Selector
wantsForwardedScrollEventsForAxisSelector = mkSelector "wantsForwardedScrollEventsForAxis:"

-- | @Selector@ for @supplementalTargetForAction:sender:@
supplementalTargetForAction_senderSelector :: Selector
supplementalTargetForAction_senderSelector = mkSelector "supplementalTargetForAction:sender:"

-- | @Selector@ for @encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoderSelector :: Selector
encodeRestorableStateWithCoderSelector = mkSelector "encodeRestorableStateWithCoder:"

-- | @Selector@ for @encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueueSelector :: Selector
encodeRestorableStateWithCoder_backgroundQueueSelector = mkSelector "encodeRestorableStateWithCoder:backgroundQueue:"

-- | @Selector@ for @restoreStateWithCoder:@
restoreStateWithCoderSelector :: Selector
restoreStateWithCoderSelector = mkSelector "restoreStateWithCoder:"

-- | @Selector@ for @invalidateRestorableState@
invalidateRestorableStateSelector :: Selector
invalidateRestorableStateSelector = mkSelector "invalidateRestorableState"

-- | @Selector@ for @allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPathSelector :: Selector
allowedClassesForRestorableStateKeyPathSelector = mkSelector "allowedClassesForRestorableStateKeyPath:"

-- | @Selector@ for @interfaceStyle@
interfaceStyleSelector :: Selector
interfaceStyleSelector = mkSelector "interfaceStyle"

-- | @Selector@ for @setInterfaceStyle:@
setInterfaceStyleSelector :: Selector
setInterfaceStyleSelector = mkSelector "setInterfaceStyle:"

-- | @Selector@ for @makeTouchBar@
makeTouchBarSelector :: Selector
makeTouchBarSelector = mkSelector "makeTouchBar"

-- | @Selector@ for @updateUserActivityState:@
updateUserActivityStateSelector :: Selector
updateUserActivityStateSelector = mkSelector "updateUserActivityState:"

-- | @Selector@ for @performMnemonic:@
performMnemonicSelector :: Selector
performMnemonicSelector = mkSelector "performMnemonic:"

-- | @Selector@ for @showWritingTools:@
showWritingToolsSelector :: Selector
showWritingToolsSelector = mkSelector "showWritingTools:"

-- | @Selector@ for @newWindowForTab:@
newWindowForTabSelector :: Selector
newWindowForTabSelector = mkSelector "newWindowForTab:"

-- | @Selector@ for @performTextFinderAction:@
performTextFinderActionSelector :: Selector
performTextFinderActionSelector = mkSelector "performTextFinderAction:"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @validateProposedFirstResponder:forEvent:@
validateProposedFirstResponder_forEventSelector :: Selector
validateProposedFirstResponder_forEventSelector = mkSelector "validateProposedFirstResponder:forEvent:"

-- | @Selector@ for @nextResponder@
nextResponderSelector :: Selector
nextResponderSelector = mkSelector "nextResponder"

-- | @Selector@ for @setNextResponder:@
setNextResponderSelector :: Selector
setNextResponderSelector = mkSelector "setNextResponder:"

-- | @Selector@ for @acceptsFirstResponder@
acceptsFirstResponderSelector :: Selector
acceptsFirstResponderSelector = mkSelector "acceptsFirstResponder"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector
undoManagerSelector = mkSelector "undoManager"

