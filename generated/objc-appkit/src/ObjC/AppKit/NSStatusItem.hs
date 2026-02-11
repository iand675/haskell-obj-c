{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStatusItem@.
module ObjC.AppKit.NSStatusItem
  ( NSStatusItem
  , IsNSStatusItem(..)
  , sendActionOn
  , drawStatusBarBackgroundInRect_withHighlight
  , popUpStatusItemMenu
  , statusBar
  , length_
  , setLength
  , menu
  , setMenu
  , behavior
  , setBehavior
  , visible
  , setVisible
  , autosaveName
  , setAutosaveName
  , action
  , setAction
  , doubleAction
  , setDoubleAction
  , target
  , setTarget
  , enabled
  , setEnabled
  , highlightMode
  , setHighlightMode
  , sendActionOnSelector
  , drawStatusBarBackgroundInRect_withHighlightSelector
  , popUpStatusItemMenuSelector
  , statusBarSelector
  , lengthSelector
  , setLengthSelector
  , menuSelector
  , setMenuSelector
  , behaviorSelector
  , setBehaviorSelector
  , visibleSelector
  , setVisibleSelector
  , autosaveNameSelector
  , setAutosaveNameSelector
  , actionSelector
  , setActionSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , targetSelector
  , setTargetSelector
  , enabledSelector
  , setEnabledSelector
  , highlightModeSelector
  , setHighlightModeSelector

  -- * Enum types
  , NSEventMask(NSEventMask)
  , pattern NSEventMaskLeftMouseDown
  , pattern NSEventMaskLeftMouseUp
  , pattern NSEventMaskRightMouseDown
  , pattern NSEventMaskRightMouseUp
  , pattern NSEventMaskMouseMoved
  , pattern NSEventMaskLeftMouseDragged
  , pattern NSEventMaskRightMouseDragged
  , pattern NSEventMaskMouseEntered
  , pattern NSEventMaskMouseExited
  , pattern NSEventMaskKeyDown
  , pattern NSEventMaskKeyUp
  , pattern NSEventMaskFlagsChanged
  , pattern NSEventMaskAppKitDefined
  , pattern NSEventMaskSystemDefined
  , pattern NSEventMaskApplicationDefined
  , pattern NSEventMaskPeriodic
  , pattern NSEventMaskCursorUpdate
  , pattern NSEventMaskScrollWheel
  , pattern NSEventMaskTabletPoint
  , pattern NSEventMaskTabletProximity
  , pattern NSEventMaskOtherMouseDown
  , pattern NSEventMaskOtherMouseUp
  , pattern NSEventMaskOtherMouseDragged
  , pattern NSEventMaskGesture
  , pattern NSEventMaskMagnify
  , pattern NSEventMaskSwipe
  , pattern NSEventMaskRotate
  , pattern NSEventMaskBeginGesture
  , pattern NSEventMaskEndGesture
  , pattern NSEventMaskSmartMagnify
  , pattern NSEventMaskPressure
  , pattern NSEventMaskDirectTouch
  , pattern NSEventMaskChangeMode
  , pattern NSEventMaskMouseCancelled
  , pattern NSEventMaskAny
  , NSStatusItemBehavior(NSStatusItemBehavior)
  , pattern NSStatusItemBehaviorRemovalAllowed
  , pattern NSStatusItemBehaviorTerminationOnRemoval

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- sendActionOn:@
sendActionOn :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSEventMask -> IO CLong
sendActionOn nsStatusItem  mask =
  sendMsg nsStatusItem (mkSelector "sendActionOn:") retCLong [argCULong (coerce mask)]

-- | @- drawStatusBarBackgroundInRect:withHighlight:@
drawStatusBarBackgroundInRect_withHighlight :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSRect -> Bool -> IO ()
drawStatusBarBackgroundInRect_withHighlight nsStatusItem  rect highlight =
  sendMsg nsStatusItem (mkSelector "drawStatusBarBackgroundInRect:withHighlight:") retVoid [argNSRect rect, argCULong (if highlight then 1 else 0)]

-- | @- popUpStatusItemMenu:@
popUpStatusItemMenu :: (IsNSStatusItem nsStatusItem, IsNSMenu menu) => nsStatusItem -> menu -> IO ()
popUpStatusItemMenu nsStatusItem  menu =
withObjCPtr menu $ \raw_menu ->
    sendMsg nsStatusItem (mkSelector "popUpStatusItemMenu:") retVoid [argPtr (castPtr raw_menu :: Ptr ())]

-- | @- statusBar@
statusBar :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSStatusBar)
statusBar nsStatusItem  =
  sendMsg nsStatusItem (mkSelector "statusBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO CDouble
length_ nsStatusItem  =
  sendMsg nsStatusItem (mkSelector "length") retCDouble []

-- | @- setLength:@
setLength :: IsNSStatusItem nsStatusItem => nsStatusItem -> CDouble -> IO ()
setLength nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setLength:") retVoid [argCDouble (fromIntegral value)]

-- | @- menu@
menu :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSMenu)
menu nsStatusItem  =
  sendMsg nsStatusItem (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenu:@
setMenu :: (IsNSStatusItem nsStatusItem, IsNSMenu value) => nsStatusItem -> value -> IO ()
setMenu nsStatusItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsStatusItem (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- behavior@
behavior :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO NSStatusItemBehavior
behavior nsStatusItem  =
  fmap (coerce :: CULong -> NSStatusItemBehavior) $ sendMsg nsStatusItem (mkSelector "behavior") retCULong []

-- | @- setBehavior:@
setBehavior :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSStatusItemBehavior -> IO ()
setBehavior nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setBehavior:") retVoid [argCULong (coerce value)]

-- | @- visible@
visible :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
visible nsStatusItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStatusItem (mkSelector "visible") retCULong []

-- | @- setVisible:@
setVisible :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setVisible nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autosaveName@
autosaveName :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSString)
autosaveName nsStatusItem  =
  sendMsg nsStatusItem (mkSelector "autosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSStatusItem nsStatusItem, IsNSString value) => nsStatusItem -> value -> IO ()
setAutosaveName nsStatusItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsStatusItem (mkSelector "setAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- action@
action :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Selector
action nsStatusItem  =
  fmap (Selector . castPtr) $ sendMsg nsStatusItem (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> Selector -> IO ()
setAction nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- doubleAction@
doubleAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Selector
doubleAction nsStatusItem  =
  fmap (Selector . castPtr) $ sendMsg nsStatusItem (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> Selector -> IO ()
setDoubleAction nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- target@
target :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO RawId
target nsStatusItem  =
  fmap (RawId . castPtr) $ sendMsg nsStatusItem (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSStatusItem nsStatusItem => nsStatusItem -> RawId -> IO ()
setTarget nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- enabled@
enabled :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
enabled nsStatusItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStatusItem (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setEnabled nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlightMode@
highlightMode :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
highlightMode nsStatusItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsStatusItem (mkSelector "highlightMode") retCULong []

-- | @- setHighlightMode:@
setHighlightMode :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setHighlightMode nsStatusItem  value =
  sendMsg nsStatusItem (mkSelector "setHighlightMode:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @drawStatusBarBackgroundInRect:withHighlight:@
drawStatusBarBackgroundInRect_withHighlightSelector :: Selector
drawStatusBarBackgroundInRect_withHighlightSelector = mkSelector "drawStatusBarBackgroundInRect:withHighlight:"

-- | @Selector@ for @popUpStatusItemMenu:@
popUpStatusItemMenuSelector :: Selector
popUpStatusItemMenuSelector = mkSelector "popUpStatusItemMenu:"

-- | @Selector@ for @statusBar@
statusBarSelector :: Selector
statusBarSelector = mkSelector "statusBar"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @setVisible:@
setVisibleSelector :: Selector
setVisibleSelector = mkSelector "setVisible:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @highlightMode@
highlightModeSelector :: Selector
highlightModeSelector = mkSelector "highlightMode"

-- | @Selector@ for @setHighlightMode:@
setHighlightModeSelector :: Selector
setHighlightModeSelector = mkSelector "setHighlightMode:"

