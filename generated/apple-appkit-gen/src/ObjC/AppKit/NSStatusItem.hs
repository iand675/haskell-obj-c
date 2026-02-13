{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , button
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
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , image
  , setImage
  , alternateImage
  , setAlternateImage
  , enabled
  , setEnabled
  , highlightMode
  , setHighlightMode
  , toolTip
  , setToolTip
  , view
  , setView
  , actionSelector
  , alternateImageSelector
  , attributedTitleSelector
  , autosaveNameSelector
  , behaviorSelector
  , buttonSelector
  , doubleActionSelector
  , drawStatusBarBackgroundInRect_withHighlightSelector
  , enabledSelector
  , highlightModeSelector
  , imageSelector
  , lengthSelector
  , menuSelector
  , popUpStatusItemMenuSelector
  , sendActionOnSelector
  , setActionSelector
  , setAlternateImageSelector
  , setAttributedTitleSelector
  , setAutosaveNameSelector
  , setBehaviorSelector
  , setDoubleActionSelector
  , setEnabledSelector
  , setHighlightModeSelector
  , setImageSelector
  , setLengthSelector
  , setMenuSelector
  , setTargetSelector
  , setTitleSelector
  , setToolTipSelector
  , setViewSelector
  , setVisibleSelector
  , statusBarSelector
  , targetSelector
  , titleSelector
  , toolTipSelector
  , viewSelector
  , visibleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- sendActionOn:@
sendActionOn :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSEventMask -> IO CLong
sendActionOn nsStatusItem mask =
  sendMessage nsStatusItem sendActionOnSelector mask

-- | @- drawStatusBarBackgroundInRect:withHighlight:@
drawStatusBarBackgroundInRect_withHighlight :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSRect -> Bool -> IO ()
drawStatusBarBackgroundInRect_withHighlight nsStatusItem rect highlight =
  sendMessage nsStatusItem drawStatusBarBackgroundInRect_withHighlightSelector rect highlight

-- | @- popUpStatusItemMenu:@
popUpStatusItemMenu :: (IsNSStatusItem nsStatusItem, IsNSMenu menu) => nsStatusItem -> menu -> IO ()
popUpStatusItemMenu nsStatusItem menu =
  sendMessage nsStatusItem popUpStatusItemMenuSelector (toNSMenu menu)

-- | @- statusBar@
statusBar :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSStatusBar)
statusBar nsStatusItem =
  sendMessage nsStatusItem statusBarSelector

-- | @- length@
length_ :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO CDouble
length_ nsStatusItem =
  sendMessage nsStatusItem lengthSelector

-- | @- setLength:@
setLength :: IsNSStatusItem nsStatusItem => nsStatusItem -> CDouble -> IO ()
setLength nsStatusItem value =
  sendMessage nsStatusItem setLengthSelector value

-- | @- menu@
menu :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSMenu)
menu nsStatusItem =
  sendMessage nsStatusItem menuSelector

-- | @- setMenu:@
setMenu :: (IsNSStatusItem nsStatusItem, IsNSMenu value) => nsStatusItem -> value -> IO ()
setMenu nsStatusItem value =
  sendMessage nsStatusItem setMenuSelector (toNSMenu value)

-- | @- button@
button :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSStatusBarButton)
button nsStatusItem =
  sendMessage nsStatusItem buttonSelector

-- | @- behavior@
behavior :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO NSStatusItemBehavior
behavior nsStatusItem =
  sendMessage nsStatusItem behaviorSelector

-- | @- setBehavior:@
setBehavior :: IsNSStatusItem nsStatusItem => nsStatusItem -> NSStatusItemBehavior -> IO ()
setBehavior nsStatusItem value =
  sendMessage nsStatusItem setBehaviorSelector value

-- | @- visible@
visible :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
visible nsStatusItem =
  sendMessage nsStatusItem visibleSelector

-- | @- setVisible:@
setVisible :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setVisible nsStatusItem value =
  sendMessage nsStatusItem setVisibleSelector value

-- | @- autosaveName@
autosaveName :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSString)
autosaveName nsStatusItem =
  sendMessage nsStatusItem autosaveNameSelector

-- | @- setAutosaveName:@
setAutosaveName :: (IsNSStatusItem nsStatusItem, IsNSString value) => nsStatusItem -> value -> IO ()
setAutosaveName nsStatusItem value =
  sendMessage nsStatusItem setAutosaveNameSelector (toNSString value)

-- | @- action@
action :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Sel
action nsStatusItem =
  sendMessage nsStatusItem actionSelector

-- | @- setAction:@
setAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> Sel -> IO ()
setAction nsStatusItem value =
  sendMessage nsStatusItem setActionSelector value

-- | @- doubleAction@
doubleAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Sel
doubleAction nsStatusItem =
  sendMessage nsStatusItem doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSStatusItem nsStatusItem => nsStatusItem -> Sel -> IO ()
setDoubleAction nsStatusItem value =
  sendMessage nsStatusItem setDoubleActionSelector value

-- | @- target@
target :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO RawId
target nsStatusItem =
  sendMessage nsStatusItem targetSelector

-- | @- setTarget:@
setTarget :: IsNSStatusItem nsStatusItem => nsStatusItem -> RawId -> IO ()
setTarget nsStatusItem value =
  sendMessage nsStatusItem setTargetSelector value

-- | @- title@
title :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSString)
title nsStatusItem =
  sendMessage nsStatusItem titleSelector

-- | @- setTitle:@
setTitle :: (IsNSStatusItem nsStatusItem, IsNSString value) => nsStatusItem -> value -> IO ()
setTitle nsStatusItem value =
  sendMessage nsStatusItem setTitleSelector (toNSString value)

-- | @- attributedTitle@
attributedTitle :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSAttributedString)
attributedTitle nsStatusItem =
  sendMessage nsStatusItem attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSStatusItem nsStatusItem, IsNSAttributedString value) => nsStatusItem -> value -> IO ()
setAttributedTitle nsStatusItem value =
  sendMessage nsStatusItem setAttributedTitleSelector (toNSAttributedString value)

-- | @- image@
image :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSImage)
image nsStatusItem =
  sendMessage nsStatusItem imageSelector

-- | @- setImage:@
setImage :: (IsNSStatusItem nsStatusItem, IsNSImage value) => nsStatusItem -> value -> IO ()
setImage nsStatusItem value =
  sendMessage nsStatusItem setImageSelector (toNSImage value)

-- | @- alternateImage@
alternateImage :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSImage)
alternateImage nsStatusItem =
  sendMessage nsStatusItem alternateImageSelector

-- | @- setAlternateImage:@
setAlternateImage :: (IsNSStatusItem nsStatusItem, IsNSImage value) => nsStatusItem -> value -> IO ()
setAlternateImage nsStatusItem value =
  sendMessage nsStatusItem setAlternateImageSelector (toNSImage value)

-- | @- enabled@
enabled :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
enabled nsStatusItem =
  sendMessage nsStatusItem enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setEnabled nsStatusItem value =
  sendMessage nsStatusItem setEnabledSelector value

-- | @- highlightMode@
highlightMode :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO Bool
highlightMode nsStatusItem =
  sendMessage nsStatusItem highlightModeSelector

-- | @- setHighlightMode:@
setHighlightMode :: IsNSStatusItem nsStatusItem => nsStatusItem -> Bool -> IO ()
setHighlightMode nsStatusItem value =
  sendMessage nsStatusItem setHighlightModeSelector value

-- | @- toolTip@
toolTip :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSString)
toolTip nsStatusItem =
  sendMessage nsStatusItem toolTipSelector

-- | @- setToolTip:@
setToolTip :: (IsNSStatusItem nsStatusItem, IsNSString value) => nsStatusItem -> value -> IO ()
setToolTip nsStatusItem value =
  sendMessage nsStatusItem setToolTipSelector (toNSString value)

-- | @- view@
view :: IsNSStatusItem nsStatusItem => nsStatusItem -> IO (Id NSView)
view nsStatusItem =
  sendMessage nsStatusItem viewSelector

-- | @- setView:@
setView :: (IsNSStatusItem nsStatusItem, IsNSView value) => nsStatusItem -> value -> IO ()
setView nsStatusItem value =
  sendMessage nsStatusItem setViewSelector (toNSView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendActionOn:@
sendActionOnSelector :: Selector '[NSEventMask] CLong
sendActionOnSelector = mkSelector "sendActionOn:"

-- | @Selector@ for @drawStatusBarBackgroundInRect:withHighlight:@
drawStatusBarBackgroundInRect_withHighlightSelector :: Selector '[NSRect, Bool] ()
drawStatusBarBackgroundInRect_withHighlightSelector = mkSelector "drawStatusBarBackgroundInRect:withHighlight:"

-- | @Selector@ for @popUpStatusItemMenu:@
popUpStatusItemMenuSelector :: Selector '[Id NSMenu] ()
popUpStatusItemMenuSelector = mkSelector "popUpStatusItemMenu:"

-- | @Selector@ for @statusBar@
statusBarSelector :: Selector '[] (Id NSStatusBar)
statusBarSelector = mkSelector "statusBar"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CDouble
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CDouble] ()
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @button@
buttonSelector :: Selector '[] (Id NSStatusBarButton)
buttonSelector = mkSelector "button"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector '[] NSStatusItemBehavior
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector '[NSStatusItemBehavior] ()
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @setVisible:@
setVisibleSelector :: Selector '[Bool] ()
setVisibleSelector = mkSelector "setVisible:"

-- | @Selector@ for @autosaveName@
autosaveNameSelector :: Selector '[] (Id NSString)
autosaveNameSelector = mkSelector "autosaveName"

-- | @Selector@ for @setAutosaveName:@
setAutosaveNameSelector :: Selector '[Id NSString] ()
setAutosaveNameSelector = mkSelector "setAutosaveName:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector '[] (Id NSImage)
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @setAlternateImage:@
setAlternateImageSelector :: Selector '[Id NSImage] ()
setAlternateImageSelector = mkSelector "setAlternateImage:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @highlightMode@
highlightModeSelector :: Selector '[] Bool
highlightModeSelector = mkSelector "highlightMode"

-- | @Selector@ for @setHighlightMode:@
setHighlightModeSelector :: Selector '[Bool] ()
setHighlightModeSelector = mkSelector "setHighlightMode:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

