{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCursor@.
module ObjC.AppKit.NSCursor
  ( NSCursor
  , IsNSCursor(..)
  , initWithImage_hotSpot
  , initWithCoder
  , hide
  , unhide
  , setHiddenUntilMouseMoves
  , nsCursorPop
  , pop
  , push
  , set
  , columnResizeCursorInDirections
  , rowResizeCursorInDirections
  , frameResizeCursorFromPosition_inDirections
  , initWithImage_foregroundColorHint_backgroundColorHint_hotSpot
  , setOnMouseExited
  , setOnMouseEntered
  , mouseEntered
  , mouseExited
  , image
  , hotSpot
  , currentCursor
  , arrowCursor
  , crosshairCursor
  , disappearingItemCursor
  , operationNotAllowedCursor
  , dragLinkCursor
  , dragCopyCursor
  , contextualMenuCursor
  , pointingHandCursor
  , closedHandCursor
  , openHandCursor
  , iBeamCursor
  , iBeamCursorForVerticalLayout
  , zoomInCursor
  , zoomOutCursor
  , columnResizeCursor
  , rowResizeCursor
  , currentSystemCursor
  , resizeLeftCursor
  , resizeRightCursor
  , resizeLeftRightCursor
  , resizeUpCursor
  , resizeDownCursor
  , resizeUpDownCursor
  , initWithImage_hotSpotSelector
  , initWithCoderSelector
  , hideSelector
  , unhideSelector
  , setHiddenUntilMouseMovesSelector
  , popSelector
  , pushSelector
  , setSelector
  , columnResizeCursorInDirectionsSelector
  , rowResizeCursorInDirectionsSelector
  , frameResizeCursorFromPosition_inDirectionsSelector
  , initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector
  , setOnMouseExitedSelector
  , setOnMouseEnteredSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , imageSelector
  , hotSpotSelector
  , currentCursorSelector
  , arrowCursorSelector
  , crosshairCursorSelector
  , disappearingItemCursorSelector
  , operationNotAllowedCursorSelector
  , dragLinkCursorSelector
  , dragCopyCursorSelector
  , contextualMenuCursorSelector
  , pointingHandCursorSelector
  , closedHandCursorSelector
  , openHandCursorSelector
  , iBeamCursorSelector
  , iBeamCursorForVerticalLayoutSelector
  , zoomInCursorSelector
  , zoomOutCursorSelector
  , columnResizeCursorSelector
  , rowResizeCursorSelector
  , currentSystemCursorSelector
  , resizeLeftCursorSelector
  , resizeRightCursorSelector
  , resizeLeftRightCursorSelector
  , resizeUpCursorSelector
  , resizeDownCursorSelector
  , resizeUpDownCursorSelector

  -- * Enum types
  , NSCursorFrameResizeDirections(NSCursorFrameResizeDirections)
  , pattern NSCursorFrameResizeDirectionsInward
  , pattern NSCursorFrameResizeDirectionsOutward
  , pattern NSCursorFrameResizeDirectionsAll
  , NSCursorFrameResizePosition(NSCursorFrameResizePosition)
  , pattern NSCursorFrameResizePositionTop
  , pattern NSCursorFrameResizePositionLeft
  , pattern NSCursorFrameResizePositionBottom
  , pattern NSCursorFrameResizePositionRight
  , pattern NSCursorFrameResizePositionTopLeft
  , pattern NSCursorFrameResizePositionTopRight
  , pattern NSCursorFrameResizePositionBottomLeft
  , pattern NSCursorFrameResizePositionBottomRight
  , NSHorizontalDirections(NSHorizontalDirections)
  , pattern NSHorizontalDirectionsLeft
  , pattern NSHorizontalDirectionsRight
  , pattern NSHorizontalDirectionsAll
  , NSVerticalDirections(NSVerticalDirections)
  , pattern NSVerticalDirectionsUp
  , pattern NSVerticalDirectionsDown
  , pattern NSVerticalDirectionsAll

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithImage:hotSpot:@
initWithImage_hotSpot :: (IsNSCursor nsCursor, IsNSImage newImage) => nsCursor -> newImage -> NSPoint -> IO (Id NSCursor)
initWithImage_hotSpot nsCursor  newImage point =
  withObjCPtr newImage $ \raw_newImage ->
      sendMsg nsCursor (mkSelector "initWithImage:hotSpot:") (retPtr retVoid) [argPtr (castPtr raw_newImage :: Ptr ()), argNSPoint point] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSCursor nsCursor, IsNSCoder coder) => nsCursor -> coder -> IO (Id NSCursor)
initWithCoder nsCursor  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsCursor (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ hide@
hide :: IO ()
hide  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "hide") retVoid []

-- | @+ unhide@
unhide :: IO ()
unhide  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "unhide") retVoid []

-- | @+ setHiddenUntilMouseMoves:@
setHiddenUntilMouseMoves :: Bool -> IO ()
setHiddenUntilMouseMoves flag =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "setHiddenUntilMouseMoves:") retVoid [argCULong (if flag then 1 else 0)]

-- | @+ pop@
nsCursorPop :: IO ()
nsCursorPop  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "pop") retVoid []

-- | @- pop@
pop :: IsNSCursor nsCursor => nsCursor -> IO ()
pop nsCursor  =
    sendMsg nsCursor (mkSelector "pop") retVoid []

-- | @- push@
push :: IsNSCursor nsCursor => nsCursor -> IO ()
push nsCursor  =
    sendMsg nsCursor (mkSelector "push") retVoid []

-- | @- set@
set :: IsNSCursor nsCursor => nsCursor -> IO ()
set nsCursor  =
    sendMsg nsCursor (mkSelector "set") retVoid []

-- | Returns the cursor for resizing a column (vertical divider) in the specified directions. - Parameter directions: The direction in which a column can be resized.
--
-- ObjC selector: @+ columnResizeCursorInDirections:@
columnResizeCursorInDirections :: NSHorizontalDirections -> IO (Id NSCursor)
columnResizeCursorInDirections directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "columnResizeCursorInDirections:") (retPtr retVoid) [argCULong (coerce directions)] >>= retainedObject . castPtr

-- | Returns the cursor for resizing a row (horizontal divider) in the specified directions. - Parameter directions: The direction in which a row can be resized.
--
-- ObjC selector: @+ rowResizeCursorInDirections:@
rowResizeCursorInDirections :: NSVerticalDirections -> IO (Id NSCursor)
rowResizeCursorInDirections directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "rowResizeCursorInDirections:") (retPtr retVoid) [argCULong (coerce directions)] >>= retainedObject . castPtr

-- | Returns the cursor for resizing a rectangular frame from the specified edge or corner. - Parameters:   - position: The position along the perimeter of a rectangular frame (its edges and corners) from which it’s resized.   - directions: The directions in which a rectangular frame can be resized.
--
-- ObjC selector: @+ frameResizeCursorFromPosition:inDirections:@
frameResizeCursorFromPosition_inDirections :: NSCursorFrameResizePosition -> NSCursorFrameResizeDirections -> IO (Id NSCursor)
frameResizeCursorFromPosition_inDirections position directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "frameResizeCursorFromPosition:inDirections:") (retPtr retVoid) [argCULong (coerce position), argCULong (coerce directions)] >>= retainedObject . castPtr

-- | @- initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:@
initWithImage_foregroundColorHint_backgroundColorHint_hotSpot :: (IsNSCursor nsCursor, IsNSImage newImage, IsNSColor fg, IsNSColor bg) => nsCursor -> newImage -> fg -> bg -> NSPoint -> IO (Id NSCursor)
initWithImage_foregroundColorHint_backgroundColorHint_hotSpot nsCursor  newImage fg bg hotSpot =
  withObjCPtr newImage $ \raw_newImage ->
    withObjCPtr fg $ \raw_fg ->
      withObjCPtr bg $ \raw_bg ->
          sendMsg nsCursor (mkSelector "initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:") (retPtr retVoid) [argPtr (castPtr raw_newImage :: Ptr ()), argPtr (castPtr raw_fg :: Ptr ()), argPtr (castPtr raw_bg :: Ptr ()), argNSPoint hotSpot] >>= ownedObject . castPtr

-- | @- setOnMouseExited:@
setOnMouseExited :: IsNSCursor nsCursor => nsCursor -> Bool -> IO ()
setOnMouseExited nsCursor  flag =
    sendMsg nsCursor (mkSelector "setOnMouseExited:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- setOnMouseEntered:@
setOnMouseEntered :: IsNSCursor nsCursor => nsCursor -> Bool -> IO ()
setOnMouseEntered nsCursor  flag =
    sendMsg nsCursor (mkSelector "setOnMouseEntered:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- mouseEntered:@
mouseEntered :: (IsNSCursor nsCursor, IsNSEvent event) => nsCursor -> event -> IO ()
mouseEntered nsCursor  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsCursor (mkSelector "mouseEntered:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseExited:@
mouseExited :: (IsNSCursor nsCursor, IsNSEvent event) => nsCursor -> event -> IO ()
mouseExited nsCursor  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsCursor (mkSelector "mouseExited:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- image@
image :: IsNSCursor nsCursor => nsCursor -> IO (Id NSImage)
image nsCursor  =
    sendMsg nsCursor (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hotSpot@
hotSpot :: IsNSCursor nsCursor => nsCursor -> IO NSPoint
hotSpot nsCursor  =
    sendMsgStret nsCursor (mkSelector "hotSpot") retNSPoint []

-- | Returns the application’s current cursor. - Note: This isn’t necessarily the cursor that is currently being displayed, as the system may be showing the cursor for another running application.
--
-- ObjC selector: @+ currentCursor@
currentCursor :: IO (Id NSCursor)
currentCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "currentCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the default cursor, the arrow cursor. - Discussion: The default cursor, a slanted arrow with its hot spot at the tip. The arrow cursor is the one you’re used to seeing over buttons, scrollers, and many other objects in the window system.
--
-- ObjC selector: @+ arrowCursor@
arrowCursor :: IO (Id NSCursor)
arrowCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "arrowCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ crosshairCursor@
crosshairCursor :: IO (Id NSCursor)
crosshairCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "crosshairCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ disappearingItemCursor@
disappearingItemCursor :: IO (Id NSCursor)
disappearingItemCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "disappearingItemCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ operationNotAllowedCursor@
operationNotAllowedCursor :: IO (Id NSCursor)
operationNotAllowedCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "operationNotAllowedCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dragLinkCursor@
dragLinkCursor :: IO (Id NSCursor)
dragLinkCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "dragLinkCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dragCopyCursor@
dragCopyCursor :: IO (Id NSCursor)
dragCopyCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "dragCopyCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ contextualMenuCursor@
contextualMenuCursor :: IO (Id NSCursor)
contextualMenuCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "contextualMenuCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pointingHandCursor@
pointingHandCursor :: IO (Id NSCursor)
pointingHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "pointingHandCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ closedHandCursor@
closedHandCursor :: IO (Id NSCursor)
closedHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "closedHandCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ openHandCursor@
openHandCursor :: IO (Id NSCursor)
openHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "openHandCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ IBeamCursor@
iBeamCursor :: IO (Id NSCursor)
iBeamCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "IBeamCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ IBeamCursorForVerticalLayout@
iBeamCursorForVerticalLayout :: IO (Id NSCursor)
iBeamCursorForVerticalLayout  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "IBeamCursorForVerticalLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the zoom-in cursor. - Note: This cursor is used to indicate zooming in on (magnifying) a canvas or object.
--
-- ObjC selector: @+ zoomInCursor@
zoomInCursor :: IO (Id NSCursor)
zoomInCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "zoomInCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the zoom-out cursor. - Note: This cursor is used to indicate zooming out of a canvas or object.
--
-- ObjC selector: @+ zoomOutCursor@
zoomOutCursor :: IO (Id NSCursor)
zoomOutCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "zoomOutCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the cursor for resizing a column (vertical divider) in either direction.
--
-- ObjC selector: @+ columnResizeCursor@
columnResizeCursor :: IO (Id NSCursor)
columnResizeCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "columnResizeCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the cursor for resizing a row (horizontal divider) in either direction.
--
-- ObjC selector: @+ rowResizeCursor@
rowResizeCursor :: IO (Id NSCursor)
rowResizeCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "rowResizeCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property will always be @nil@ in a future version of macOS.
--
-- ObjC selector: @+ currentSystemCursor@
currentSystemCursor :: IO (Id NSCursor)
currentSystemCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "currentSystemCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeLeftCursor@
resizeLeftCursor :: IO (Id NSCursor)
resizeLeftCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeLeftCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeRightCursor@
resizeRightCursor :: IO (Id NSCursor)
resizeRightCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeRightCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeLeftRightCursor@
resizeLeftRightCursor :: IO (Id NSCursor)
resizeLeftRightCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeLeftRightCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeUpCursor@
resizeUpCursor :: IO (Id NSCursor)
resizeUpCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeUpCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeDownCursor@
resizeDownCursor :: IO (Id NSCursor)
resizeDownCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeDownCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ resizeUpDownCursor@
resizeUpDownCursor :: IO (Id NSCursor)
resizeUpDownCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "resizeUpDownCursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImage:hotSpot:@
initWithImage_hotSpotSelector :: Selector
initWithImage_hotSpotSelector = mkSelector "initWithImage:hotSpot:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @hide@
hideSelector :: Selector
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @setHiddenUntilMouseMoves:@
setHiddenUntilMouseMovesSelector :: Selector
setHiddenUntilMouseMovesSelector = mkSelector "setHiddenUntilMouseMoves:"

-- | @Selector@ for @pop@
popSelector :: Selector
popSelector = mkSelector "pop"

-- | @Selector@ for @push@
pushSelector :: Selector
pushSelector = mkSelector "push"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @columnResizeCursorInDirections:@
columnResizeCursorInDirectionsSelector :: Selector
columnResizeCursorInDirectionsSelector = mkSelector "columnResizeCursorInDirections:"

-- | @Selector@ for @rowResizeCursorInDirections:@
rowResizeCursorInDirectionsSelector :: Selector
rowResizeCursorInDirectionsSelector = mkSelector "rowResizeCursorInDirections:"

-- | @Selector@ for @frameResizeCursorFromPosition:inDirections:@
frameResizeCursorFromPosition_inDirectionsSelector :: Selector
frameResizeCursorFromPosition_inDirectionsSelector = mkSelector "frameResizeCursorFromPosition:inDirections:"

-- | @Selector@ for @initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:@
initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector :: Selector
initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector = mkSelector "initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:"

-- | @Selector@ for @setOnMouseExited:@
setOnMouseExitedSelector :: Selector
setOnMouseExitedSelector = mkSelector "setOnMouseExited:"

-- | @Selector@ for @setOnMouseEntered:@
setOnMouseEnteredSelector :: Selector
setOnMouseEnteredSelector = mkSelector "setOnMouseEntered:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @hotSpot@
hotSpotSelector :: Selector
hotSpotSelector = mkSelector "hotSpot"

-- | @Selector@ for @currentCursor@
currentCursorSelector :: Selector
currentCursorSelector = mkSelector "currentCursor"

-- | @Selector@ for @arrowCursor@
arrowCursorSelector :: Selector
arrowCursorSelector = mkSelector "arrowCursor"

-- | @Selector@ for @crosshairCursor@
crosshairCursorSelector :: Selector
crosshairCursorSelector = mkSelector "crosshairCursor"

-- | @Selector@ for @disappearingItemCursor@
disappearingItemCursorSelector :: Selector
disappearingItemCursorSelector = mkSelector "disappearingItemCursor"

-- | @Selector@ for @operationNotAllowedCursor@
operationNotAllowedCursorSelector :: Selector
operationNotAllowedCursorSelector = mkSelector "operationNotAllowedCursor"

-- | @Selector@ for @dragLinkCursor@
dragLinkCursorSelector :: Selector
dragLinkCursorSelector = mkSelector "dragLinkCursor"

-- | @Selector@ for @dragCopyCursor@
dragCopyCursorSelector :: Selector
dragCopyCursorSelector = mkSelector "dragCopyCursor"

-- | @Selector@ for @contextualMenuCursor@
contextualMenuCursorSelector :: Selector
contextualMenuCursorSelector = mkSelector "contextualMenuCursor"

-- | @Selector@ for @pointingHandCursor@
pointingHandCursorSelector :: Selector
pointingHandCursorSelector = mkSelector "pointingHandCursor"

-- | @Selector@ for @closedHandCursor@
closedHandCursorSelector :: Selector
closedHandCursorSelector = mkSelector "closedHandCursor"

-- | @Selector@ for @openHandCursor@
openHandCursorSelector :: Selector
openHandCursorSelector = mkSelector "openHandCursor"

-- | @Selector@ for @IBeamCursor@
iBeamCursorSelector :: Selector
iBeamCursorSelector = mkSelector "IBeamCursor"

-- | @Selector@ for @IBeamCursorForVerticalLayout@
iBeamCursorForVerticalLayoutSelector :: Selector
iBeamCursorForVerticalLayoutSelector = mkSelector "IBeamCursorForVerticalLayout"

-- | @Selector@ for @zoomInCursor@
zoomInCursorSelector :: Selector
zoomInCursorSelector = mkSelector "zoomInCursor"

-- | @Selector@ for @zoomOutCursor@
zoomOutCursorSelector :: Selector
zoomOutCursorSelector = mkSelector "zoomOutCursor"

-- | @Selector@ for @columnResizeCursor@
columnResizeCursorSelector :: Selector
columnResizeCursorSelector = mkSelector "columnResizeCursor"

-- | @Selector@ for @rowResizeCursor@
rowResizeCursorSelector :: Selector
rowResizeCursorSelector = mkSelector "rowResizeCursor"

-- | @Selector@ for @currentSystemCursor@
currentSystemCursorSelector :: Selector
currentSystemCursorSelector = mkSelector "currentSystemCursor"

-- | @Selector@ for @resizeLeftCursor@
resizeLeftCursorSelector :: Selector
resizeLeftCursorSelector = mkSelector "resizeLeftCursor"

-- | @Selector@ for @resizeRightCursor@
resizeRightCursorSelector :: Selector
resizeRightCursorSelector = mkSelector "resizeRightCursor"

-- | @Selector@ for @resizeLeftRightCursor@
resizeLeftRightCursorSelector :: Selector
resizeLeftRightCursorSelector = mkSelector "resizeLeftRightCursor"

-- | @Selector@ for @resizeUpCursor@
resizeUpCursorSelector :: Selector
resizeUpCursorSelector = mkSelector "resizeUpCursor"

-- | @Selector@ for @resizeDownCursor@
resizeDownCursorSelector :: Selector
resizeDownCursorSelector = mkSelector "resizeDownCursor"

-- | @Selector@ for @resizeUpDownCursor@
resizeUpDownCursorSelector :: Selector
resizeUpDownCursorSelector = mkSelector "resizeUpDownCursor"

