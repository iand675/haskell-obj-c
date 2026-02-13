{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , arrowCursorSelector
  , closedHandCursorSelector
  , columnResizeCursorInDirectionsSelector
  , columnResizeCursorSelector
  , contextualMenuCursorSelector
  , crosshairCursorSelector
  , currentCursorSelector
  , currentSystemCursorSelector
  , disappearingItemCursorSelector
  , dragCopyCursorSelector
  , dragLinkCursorSelector
  , frameResizeCursorFromPosition_inDirectionsSelector
  , hideSelector
  , hotSpotSelector
  , iBeamCursorForVerticalLayoutSelector
  , iBeamCursorSelector
  , imageSelector
  , initWithCoderSelector
  , initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector
  , initWithImage_hotSpotSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , nsCursorPopSelector
  , openHandCursorSelector
  , operationNotAllowedCursorSelector
  , pointingHandCursorSelector
  , popSelector
  , pushSelector
  , resizeDownCursorSelector
  , resizeLeftCursorSelector
  , resizeLeftRightCursorSelector
  , resizeRightCursorSelector
  , resizeUpCursorSelector
  , resizeUpDownCursorSelector
  , rowResizeCursorInDirectionsSelector
  , rowResizeCursorSelector
  , setHiddenUntilMouseMovesSelector
  , setOnMouseEnteredSelector
  , setOnMouseExitedSelector
  , setSelector
  , unhideSelector
  , zoomInCursorSelector
  , zoomOutCursorSelector

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

-- | @- initWithImage:hotSpot:@
initWithImage_hotSpot :: (IsNSCursor nsCursor, IsNSImage newImage) => nsCursor -> newImage -> NSPoint -> IO (Id NSCursor)
initWithImage_hotSpot nsCursor newImage point =
  sendOwnedMessage nsCursor initWithImage_hotSpotSelector (toNSImage newImage) point

-- | @- initWithCoder:@
initWithCoder :: (IsNSCursor nsCursor, IsNSCoder coder) => nsCursor -> coder -> IO (Id NSCursor)
initWithCoder nsCursor coder =
  sendOwnedMessage nsCursor initWithCoderSelector (toNSCoder coder)

-- | @+ hide@
hide :: IO ()
hide  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' hideSelector

-- | @+ unhide@
unhide :: IO ()
unhide  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' unhideSelector

-- | @+ setHiddenUntilMouseMoves:@
setHiddenUntilMouseMoves :: Bool -> IO ()
setHiddenUntilMouseMoves flag =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' setHiddenUntilMouseMovesSelector flag

-- | @+ pop@
nsCursorPop :: IO ()
nsCursorPop  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' nsCursorPopSelector

-- | @- pop@
pop :: IsNSCursor nsCursor => nsCursor -> IO ()
pop nsCursor =
  sendMessage nsCursor popSelector

-- | @- push@
push :: IsNSCursor nsCursor => nsCursor -> IO ()
push nsCursor =
  sendMessage nsCursor pushSelector

-- | @- set@
set :: IsNSCursor nsCursor => nsCursor -> IO ()
set nsCursor =
  sendMessage nsCursor setSelector

-- | Returns the cursor for resizing a column (vertical divider) in the specified directions. - Parameter directions: The direction in which a column can be resized.
--
-- ObjC selector: @+ columnResizeCursorInDirections:@
columnResizeCursorInDirections :: NSHorizontalDirections -> IO (Id NSCursor)
columnResizeCursorInDirections directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' columnResizeCursorInDirectionsSelector directions

-- | Returns the cursor for resizing a row (horizontal divider) in the specified directions. - Parameter directions: The direction in which a row can be resized.
--
-- ObjC selector: @+ rowResizeCursorInDirections:@
rowResizeCursorInDirections :: NSVerticalDirections -> IO (Id NSCursor)
rowResizeCursorInDirections directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' rowResizeCursorInDirectionsSelector directions

-- | Returns the cursor for resizing a rectangular frame from the specified edge or corner. - Parameters:   - position: The position along the perimeter of a rectangular frame (its edges and corners) from which it’s resized.   - directions: The directions in which a rectangular frame can be resized.
--
-- ObjC selector: @+ frameResizeCursorFromPosition:inDirections:@
frameResizeCursorFromPosition_inDirections :: NSCursorFrameResizePosition -> NSCursorFrameResizeDirections -> IO (Id NSCursor)
frameResizeCursorFromPosition_inDirections position directions =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' frameResizeCursorFromPosition_inDirectionsSelector position directions

-- | @- initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:@
initWithImage_foregroundColorHint_backgroundColorHint_hotSpot :: (IsNSCursor nsCursor, IsNSImage newImage, IsNSColor fg, IsNSColor bg) => nsCursor -> newImage -> fg -> bg -> NSPoint -> IO (Id NSCursor)
initWithImage_foregroundColorHint_backgroundColorHint_hotSpot nsCursor newImage fg bg hotSpot =
  sendOwnedMessage nsCursor initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector (toNSImage newImage) (toNSColor fg) (toNSColor bg) hotSpot

-- | @- setOnMouseExited:@
setOnMouseExited :: IsNSCursor nsCursor => nsCursor -> Bool -> IO ()
setOnMouseExited nsCursor flag =
  sendMessage nsCursor setOnMouseExitedSelector flag

-- | @- setOnMouseEntered:@
setOnMouseEntered :: IsNSCursor nsCursor => nsCursor -> Bool -> IO ()
setOnMouseEntered nsCursor flag =
  sendMessage nsCursor setOnMouseEnteredSelector flag

-- | @- mouseEntered:@
mouseEntered :: (IsNSCursor nsCursor, IsNSEvent event) => nsCursor -> event -> IO ()
mouseEntered nsCursor event =
  sendMessage nsCursor mouseEnteredSelector (toNSEvent event)

-- | @- mouseExited:@
mouseExited :: (IsNSCursor nsCursor, IsNSEvent event) => nsCursor -> event -> IO ()
mouseExited nsCursor event =
  sendMessage nsCursor mouseExitedSelector (toNSEvent event)

-- | @- image@
image :: IsNSCursor nsCursor => nsCursor -> IO (Id NSImage)
image nsCursor =
  sendMessage nsCursor imageSelector

-- | @- hotSpot@
hotSpot :: IsNSCursor nsCursor => nsCursor -> IO NSPoint
hotSpot nsCursor =
  sendMessage nsCursor hotSpotSelector

-- | Returns the application’s current cursor. - Note: This isn’t necessarily the cursor that is currently being displayed, as the system may be showing the cursor for another running application.
--
-- ObjC selector: @+ currentCursor@
currentCursor :: IO (Id NSCursor)
currentCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' currentCursorSelector

-- | Returns the default cursor, the arrow cursor. - Discussion: The default cursor, a slanted arrow with its hot spot at the tip. The arrow cursor is the one you’re used to seeing over buttons, scrollers, and many other objects in the window system.
--
-- ObjC selector: @+ arrowCursor@
arrowCursor :: IO (Id NSCursor)
arrowCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' arrowCursorSelector

-- | @+ crosshairCursor@
crosshairCursor :: IO (Id NSCursor)
crosshairCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' crosshairCursorSelector

-- | @+ disappearingItemCursor@
disappearingItemCursor :: IO (Id NSCursor)
disappearingItemCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' disappearingItemCursorSelector

-- | @+ operationNotAllowedCursor@
operationNotAllowedCursor :: IO (Id NSCursor)
operationNotAllowedCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' operationNotAllowedCursorSelector

-- | @+ dragLinkCursor@
dragLinkCursor :: IO (Id NSCursor)
dragLinkCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' dragLinkCursorSelector

-- | @+ dragCopyCursor@
dragCopyCursor :: IO (Id NSCursor)
dragCopyCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' dragCopyCursorSelector

-- | @+ contextualMenuCursor@
contextualMenuCursor :: IO (Id NSCursor)
contextualMenuCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' contextualMenuCursorSelector

-- | @+ pointingHandCursor@
pointingHandCursor :: IO (Id NSCursor)
pointingHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' pointingHandCursorSelector

-- | @+ closedHandCursor@
closedHandCursor :: IO (Id NSCursor)
closedHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' closedHandCursorSelector

-- | @+ openHandCursor@
openHandCursor :: IO (Id NSCursor)
openHandCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' openHandCursorSelector

-- | @+ IBeamCursor@
iBeamCursor :: IO (Id NSCursor)
iBeamCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' iBeamCursorSelector

-- | @+ IBeamCursorForVerticalLayout@
iBeamCursorForVerticalLayout :: IO (Id NSCursor)
iBeamCursorForVerticalLayout  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' iBeamCursorForVerticalLayoutSelector

-- | Returns the zoom-in cursor. - Note: This cursor is used to indicate zooming in on (magnifying) a canvas or object.
--
-- ObjC selector: @+ zoomInCursor@
zoomInCursor :: IO (Id NSCursor)
zoomInCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' zoomInCursorSelector

-- | Returns the zoom-out cursor. - Note: This cursor is used to indicate zooming out of a canvas or object.
--
-- ObjC selector: @+ zoomOutCursor@
zoomOutCursor :: IO (Id NSCursor)
zoomOutCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' zoomOutCursorSelector

-- | Returns the cursor for resizing a column (vertical divider) in either direction.
--
-- ObjC selector: @+ columnResizeCursor@
columnResizeCursor :: IO (Id NSCursor)
columnResizeCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' columnResizeCursorSelector

-- | Returns the cursor for resizing a row (horizontal divider) in either direction.
--
-- ObjC selector: @+ rowResizeCursor@
rowResizeCursor :: IO (Id NSCursor)
rowResizeCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' rowResizeCursorSelector

-- | This property will always be @nil@ in a future version of macOS.
--
-- ObjC selector: @+ currentSystemCursor@
currentSystemCursor :: IO (Id NSCursor)
currentSystemCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' currentSystemCursorSelector

-- | @+ resizeLeftCursor@
resizeLeftCursor :: IO (Id NSCursor)
resizeLeftCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeLeftCursorSelector

-- | @+ resizeRightCursor@
resizeRightCursor :: IO (Id NSCursor)
resizeRightCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeRightCursorSelector

-- | @+ resizeLeftRightCursor@
resizeLeftRightCursor :: IO (Id NSCursor)
resizeLeftRightCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeLeftRightCursorSelector

-- | @+ resizeUpCursor@
resizeUpCursor :: IO (Id NSCursor)
resizeUpCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeUpCursorSelector

-- | @+ resizeDownCursor@
resizeDownCursor :: IO (Id NSCursor)
resizeDownCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeDownCursorSelector

-- | @+ resizeUpDownCursor@
resizeUpDownCursor :: IO (Id NSCursor)
resizeUpDownCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' resizeUpDownCursorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImage:hotSpot:@
initWithImage_hotSpotSelector :: Selector '[Id NSImage, NSPoint] (Id NSCursor)
initWithImage_hotSpotSelector = mkSelector "initWithImage:hotSpot:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSCursor)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @hide@
hideSelector :: Selector '[] ()
hideSelector = mkSelector "hide"

-- | @Selector@ for @unhide@
unhideSelector :: Selector '[] ()
unhideSelector = mkSelector "unhide"

-- | @Selector@ for @setHiddenUntilMouseMoves:@
setHiddenUntilMouseMovesSelector :: Selector '[Bool] ()
setHiddenUntilMouseMovesSelector = mkSelector "setHiddenUntilMouseMoves:"

-- | @Selector@ for @pop@
nsCursorPopSelector :: Selector '[] ()
nsCursorPopSelector = mkSelector "pop"

-- | @Selector@ for @pop@
popSelector :: Selector '[] ()
popSelector = mkSelector "pop"

-- | @Selector@ for @push@
pushSelector :: Selector '[] ()
pushSelector = mkSelector "push"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @columnResizeCursorInDirections:@
columnResizeCursorInDirectionsSelector :: Selector '[NSHorizontalDirections] (Id NSCursor)
columnResizeCursorInDirectionsSelector = mkSelector "columnResizeCursorInDirections:"

-- | @Selector@ for @rowResizeCursorInDirections:@
rowResizeCursorInDirectionsSelector :: Selector '[NSVerticalDirections] (Id NSCursor)
rowResizeCursorInDirectionsSelector = mkSelector "rowResizeCursorInDirections:"

-- | @Selector@ for @frameResizeCursorFromPosition:inDirections:@
frameResizeCursorFromPosition_inDirectionsSelector :: Selector '[NSCursorFrameResizePosition, NSCursorFrameResizeDirections] (Id NSCursor)
frameResizeCursorFromPosition_inDirectionsSelector = mkSelector "frameResizeCursorFromPosition:inDirections:"

-- | @Selector@ for @initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:@
initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector :: Selector '[Id NSImage, Id NSColor, Id NSColor, NSPoint] (Id NSCursor)
initWithImage_foregroundColorHint_backgroundColorHint_hotSpotSelector = mkSelector "initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:"

-- | @Selector@ for @setOnMouseExited:@
setOnMouseExitedSelector :: Selector '[Bool] ()
setOnMouseExitedSelector = mkSelector "setOnMouseExited:"

-- | @Selector@ for @setOnMouseEntered:@
setOnMouseEnteredSelector :: Selector '[Bool] ()
setOnMouseEnteredSelector = mkSelector "setOnMouseEntered:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector '[Id NSEvent] ()
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector '[Id NSEvent] ()
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @hotSpot@
hotSpotSelector :: Selector '[] NSPoint
hotSpotSelector = mkSelector "hotSpot"

-- | @Selector@ for @currentCursor@
currentCursorSelector :: Selector '[] (Id NSCursor)
currentCursorSelector = mkSelector "currentCursor"

-- | @Selector@ for @arrowCursor@
arrowCursorSelector :: Selector '[] (Id NSCursor)
arrowCursorSelector = mkSelector "arrowCursor"

-- | @Selector@ for @crosshairCursor@
crosshairCursorSelector :: Selector '[] (Id NSCursor)
crosshairCursorSelector = mkSelector "crosshairCursor"

-- | @Selector@ for @disappearingItemCursor@
disappearingItemCursorSelector :: Selector '[] (Id NSCursor)
disappearingItemCursorSelector = mkSelector "disappearingItemCursor"

-- | @Selector@ for @operationNotAllowedCursor@
operationNotAllowedCursorSelector :: Selector '[] (Id NSCursor)
operationNotAllowedCursorSelector = mkSelector "operationNotAllowedCursor"

-- | @Selector@ for @dragLinkCursor@
dragLinkCursorSelector :: Selector '[] (Id NSCursor)
dragLinkCursorSelector = mkSelector "dragLinkCursor"

-- | @Selector@ for @dragCopyCursor@
dragCopyCursorSelector :: Selector '[] (Id NSCursor)
dragCopyCursorSelector = mkSelector "dragCopyCursor"

-- | @Selector@ for @contextualMenuCursor@
contextualMenuCursorSelector :: Selector '[] (Id NSCursor)
contextualMenuCursorSelector = mkSelector "contextualMenuCursor"

-- | @Selector@ for @pointingHandCursor@
pointingHandCursorSelector :: Selector '[] (Id NSCursor)
pointingHandCursorSelector = mkSelector "pointingHandCursor"

-- | @Selector@ for @closedHandCursor@
closedHandCursorSelector :: Selector '[] (Id NSCursor)
closedHandCursorSelector = mkSelector "closedHandCursor"

-- | @Selector@ for @openHandCursor@
openHandCursorSelector :: Selector '[] (Id NSCursor)
openHandCursorSelector = mkSelector "openHandCursor"

-- | @Selector@ for @IBeamCursor@
iBeamCursorSelector :: Selector '[] (Id NSCursor)
iBeamCursorSelector = mkSelector "IBeamCursor"

-- | @Selector@ for @IBeamCursorForVerticalLayout@
iBeamCursorForVerticalLayoutSelector :: Selector '[] (Id NSCursor)
iBeamCursorForVerticalLayoutSelector = mkSelector "IBeamCursorForVerticalLayout"

-- | @Selector@ for @zoomInCursor@
zoomInCursorSelector :: Selector '[] (Id NSCursor)
zoomInCursorSelector = mkSelector "zoomInCursor"

-- | @Selector@ for @zoomOutCursor@
zoomOutCursorSelector :: Selector '[] (Id NSCursor)
zoomOutCursorSelector = mkSelector "zoomOutCursor"

-- | @Selector@ for @columnResizeCursor@
columnResizeCursorSelector :: Selector '[] (Id NSCursor)
columnResizeCursorSelector = mkSelector "columnResizeCursor"

-- | @Selector@ for @rowResizeCursor@
rowResizeCursorSelector :: Selector '[] (Id NSCursor)
rowResizeCursorSelector = mkSelector "rowResizeCursor"

-- | @Selector@ for @currentSystemCursor@
currentSystemCursorSelector :: Selector '[] (Id NSCursor)
currentSystemCursorSelector = mkSelector "currentSystemCursor"

-- | @Selector@ for @resizeLeftCursor@
resizeLeftCursorSelector :: Selector '[] (Id NSCursor)
resizeLeftCursorSelector = mkSelector "resizeLeftCursor"

-- | @Selector@ for @resizeRightCursor@
resizeRightCursorSelector :: Selector '[] (Id NSCursor)
resizeRightCursorSelector = mkSelector "resizeRightCursor"

-- | @Selector@ for @resizeLeftRightCursor@
resizeLeftRightCursorSelector :: Selector '[] (Id NSCursor)
resizeLeftRightCursorSelector = mkSelector "resizeLeftRightCursor"

-- | @Selector@ for @resizeUpCursor@
resizeUpCursorSelector :: Selector '[] (Id NSCursor)
resizeUpCursorSelector = mkSelector "resizeUpCursor"

-- | @Selector@ for @resizeDownCursor@
resizeDownCursorSelector :: Selector '[] (Id NSCursor)
resizeDownCursorSelector = mkSelector "resizeDownCursor"

-- | @Selector@ for @resizeUpDownCursor@
resizeUpDownCursorSelector :: Selector '[] (Id NSCursor)
resizeUpDownCursorSelector = mkSelector "resizeUpDownCursor"

