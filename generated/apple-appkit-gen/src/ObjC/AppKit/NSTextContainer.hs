{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextContainer@.
module ObjC.AppKit.NSTextContainer
  ( NSTextContainer
  , IsNSTextContainer(..)
  , initWithCoder
  , initWithContainerSize
  , lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect
  , containsPoint
  , replaceLayoutManager
  , textLayoutManager
  , lineBreakMode
  , setLineBreakMode
  , lineFragmentPadding
  , setLineFragmentPadding
  , maximumNumberOfLines
  , setMaximumNumberOfLines
  , simpleRectangularTextContainer
  , widthTracksTextView
  , setWidthTracksTextView
  , heightTracksTextView
  , setHeightTracksTextView
  , containerSize
  , setContainerSize
  , layoutManager
  , setLayoutManager
  , exclusionPaths
  , setExclusionPaths
  , textView
  , setTextView
  , initWithCoderSelector
  , initWithContainerSizeSelector
  , lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector
  , containsPointSelector
  , replaceLayoutManagerSelector
  , textLayoutManagerSelector
  , lineBreakModeSelector
  , setLineBreakModeSelector
  , lineFragmentPaddingSelector
  , setLineFragmentPaddingSelector
  , maximumNumberOfLinesSelector
  , setMaximumNumberOfLinesSelector
  , simpleRectangularTextContainerSelector
  , widthTracksTextViewSelector
  , setWidthTracksTextViewSelector
  , heightTracksTextViewSelector
  , setHeightTracksTextViewSelector
  , containerSizeSelector
  , setContainerSizeSelector
  , layoutManagerSelector
  , setLayoutManagerSelector
  , exclusionPathsSelector
  , setExclusionPathsSelector
  , textViewSelector
  , setTextViewSelector

  -- * Enum types
  , NSLineBreakMode(NSLineBreakMode)
  , pattern NSLineBreakByWordWrapping
  , pattern NSLineBreakByCharWrapping
  , pattern NSLineBreakByClipping
  , pattern NSLineBreakByTruncatingHead
  , pattern NSLineBreakByTruncatingTail
  , pattern NSLineBreakByTruncatingMiddle
  , NSLineMovementDirection(NSLineMovementDirection)
  , pattern NSLineDoesntMove
  , pattern NSLineMovesLeft
  , pattern NSLineMovesRight
  , pattern NSLineMovesDown
  , pattern NSLineMovesUp
  , NSLineSweepDirection(NSLineSweepDirection)
  , pattern NSLineSweepLeft
  , pattern NSLineSweepRight
  , pattern NSLineSweepDown
  , pattern NSLineSweepUp
  , NSWritingDirection(NSWritingDirection)
  , pattern NSWritingDirectionNatural
  , pattern NSWritingDirectionLeftToRight
  , pattern NSWritingDirectionRightToLeft

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

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextContainer nsTextContainer, IsNSCoder coder) => nsTextContainer -> coder -> IO (Id NSTextContainer)
initWithCoder nsTextContainer  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsTextContainer (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerSize:@
initWithContainerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSSize -> IO (Id NSTextContainer)
initWithContainerSize nsTextContainer  aContainerSize =
    sendMsg nsTextContainer (mkSelector "initWithContainerSize:") (retPtr retVoid) [argNSSize aContainerSize] >>= ownedObject . castPtr

-- | @- lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:@
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSRect -> NSLineSweepDirection -> NSLineMovementDirection -> Ptr NSRect -> IO NSRect
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect nsTextContainer  proposedRect sweepDirection movementDirection remainingRect =
    sendMsgStret nsTextContainer (mkSelector "lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:") retNSRect [argNSRect proposedRect, argCULong (coerce sweepDirection), argCULong (coerce movementDirection), argPtr remainingRect]

-- | @- containsPoint:@
containsPoint :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSPoint -> IO Bool
containsPoint nsTextContainer  point =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContainer (mkSelector "containsPoint:") retCULong [argNSPoint point]

-- | @- replaceLayoutManager:@
replaceLayoutManager :: (IsNSTextContainer nsTextContainer, IsNSLayoutManager newLayoutManager) => nsTextContainer -> newLayoutManager -> IO ()
replaceLayoutManager nsTextContainer  newLayoutManager =
  withObjCPtr newLayoutManager $ \raw_newLayoutManager ->
      sendMsg nsTextContainer (mkSelector "replaceLayoutManager:") retVoid [argPtr (castPtr raw_newLayoutManager :: Ptr ())]

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "textLayoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lineBreakMode@
lineBreakMode :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO NSLineBreakMode
lineBreakMode nsTextContainer  =
    fmap (coerce :: CULong -> NSLineBreakMode) $ sendMsg nsTextContainer (mkSelector "lineBreakMode") retCULong []

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSLineBreakMode -> IO ()
setLineBreakMode nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setLineBreakMode:") retVoid [argCULong (coerce value)]

-- | *********************** Layout constraint properties ************************
--
-- ObjC selector: @- lineFragmentPadding@
lineFragmentPadding :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO CDouble
lineFragmentPadding nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "lineFragmentPadding") retCDouble []

-- | *********************** Layout constraint properties ************************
--
-- ObjC selector: @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSTextContainer nsTextContainer => nsTextContainer -> CDouble -> IO ()
setLineFragmentPadding nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setLineFragmentPadding:") retVoid [argCDouble value]

-- | @- maximumNumberOfLines@
maximumNumberOfLines :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO CULong
maximumNumberOfLines nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "maximumNumberOfLines") retCULong []

-- | @- setMaximumNumberOfLines:@
setMaximumNumberOfLines :: IsNSTextContainer nsTextContainer => nsTextContainer -> CULong -> IO ()
setMaximumNumberOfLines nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setMaximumNumberOfLines:") retVoid [argCULong value]

-- | @- simpleRectangularTextContainer@
simpleRectangularTextContainer :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
simpleRectangularTextContainer nsTextContainer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContainer (mkSelector "simpleRectangularTextContainer") retCULong []

-- | ************************** View synchronization ***************************
--
-- ObjC selector: @- widthTracksTextView@
widthTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
widthTracksTextView nsTextContainer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContainer (mkSelector "widthTracksTextView") retCULong []

-- | ************************** View synchronization ***************************
--
-- ObjC selector: @- setWidthTracksTextView:@
setWidthTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> Bool -> IO ()
setWidthTracksTextView nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setWidthTracksTextView:") retVoid [argCULong (if value then 1 else 0)]

-- | @- heightTracksTextView@
heightTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
heightTracksTextView nsTextContainer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContainer (mkSelector "heightTracksTextView") retCULong []

-- | @- setHeightTracksTextView:@
setHeightTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> Bool -> IO ()
setHeightTracksTextView nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setHeightTracksTextView:") retVoid [argCULong (if value then 1 else 0)]

-- | @- containerSize@
containerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO NSSize
containerSize nsTextContainer  =
    sendMsgStret nsTextContainer (mkSelector "containerSize") retNSSize []

-- | @- setContainerSize:@
setContainerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSSize -> IO ()
setContainerSize nsTextContainer  value =
    sendMsg nsTextContainer (mkSelector "setContainerSize:") retVoid [argNSSize value]

-- | @- layoutManager@
layoutManager :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSLayoutManager)
layoutManager nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "layoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayoutManager:@
setLayoutManager :: (IsNSTextContainer nsTextContainer, IsNSLayoutManager value) => nsTextContainer -> value -> IO ()
setLayoutManager nsTextContainer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextContainer (mkSelector "setLayoutManager:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exclusionPaths@
exclusionPaths :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSArray)
exclusionPaths nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "exclusionPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExclusionPaths:@
setExclusionPaths :: (IsNSTextContainer nsTextContainer, IsNSArray value) => nsTextContainer -> value -> IO ()
setExclusionPaths nsTextContainer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextContainer (mkSelector "setExclusionPaths:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textView@
textView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSTextView)
textView nsTextContainer  =
    sendMsg nsTextContainer (mkSelector "textView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextView:@
setTextView :: (IsNSTextContainer nsTextContainer, IsNSTextView value) => nsTextContainer -> value -> IO ()
setTextView nsTextContainer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextContainer (mkSelector "setTextView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerSize:@
initWithContainerSizeSelector :: Selector
initWithContainerSizeSelector = mkSelector "initWithContainerSize:"

-- | @Selector@ for @lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:@
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector :: Selector
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector = mkSelector "lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @replaceLayoutManager:@
replaceLayoutManagerSelector :: Selector
replaceLayoutManagerSelector = mkSelector "replaceLayoutManager:"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @lineFragmentPadding@
lineFragmentPaddingSelector :: Selector
lineFragmentPaddingSelector = mkSelector "lineFragmentPadding"

-- | @Selector@ for @setLineFragmentPadding:@
setLineFragmentPaddingSelector :: Selector
setLineFragmentPaddingSelector = mkSelector "setLineFragmentPadding:"

-- | @Selector@ for @maximumNumberOfLines@
maximumNumberOfLinesSelector :: Selector
maximumNumberOfLinesSelector = mkSelector "maximumNumberOfLines"

-- | @Selector@ for @setMaximumNumberOfLines:@
setMaximumNumberOfLinesSelector :: Selector
setMaximumNumberOfLinesSelector = mkSelector "setMaximumNumberOfLines:"

-- | @Selector@ for @simpleRectangularTextContainer@
simpleRectangularTextContainerSelector :: Selector
simpleRectangularTextContainerSelector = mkSelector "simpleRectangularTextContainer"

-- | @Selector@ for @widthTracksTextView@
widthTracksTextViewSelector :: Selector
widthTracksTextViewSelector = mkSelector "widthTracksTextView"

-- | @Selector@ for @setWidthTracksTextView:@
setWidthTracksTextViewSelector :: Selector
setWidthTracksTextViewSelector = mkSelector "setWidthTracksTextView:"

-- | @Selector@ for @heightTracksTextView@
heightTracksTextViewSelector :: Selector
heightTracksTextViewSelector = mkSelector "heightTracksTextView"

-- | @Selector@ for @setHeightTracksTextView:@
setHeightTracksTextViewSelector :: Selector
setHeightTracksTextViewSelector = mkSelector "setHeightTracksTextView:"

-- | @Selector@ for @containerSize@
containerSizeSelector :: Selector
containerSizeSelector = mkSelector "containerSize"

-- | @Selector@ for @setContainerSize:@
setContainerSizeSelector :: Selector
setContainerSizeSelector = mkSelector "setContainerSize:"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @setLayoutManager:@
setLayoutManagerSelector :: Selector
setLayoutManagerSelector = mkSelector "setLayoutManager:"

-- | @Selector@ for @exclusionPaths@
exclusionPathsSelector :: Selector
exclusionPathsSelector = mkSelector "exclusionPaths"

-- | @Selector@ for @setExclusionPaths:@
setExclusionPathsSelector :: Selector
setExclusionPathsSelector = mkSelector "setExclusionPaths:"

-- | @Selector@ for @textView@
textViewSelector :: Selector
textViewSelector = mkSelector "textView"

-- | @Selector@ for @setTextView:@
setTextViewSelector :: Selector
setTextViewSelector = mkSelector "setTextView:"

