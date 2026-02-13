{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , containerSizeSelector
  , containsPointSelector
  , exclusionPathsSelector
  , heightTracksTextViewSelector
  , initWithCoderSelector
  , initWithContainerSizeSelector
  , layoutManagerSelector
  , lineBreakModeSelector
  , lineFragmentPaddingSelector
  , lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector
  , maximumNumberOfLinesSelector
  , replaceLayoutManagerSelector
  , setContainerSizeSelector
  , setExclusionPathsSelector
  , setHeightTracksTextViewSelector
  , setLayoutManagerSelector
  , setLineBreakModeSelector
  , setLineFragmentPaddingSelector
  , setMaximumNumberOfLinesSelector
  , setTextViewSelector
  , setWidthTracksTextViewSelector
  , simpleRectangularTextContainerSelector
  , textLayoutManagerSelector
  , textViewSelector
  , widthTracksTextViewSelector

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

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextContainer nsTextContainer, IsNSCoder coder) => nsTextContainer -> coder -> IO (Id NSTextContainer)
initWithCoder nsTextContainer coder =
  sendOwnedMessage nsTextContainer initWithCoderSelector (toNSCoder coder)

-- | @- initWithContainerSize:@
initWithContainerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSSize -> IO (Id NSTextContainer)
initWithContainerSize nsTextContainer aContainerSize =
  sendOwnedMessage nsTextContainer initWithContainerSizeSelector aContainerSize

-- | @- lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:@
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSRect -> NSLineSweepDirection -> NSLineMovementDirection -> Ptr NSRect -> IO NSRect
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect nsTextContainer proposedRect sweepDirection movementDirection remainingRect =
  sendMessage nsTextContainer lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector proposedRect sweepDirection movementDirection remainingRect

-- | @- containsPoint:@
containsPoint :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSPoint -> IO Bool
containsPoint nsTextContainer point =
  sendMessage nsTextContainer containsPointSelector point

-- | @- replaceLayoutManager:@
replaceLayoutManager :: (IsNSTextContainer nsTextContainer, IsNSLayoutManager newLayoutManager) => nsTextContainer -> newLayoutManager -> IO ()
replaceLayoutManager nsTextContainer newLayoutManager =
  sendMessage nsTextContainer replaceLayoutManagerSelector (toNSLayoutManager newLayoutManager)

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextContainer =
  sendMessage nsTextContainer textLayoutManagerSelector

-- | @- lineBreakMode@
lineBreakMode :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO NSLineBreakMode
lineBreakMode nsTextContainer =
  sendMessage nsTextContainer lineBreakModeSelector

-- | @- setLineBreakMode:@
setLineBreakMode :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSLineBreakMode -> IO ()
setLineBreakMode nsTextContainer value =
  sendMessage nsTextContainer setLineBreakModeSelector value

-- | *********************** Layout constraint properties ************************
--
-- ObjC selector: @- lineFragmentPadding@
lineFragmentPadding :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO CDouble
lineFragmentPadding nsTextContainer =
  sendMessage nsTextContainer lineFragmentPaddingSelector

-- | *********************** Layout constraint properties ************************
--
-- ObjC selector: @- setLineFragmentPadding:@
setLineFragmentPadding :: IsNSTextContainer nsTextContainer => nsTextContainer -> CDouble -> IO ()
setLineFragmentPadding nsTextContainer value =
  sendMessage nsTextContainer setLineFragmentPaddingSelector value

-- | @- maximumNumberOfLines@
maximumNumberOfLines :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO CULong
maximumNumberOfLines nsTextContainer =
  sendMessage nsTextContainer maximumNumberOfLinesSelector

-- | @- setMaximumNumberOfLines:@
setMaximumNumberOfLines :: IsNSTextContainer nsTextContainer => nsTextContainer -> CULong -> IO ()
setMaximumNumberOfLines nsTextContainer value =
  sendMessage nsTextContainer setMaximumNumberOfLinesSelector value

-- | @- simpleRectangularTextContainer@
simpleRectangularTextContainer :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
simpleRectangularTextContainer nsTextContainer =
  sendMessage nsTextContainer simpleRectangularTextContainerSelector

-- | ************************** View synchronization ***************************
--
-- ObjC selector: @- widthTracksTextView@
widthTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
widthTracksTextView nsTextContainer =
  sendMessage nsTextContainer widthTracksTextViewSelector

-- | ************************** View synchronization ***************************
--
-- ObjC selector: @- setWidthTracksTextView:@
setWidthTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> Bool -> IO ()
setWidthTracksTextView nsTextContainer value =
  sendMessage nsTextContainer setWidthTracksTextViewSelector value

-- | @- heightTracksTextView@
heightTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO Bool
heightTracksTextView nsTextContainer =
  sendMessage nsTextContainer heightTracksTextViewSelector

-- | @- setHeightTracksTextView:@
setHeightTracksTextView :: IsNSTextContainer nsTextContainer => nsTextContainer -> Bool -> IO ()
setHeightTracksTextView nsTextContainer value =
  sendMessage nsTextContainer setHeightTracksTextViewSelector value

-- | @- containerSize@
containerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO NSSize
containerSize nsTextContainer =
  sendMessage nsTextContainer containerSizeSelector

-- | @- setContainerSize:@
setContainerSize :: IsNSTextContainer nsTextContainer => nsTextContainer -> NSSize -> IO ()
setContainerSize nsTextContainer value =
  sendMessage nsTextContainer setContainerSizeSelector value

-- | @- layoutManager@
layoutManager :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSLayoutManager)
layoutManager nsTextContainer =
  sendMessage nsTextContainer layoutManagerSelector

-- | @- setLayoutManager:@
setLayoutManager :: (IsNSTextContainer nsTextContainer, IsNSLayoutManager value) => nsTextContainer -> value -> IO ()
setLayoutManager nsTextContainer value =
  sendMessage nsTextContainer setLayoutManagerSelector (toNSLayoutManager value)

-- | @- exclusionPaths@
exclusionPaths :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSArray)
exclusionPaths nsTextContainer =
  sendMessage nsTextContainer exclusionPathsSelector

-- | @- setExclusionPaths:@
setExclusionPaths :: (IsNSTextContainer nsTextContainer, IsNSArray value) => nsTextContainer -> value -> IO ()
setExclusionPaths nsTextContainer value =
  sendMessage nsTextContainer setExclusionPathsSelector (toNSArray value)

-- | @- textView@
textView :: IsNSTextContainer nsTextContainer => nsTextContainer -> IO (Id NSTextView)
textView nsTextContainer =
  sendMessage nsTextContainer textViewSelector

-- | @- setTextView:@
setTextView :: (IsNSTextContainer nsTextContainer, IsNSTextView value) => nsTextContainer -> value -> IO ()
setTextView nsTextContainer value =
  sendMessage nsTextContainer setTextViewSelector (toNSTextView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextContainer)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerSize:@
initWithContainerSizeSelector :: Selector '[NSSize] (Id NSTextContainer)
initWithContainerSizeSelector = mkSelector "initWithContainerSize:"

-- | @Selector@ for @lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:@
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector :: Selector '[NSRect, NSLineSweepDirection, NSLineMovementDirection, Ptr NSRect] NSRect
lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRectSelector = mkSelector "lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector '[NSPoint] Bool
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @replaceLayoutManager:@
replaceLayoutManagerSelector :: Selector '[Id NSLayoutManager] ()
replaceLayoutManagerSelector = mkSelector "replaceLayoutManager:"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @lineBreakMode@
lineBreakModeSelector :: Selector '[] NSLineBreakMode
lineBreakModeSelector = mkSelector "lineBreakMode"

-- | @Selector@ for @setLineBreakMode:@
setLineBreakModeSelector :: Selector '[NSLineBreakMode] ()
setLineBreakModeSelector = mkSelector "setLineBreakMode:"

-- | @Selector@ for @lineFragmentPadding@
lineFragmentPaddingSelector :: Selector '[] CDouble
lineFragmentPaddingSelector = mkSelector "lineFragmentPadding"

-- | @Selector@ for @setLineFragmentPadding:@
setLineFragmentPaddingSelector :: Selector '[CDouble] ()
setLineFragmentPaddingSelector = mkSelector "setLineFragmentPadding:"

-- | @Selector@ for @maximumNumberOfLines@
maximumNumberOfLinesSelector :: Selector '[] CULong
maximumNumberOfLinesSelector = mkSelector "maximumNumberOfLines"

-- | @Selector@ for @setMaximumNumberOfLines:@
setMaximumNumberOfLinesSelector :: Selector '[CULong] ()
setMaximumNumberOfLinesSelector = mkSelector "setMaximumNumberOfLines:"

-- | @Selector@ for @simpleRectangularTextContainer@
simpleRectangularTextContainerSelector :: Selector '[] Bool
simpleRectangularTextContainerSelector = mkSelector "simpleRectangularTextContainer"

-- | @Selector@ for @widthTracksTextView@
widthTracksTextViewSelector :: Selector '[] Bool
widthTracksTextViewSelector = mkSelector "widthTracksTextView"

-- | @Selector@ for @setWidthTracksTextView:@
setWidthTracksTextViewSelector :: Selector '[Bool] ()
setWidthTracksTextViewSelector = mkSelector "setWidthTracksTextView:"

-- | @Selector@ for @heightTracksTextView@
heightTracksTextViewSelector :: Selector '[] Bool
heightTracksTextViewSelector = mkSelector "heightTracksTextView"

-- | @Selector@ for @setHeightTracksTextView:@
setHeightTracksTextViewSelector :: Selector '[Bool] ()
setHeightTracksTextViewSelector = mkSelector "setHeightTracksTextView:"

-- | @Selector@ for @containerSize@
containerSizeSelector :: Selector '[] NSSize
containerSizeSelector = mkSelector "containerSize"

-- | @Selector@ for @setContainerSize:@
setContainerSizeSelector :: Selector '[NSSize] ()
setContainerSizeSelector = mkSelector "setContainerSize:"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] (Id NSLayoutManager)
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @setLayoutManager:@
setLayoutManagerSelector :: Selector '[Id NSLayoutManager] ()
setLayoutManagerSelector = mkSelector "setLayoutManager:"

-- | @Selector@ for @exclusionPaths@
exclusionPathsSelector :: Selector '[] (Id NSArray)
exclusionPathsSelector = mkSelector "exclusionPaths"

-- | @Selector@ for @setExclusionPaths:@
setExclusionPathsSelector :: Selector '[Id NSArray] ()
setExclusionPathsSelector = mkSelector "setExclusionPaths:"

-- | @Selector@ for @textView@
textViewSelector :: Selector '[] (Id NSTextView)
textViewSelector = mkSelector "textView"

-- | @Selector@ for @setTextView:@
setTextViewSelector :: Selector '[Id NSTextView] ()
setTextViewSelector = mkSelector "setTextView:"

