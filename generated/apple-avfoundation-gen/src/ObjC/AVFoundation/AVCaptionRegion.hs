{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionRegion
--
-- An instance of AVCaptionRegion represents a region where a caption is placed.
--
-- Currently, there is just four predefined region instances. The interface doesn't support configuration of region settings.
--
-- Generated bindings for @AVCaptionRegion@.
module ObjC.AVFoundation.AVCaptionRegion
  ( AVCaptionRegion
  , IsAVCaptionRegion(..)
  , encodeWithCoder
  , isEqual
  , mutableCopyWithZone
  , appleITTTopRegion
  , appleITTBottomRegion
  , appleITTLeftRegion
  , appleITTRightRegion
  , subRipTextBottomRegion
  , identifier
  , origin
  , size
  , scroll
  , displayAlignment
  , writingMode
  , appleITTBottomRegionSelector
  , appleITTLeftRegionSelector
  , appleITTRightRegionSelector
  , appleITTTopRegionSelector
  , displayAlignmentSelector
  , encodeWithCoderSelector
  , identifierSelector
  , isEqualSelector
  , mutableCopyWithZoneSelector
  , originSelector
  , scrollSelector
  , sizeSelector
  , subRipTextBottomRegionSelector
  , writingModeSelector

  -- * Enum types
  , AVCaptionRegionDisplayAlignment(AVCaptionRegionDisplayAlignment)
  , pattern AVCaptionRegionDisplayAlignmentBefore
  , pattern AVCaptionRegionDisplayAlignmentCenter
  , pattern AVCaptionRegionDisplayAlignmentAfter
  , AVCaptionRegionScroll(AVCaptionRegionScroll)
  , pattern AVCaptionRegionScrollNone
  , pattern AVCaptionRegionScrollRollUp
  , AVCaptionRegionWritingMode(AVCaptionRegionWritingMode)
  , pattern AVCaptionRegionWritingModeLeftToRightAndTopToBottom
  , pattern AVCaptionRegionWritingModeTopToBottomAndRightToLeft

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Structs
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | encodeWithCoder:
--
-- NSCoding protocol method override
--
-- This method throws an exception if the caption region's size has different units for width and height, or if the units are unrecognizeable.
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsAVCaptionRegion avCaptionRegion, IsNSCoder encoder) => avCaptionRegion -> encoder -> IO ()
encodeWithCoder avCaptionRegion encoder =
  sendMessage avCaptionRegion encodeWithCoderSelector (toNSCoder encoder)

-- | isEqual:
--
-- NSObject protocol method override
--
-- This method throws an exception if the caption region's size has different units for width and height, or if the units are unrecognizeable.
--
-- ObjC selector: @- isEqual:@
isEqual :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> RawId -> IO Bool
isEqual avCaptionRegion object =
  sendMessage avCaptionRegion isEqualSelector object

-- | mutableCopyWithZone:
--
-- NSMutableCopying protocol method override
--
-- This method throws an exception if the caption region contains an identifier.
--
-- ObjC selector: @- mutableCopyWithZone:@
mutableCopyWithZone :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> Ptr () -> IO RawId
mutableCopyWithZone avCaptionRegion zone =
  sendOwnedMessage avCaptionRegion mutableCopyWithZoneSelector zone

-- | appleITTTopRegion
--
-- The top region for iTT format
--
-- This region can be used in iTT format and it occupies the top 15% of the display area. The region uses LRTB, a line progresses left to right and the block extends from top to bottom. Each line is stacked with top justified.
--
-- ObjC selector: @+ appleITTTopRegion@
appleITTTopRegion :: IO (Id AVCaptionRegion)
appleITTTopRegion  =
  do
    cls' <- getRequiredClass "AVCaptionRegion"
    sendClassMessage cls' appleITTTopRegionSelector

-- | appleITTBottomRegion
--
-- The bottom region for iTT format
--
-- This region can be used in iTT format and it occupies the bottom 15% of the display area. The region uses LRTB, a line progresses left to right and the block extends from top to bottom. Each line is stacked with bottom justified.
--
-- ObjC selector: @+ appleITTBottomRegion@
appleITTBottomRegion :: IO (Id AVCaptionRegion)
appleITTBottomRegion  =
  do
    cls' <- getRequiredClass "AVCaptionRegion"
    sendClassMessage cls' appleITTBottomRegionSelector

-- | appleITTLeftRegion
--
-- The  left region for iTT format
--
-- This region can be used in iTT format and it occupies the left 15% of the display area. The region uses TBRL, a line progresses top to bottom and the block extends from right to left. Each line is stacked with right justified.
--
-- ObjC selector: @+ appleITTLeftRegion@
appleITTLeftRegion :: IO (Id AVCaptionRegion)
appleITTLeftRegion  =
  do
    cls' <- getRequiredClass "AVCaptionRegion"
    sendClassMessage cls' appleITTLeftRegionSelector

-- | appleITTRightRegion
--
-- The right region for iTT format
--
-- This region can be used in iTT format and it occupies the right 15% of the display area. The region uses TBRL, a line progresses top to bottom and the block extends from right to left. Each line is stacked with right justified.
--
-- ObjC selector: @+ appleITTRightRegion@
appleITTRightRegion :: IO (Id AVCaptionRegion)
appleITTRightRegion  =
  do
    cls' <- getRequiredClass "AVCaptionRegion"
    sendClassMessage cls' appleITTRightRegionSelector

-- | subRipTextBottomRegion
--
-- The bottom region for SubRip Text (SRT) format
--
-- This region can be used in SRT format and it occupies the entire video display area. The region uses LRTB, a line progresses left to right and the block extends from top to bottom. Each line is stacked with bottom justified.
--
-- ObjC selector: @+ subRipTextBottomRegion@
subRipTextBottomRegion :: IO (Id AVCaptionRegion)
subRipTextBottomRegion  =
  do
    cls' <- getRequiredClass "AVCaptionRegion"
    sendClassMessage cls' subRipTextBottomRegionSelector

-- | identifier
--
-- Identifier for the region
--
-- When regionIdentifier is nil, two regions with the same position and endPosition are considered to be same, that is    captions referring these regions belong to the same region when serialized to a format like TTML.  In addition, the	AVCaptionRegion cannot be mutably copied.
--
-- When regionIdentifier is not nil, two regions are same if and only if the region identifier is equal. It is a    client's responsibility to ensure these AVCaptionRegion objects have the same properties.
--
-- ObjC selector: @- identifier@
identifier :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO (Id NSString)
identifier avCaptionRegion =
  sendMessage avCaptionRegion identifierSelector

-- | origin
--
-- The position of the top-left of the region, potentially with unspecified fields.
--
-- It returns an AVCaptionPoint potentially with unspecified x and/or y fields. Unspecified dimensions indicate the region doesn't have positioning information for that dimension.
--
-- ObjC selector: @- origin@
origin :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO AVCaptionPoint
origin avCaptionRegion =
  sendMessage avCaptionRegion originSelector

-- | size
--
-- The width and height of the region, potentally with unspecified fields.
--
-- It returns an AVCaptionSize potentially with unspecified width and/or height.	CEA608 closed captions support limits the size.height property’s value to 1 cell except when the AVCaptionRegionScroll is AVCaptionRegionScrollRollUp. If the AVCaptionRegionScroll is AVCaptionRegionScrollRollUp, the size.height property’s value must be 2, 3 or 4 cells.	It returns an AVCaptionSize with unspecifed width and height when the region doesn't have width or height information.
--
-- ObjC selector: @- size@
size :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO AVCaptionSize
size avCaptionRegion =
  sendMessage avCaptionRegion sizeSelector

-- | scroll
--
-- Scroll mode for the region
--
-- See AVCaptionRegionScrollXXX enum for possible values.
--
-- ObjC selector: @- scroll@
scroll :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO AVCaptionRegionScroll
scroll avCaptionRegion =
  sendMessage avCaptionRegion scrollSelector

-- | displayAlignment
--
-- Alignment of lines for the region
--
-- ObjC selector: @- displayAlignment@
displayAlignment :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO AVCaptionRegionDisplayAlignment
displayAlignment avCaptionRegion =
  sendMessage avCaptionRegion displayAlignmentSelector

-- | writingMode
--
-- The block and inline progression direction of the region.
--
-- ObjC selector: @- writingMode@
writingMode :: IsAVCaptionRegion avCaptionRegion => avCaptionRegion -> IO AVCaptionRegionWritingMode
writingMode avCaptionRegion =
  sendMessage avCaptionRegion writingModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @isEqual:@
isEqualSelector :: Selector '[RawId] Bool
isEqualSelector = mkSelector "isEqual:"

-- | @Selector@ for @mutableCopyWithZone:@
mutableCopyWithZoneSelector :: Selector '[Ptr ()] RawId
mutableCopyWithZoneSelector = mkSelector "mutableCopyWithZone:"

-- | @Selector@ for @appleITTTopRegion@
appleITTTopRegionSelector :: Selector '[] (Id AVCaptionRegion)
appleITTTopRegionSelector = mkSelector "appleITTTopRegion"

-- | @Selector@ for @appleITTBottomRegion@
appleITTBottomRegionSelector :: Selector '[] (Id AVCaptionRegion)
appleITTBottomRegionSelector = mkSelector "appleITTBottomRegion"

-- | @Selector@ for @appleITTLeftRegion@
appleITTLeftRegionSelector :: Selector '[] (Id AVCaptionRegion)
appleITTLeftRegionSelector = mkSelector "appleITTLeftRegion"

-- | @Selector@ for @appleITTRightRegion@
appleITTRightRegionSelector :: Selector '[] (Id AVCaptionRegion)
appleITTRightRegionSelector = mkSelector "appleITTRightRegion"

-- | @Selector@ for @subRipTextBottomRegion@
subRipTextBottomRegionSelector :: Selector '[] (Id AVCaptionRegion)
subRipTextBottomRegionSelector = mkSelector "subRipTextBottomRegion"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @origin@
originSelector :: Selector '[] AVCaptionPoint
originSelector = mkSelector "origin"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] AVCaptionSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @scroll@
scrollSelector :: Selector '[] AVCaptionRegionScroll
scrollSelector = mkSelector "scroll"

-- | @Selector@ for @displayAlignment@
displayAlignmentSelector :: Selector '[] AVCaptionRegionDisplayAlignment
displayAlignmentSelector = mkSelector "displayAlignment"

-- | @Selector@ for @writingMode@
writingModeSelector :: Selector '[] AVCaptionRegionWritingMode
writingModeSelector = mkSelector "writingMode"

