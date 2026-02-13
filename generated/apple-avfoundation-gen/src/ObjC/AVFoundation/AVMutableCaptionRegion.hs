{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableCaptionRegion
--
-- Mutable subclass of AVCaptionRegion.
--
-- Generated bindings for @AVMutableCaptionRegion@.
module ObjC.AVFoundation.AVMutableCaptionRegion
  ( AVMutableCaptionRegion
  , IsAVMutableCaptionRegion(..)
  , init_
  , initWithIdentifier
  , origin
  , setOrigin
  , size
  , setSize
  , scroll
  , setScroll
  , displayAlignment
  , setDisplayAlignment
  , writingMode
  , setWritingMode
  , displayAlignmentSelector
  , initSelector
  , initWithIdentifierSelector
  , originSelector
  , scrollSelector
  , setDisplayAlignmentSelector
  , setOriginSelector
  , setScrollSelector
  , setSizeSelector
  , setWritingModeSelector
  , sizeSelector
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

-- | init
--
-- Create a region object without any properties.
--
-- ObjC selector: @- init@
init_ :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO (Id AVMutableCaptionRegion)
init_ avMutableCaptionRegion =
  sendOwnedMessage avMutableCaptionRegion initSelector

-- | initWithIdentifier:
--
-- Create a region object with the identifier.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsAVMutableCaptionRegion avMutableCaptionRegion, IsNSString identifier) => avMutableCaptionRegion -> identifier -> IO (Id AVMutableCaptionRegion)
initWithIdentifier avMutableCaptionRegion identifier =
  sendOwnedMessage avMutableCaptionRegion initWithIdentifierSelector (toNSString identifier)

-- | origin
--
-- The position of the top-left of the region.
--
-- ObjC selector: @- origin@
origin :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionPoint
origin avMutableCaptionRegion =
  sendMessage avMutableCaptionRegion originSelector

-- | origin
--
-- The position of the top-left of the region.
--
-- ObjC selector: @- setOrigin:@
setOrigin :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionPoint -> IO ()
setOrigin avMutableCaptionRegion value =
  sendMessage avMutableCaptionRegion setOriginSelector value

-- | size
--
-- The width and height of the region, potentally with unspecified fields.
--
-- ObjC selector: @- size@
size :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionSize
size avMutableCaptionRegion =
  sendMessage avMutableCaptionRegion sizeSelector

-- | size
--
-- The width and height of the region, potentally with unspecified fields.
--
-- ObjC selector: @- setSize:@
setSize :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionSize -> IO ()
setSize avMutableCaptionRegion value =
  sendMessage avMutableCaptionRegion setSizeSelector value

-- | scroll
--
-- Region scroll mode.
--
-- ObjC selector: @- scroll@
scroll :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionScroll
scroll avMutableCaptionRegion =
  sendMessage avMutableCaptionRegion scrollSelector

-- | scroll
--
-- Region scroll mode.
--
-- ObjC selector: @- setScroll:@
setScroll :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionScroll -> IO ()
setScroll avMutableCaptionRegion value =
  sendMessage avMutableCaptionRegion setScrollSelector value

-- | displayAlignment
--
-- Alignment of lines in the region.
--
-- ObjC selector: @- displayAlignment@
displayAlignment :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionDisplayAlignment
displayAlignment avMutableCaptionRegion =
  sendMessage avMutableCaptionRegion displayAlignmentSelector

-- | displayAlignment
--
-- Alignment of lines in the region.
--
-- ObjC selector: @- setDisplayAlignment:@
setDisplayAlignment :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionDisplayAlignment -> IO ()
setDisplayAlignment avMutableCaptionRegion value =
  sendMessage avMutableCaptionRegion setDisplayAlignmentSelector value

-- | writingMode
--
-- The block and inline progression direction of the region.
--
-- ObjC selector: @- writingMode@
writingMode :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionWritingMode
writingMode avMutableCaptionRegion =
  sendMessage avMutableCaptionRegion writingModeSelector

-- | writingMode
--
-- The block and inline progression direction of the region.
--
-- ObjC selector: @- setWritingMode:@
setWritingMode :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionWritingMode -> IO ()
setWritingMode avMutableCaptionRegion value =
  sendMessage avMutableCaptionRegion setWritingModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMutableCaptionRegion)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id AVMutableCaptionRegion)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @origin@
originSelector :: Selector '[] AVCaptionPoint
originSelector = mkSelector "origin"

-- | @Selector@ for @setOrigin:@
setOriginSelector :: Selector '[AVCaptionPoint] ()
setOriginSelector = mkSelector "setOrigin:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] AVCaptionSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[AVCaptionSize] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @scroll@
scrollSelector :: Selector '[] AVCaptionRegionScroll
scrollSelector = mkSelector "scroll"

-- | @Selector@ for @setScroll:@
setScrollSelector :: Selector '[AVCaptionRegionScroll] ()
setScrollSelector = mkSelector "setScroll:"

-- | @Selector@ for @displayAlignment@
displayAlignmentSelector :: Selector '[] AVCaptionRegionDisplayAlignment
displayAlignmentSelector = mkSelector "displayAlignment"

-- | @Selector@ for @setDisplayAlignment:@
setDisplayAlignmentSelector :: Selector '[AVCaptionRegionDisplayAlignment] ()
setDisplayAlignmentSelector = mkSelector "setDisplayAlignment:"

-- | @Selector@ for @writingMode@
writingModeSelector :: Selector '[] AVCaptionRegionWritingMode
writingModeSelector = mkSelector "writingMode"

-- | @Selector@ for @setWritingMode:@
setWritingModeSelector :: Selector '[AVCaptionRegionWritingMode] ()
setWritingModeSelector = mkSelector "setWritingMode:"

