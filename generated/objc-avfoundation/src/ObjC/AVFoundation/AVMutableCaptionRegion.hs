{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithIdentifierSelector
  , originSelector
  , setOriginSelector
  , sizeSelector
  , setSizeSelector
  , scrollSelector
  , setScrollSelector
  , displayAlignmentSelector
  , setDisplayAlignmentSelector
  , writingModeSelector
  , setWritingModeSelector

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
init_ avMutableCaptionRegion  =
  sendMsg avMutableCaptionRegion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithIdentifier:
--
-- Create a region object with the identifier.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsAVMutableCaptionRegion avMutableCaptionRegion, IsNSString identifier) => avMutableCaptionRegion -> identifier -> IO (Id AVMutableCaptionRegion)
initWithIdentifier avMutableCaptionRegion  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg avMutableCaptionRegion (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | origin
--
-- The position of the top-left of the region.
--
-- ObjC selector: @- origin@
origin :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionPoint
origin avMutableCaptionRegion  =
  sendMsgStret avMutableCaptionRegion (mkSelector "origin") retAVCaptionPoint []

-- | origin
--
-- The position of the top-left of the region.
--
-- ObjC selector: @- setOrigin:@
setOrigin :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionPoint -> IO ()
setOrigin avMutableCaptionRegion  value =
  sendMsg avMutableCaptionRegion (mkSelector "setOrigin:") retVoid [argAVCaptionPoint value]

-- | size
--
-- The width and height of the region, potentally with unspecified fields.
--
-- ObjC selector: @- size@
size :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionSize
size avMutableCaptionRegion  =
  sendMsgStret avMutableCaptionRegion (mkSelector "size") retAVCaptionSize []

-- | size
--
-- The width and height of the region, potentally with unspecified fields.
--
-- ObjC selector: @- setSize:@
setSize :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionSize -> IO ()
setSize avMutableCaptionRegion  value =
  sendMsg avMutableCaptionRegion (mkSelector "setSize:") retVoid [argAVCaptionSize value]

-- | scroll
--
-- Region scroll mode.
--
-- ObjC selector: @- scroll@
scroll :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionScroll
scroll avMutableCaptionRegion  =
  fmap (coerce :: CLong -> AVCaptionRegionScroll) $ sendMsg avMutableCaptionRegion (mkSelector "scroll") retCLong []

-- | scroll
--
-- Region scroll mode.
--
-- ObjC selector: @- setScroll:@
setScroll :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionScroll -> IO ()
setScroll avMutableCaptionRegion  value =
  sendMsg avMutableCaptionRegion (mkSelector "setScroll:") retVoid [argCLong (coerce value)]

-- | displayAlignment
--
-- Alignment of lines in the region.
--
-- ObjC selector: @- displayAlignment@
displayAlignment :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionDisplayAlignment
displayAlignment avMutableCaptionRegion  =
  fmap (coerce :: CLong -> AVCaptionRegionDisplayAlignment) $ sendMsg avMutableCaptionRegion (mkSelector "displayAlignment") retCLong []

-- | displayAlignment
--
-- Alignment of lines in the region.
--
-- ObjC selector: @- setDisplayAlignment:@
setDisplayAlignment :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionDisplayAlignment -> IO ()
setDisplayAlignment avMutableCaptionRegion  value =
  sendMsg avMutableCaptionRegion (mkSelector "setDisplayAlignment:") retVoid [argCLong (coerce value)]

-- | writingMode
--
-- The block and inline progression direction of the region.
--
-- ObjC selector: @- writingMode@
writingMode :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> IO AVCaptionRegionWritingMode
writingMode avMutableCaptionRegion  =
  fmap (coerce :: CLong -> AVCaptionRegionWritingMode) $ sendMsg avMutableCaptionRegion (mkSelector "writingMode") retCLong []

-- | writingMode
--
-- The block and inline progression direction of the region.
--
-- ObjC selector: @- setWritingMode:@
setWritingMode :: IsAVMutableCaptionRegion avMutableCaptionRegion => avMutableCaptionRegion -> AVCaptionRegionWritingMode -> IO ()
setWritingMode avMutableCaptionRegion  value =
  sendMsg avMutableCaptionRegion (mkSelector "setWritingMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @origin@
originSelector :: Selector
originSelector = mkSelector "origin"

-- | @Selector@ for @setOrigin:@
setOriginSelector :: Selector
setOriginSelector = mkSelector "setOrigin:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @scroll@
scrollSelector :: Selector
scrollSelector = mkSelector "scroll"

-- | @Selector@ for @setScroll:@
setScrollSelector :: Selector
setScrollSelector = mkSelector "setScroll:"

-- | @Selector@ for @displayAlignment@
displayAlignmentSelector :: Selector
displayAlignmentSelector = mkSelector "displayAlignment"

-- | @Selector@ for @setDisplayAlignment:@
setDisplayAlignmentSelector :: Selector
setDisplayAlignmentSelector = mkSelector "setDisplayAlignment:"

-- | @Selector@ for @writingMode@
writingModeSelector :: Selector
writingModeSelector = mkSelector "writingMode"

-- | @Selector@ for @setWritingMode:@
setWritingModeSelector :: Selector
setWritingModeSelector = mkSelector "setWritingMode:"

