{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionRuby
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCaptionRuby@.
module ObjC.AVFoundation.AVCaptionRuby
  ( AVCaptionRuby
  , IsAVCaptionRuby(..)
  , init_
  , new
  , initWithText
  , initWithText_position_alignment
  , text
  , position
  , alignment
  , initSelector
  , newSelector
  , initWithTextSelector
  , initWithText_position_alignmentSelector
  , textSelector
  , positionSelector
  , alignmentSelector

  -- * Enum types
  , AVCaptionRubyAlignment(AVCaptionRubyAlignment)
  , pattern AVCaptionRubyAlignmentStart
  , pattern AVCaptionRubyAlignmentCenter
  , pattern AVCaptionRubyAlignmentDistributeSpaceBetween
  , pattern AVCaptionRubyAlignmentDistributeSpaceAround
  , AVCaptionRubyPosition(AVCaptionRubyPosition)
  , pattern AVCaptionRubyPositionBefore
  , pattern AVCaptionRubyPositionAfter

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO (Id AVCaptionRuby)
init_ avCaptionRuby  =
  sendMsg avCaptionRuby (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionRuby)
new  =
  do
    cls' <- getRequiredClass "AVCaptionRuby"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithText:@
initWithText :: (IsAVCaptionRuby avCaptionRuby, IsNSString text) => avCaptionRuby -> text -> IO (Id AVCaptionRuby)
initWithText avCaptionRuby  text =
withObjCPtr text $ \raw_text ->
    sendMsg avCaptionRuby (mkSelector "initWithText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithText:position:alignment:@
initWithText_position_alignment :: (IsAVCaptionRuby avCaptionRuby, IsNSString text) => avCaptionRuby -> text -> AVCaptionRubyPosition -> AVCaptionRubyAlignment -> IO (Id AVCaptionRuby)
initWithText_position_alignment avCaptionRuby  text position alignment =
withObjCPtr text $ \raw_text ->
    sendMsg avCaptionRuby (mkSelector "initWithText:position:alignment:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ()), argCLong (coerce position), argCLong (coerce alignment)] >>= ownedObject . castPtr

-- | text
--
-- The ruby text
--
-- ObjC selector: @- text@
text :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO (Id NSString)
text avCaptionRuby  =
  sendMsg avCaptionRuby (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | position
--
-- The position of ruby text with respect to the ruby base.
--
-- ObjC selector: @- position@
position :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO AVCaptionRubyPosition
position avCaptionRuby  =
  fmap (coerce :: CLong -> AVCaptionRubyPosition) $ sendMsg avCaptionRuby (mkSelector "position") retCLong []

-- | alignment
--
-- The alignment of ruby text.
--
-- ObjC selector: @- alignment@
alignment :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO AVCaptionRubyAlignment
alignment avCaptionRuby  =
  fmap (coerce :: CLong -> AVCaptionRubyAlignment) $ sendMsg avCaptionRuby (mkSelector "alignment") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithText:@
initWithTextSelector :: Selector
initWithTextSelector = mkSelector "initWithText:"

-- | @Selector@ for @initWithText:position:alignment:@
initWithText_position_alignmentSelector :: Selector
initWithText_position_alignmentSelector = mkSelector "initWithText:position:alignment:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

