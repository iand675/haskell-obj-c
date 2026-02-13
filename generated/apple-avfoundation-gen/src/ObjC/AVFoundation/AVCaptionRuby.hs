{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alignmentSelector
  , initSelector
  , initWithTextSelector
  , initWithText_position_alignmentSelector
  , newSelector
  , positionSelector
  , textSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO (Id AVCaptionRuby)
init_ avCaptionRuby =
  sendOwnedMessage avCaptionRuby initSelector

-- | @+ new@
new :: IO (Id AVCaptionRuby)
new  =
  do
    cls' <- getRequiredClass "AVCaptionRuby"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithText:@
initWithText :: (IsAVCaptionRuby avCaptionRuby, IsNSString text) => avCaptionRuby -> text -> IO (Id AVCaptionRuby)
initWithText avCaptionRuby text =
  sendOwnedMessage avCaptionRuby initWithTextSelector (toNSString text)

-- | @- initWithText:position:alignment:@
initWithText_position_alignment :: (IsAVCaptionRuby avCaptionRuby, IsNSString text) => avCaptionRuby -> text -> AVCaptionRubyPosition -> AVCaptionRubyAlignment -> IO (Id AVCaptionRuby)
initWithText_position_alignment avCaptionRuby text position alignment =
  sendOwnedMessage avCaptionRuby initWithText_position_alignmentSelector (toNSString text) position alignment

-- | text
--
-- The ruby text
--
-- ObjC selector: @- text@
text :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO (Id NSString)
text avCaptionRuby =
  sendMessage avCaptionRuby textSelector

-- | position
--
-- The position of ruby text with respect to the ruby base.
--
-- ObjC selector: @- position@
position :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO AVCaptionRubyPosition
position avCaptionRuby =
  sendMessage avCaptionRuby positionSelector

-- | alignment
--
-- The alignment of ruby text.
--
-- ObjC selector: @- alignment@
alignment :: IsAVCaptionRuby avCaptionRuby => avCaptionRuby -> IO AVCaptionRubyAlignment
alignment avCaptionRuby =
  sendMessage avCaptionRuby alignmentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptionRuby)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptionRuby)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithText:@
initWithTextSelector :: Selector '[Id NSString] (Id AVCaptionRuby)
initWithTextSelector = mkSelector "initWithText:"

-- | @Selector@ for @initWithText:position:alignment:@
initWithText_position_alignmentSelector :: Selector '[Id NSString, AVCaptionRubyPosition, AVCaptionRubyAlignment] (Id AVCaptionRuby)
initWithText_position_alignmentSelector = mkSelector "initWithText:position:alignment:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @position@
positionSelector :: Selector '[] AVCaptionRubyPosition
positionSelector = mkSelector "position"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] AVCaptionRubyAlignment
alignmentSelector = mkSelector "alignment"

