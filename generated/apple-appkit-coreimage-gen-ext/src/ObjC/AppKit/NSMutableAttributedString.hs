{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableAttributedString@.
module ObjC.AppKit.NSMutableAttributedString
  ( NSMutableAttributedString
  , IsNSMutableAttributedString(..)
  , updateAttachmentsFromPath
  , readFromURL_options_documentAttributes
  , readFromData_options_documentAttributes
  , updateAttachmentsFromPathSelector
  , readFromURL_options_documentAttributesSelector
  , readFromData_options_documentAttributesSelector

  -- * Enum types
  , NSFontTraitMask(NSFontTraitMask)
  , pattern NSItalicFontMask
  , pattern NSBoldFontMask
  , pattern NSUnboldFontMask
  , pattern NSNonStandardCharacterSetFontMask
  , pattern NSNarrowFontMask
  , pattern NSExpandedFontMask
  , pattern NSCondensedFontMask
  , pattern NSSmallCapsFontMask
  , pattern NSPosterFontMask
  , pattern NSCompressedFontMask
  , pattern NSFixedPitchFontMask
  , pattern NSUnitalicFontMask
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums

-- | @- updateAttachmentsFromPath:@
updateAttachmentsFromPath :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> RawId -> IO ()
updateAttachmentsFromPath nsMutableAttributedString  path =
    sendMsg nsMutableAttributedString (mkSelector "updateAttachmentsFromPath:") retVoid [argPtr (castPtr (unRawId path) :: Ptr ())]

-- | @- readFromURL:options:documentAttributes:@
readFromURL_options_documentAttributes :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> RawId -> RawId -> RawId -> IO Bool
readFromURL_options_documentAttributes nsMutableAttributedString  url options dict =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromURL:options:documentAttributes:") retCULong [argPtr (castPtr (unRawId url) :: Ptr ()), argPtr (castPtr (unRawId options) :: Ptr ()), argPtr (castPtr (unRawId dict) :: Ptr ())]

-- | @- readFromData:options:documentAttributes:@
readFromData_options_documentAttributes :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> RawId -> RawId -> RawId -> IO Bool
readFromData_options_documentAttributes nsMutableAttributedString  data_ options dict =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromData:options:documentAttributes:") retCULong [argPtr (castPtr (unRawId data_) :: Ptr ()), argPtr (castPtr (unRawId options) :: Ptr ()), argPtr (castPtr (unRawId dict) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateAttachmentsFromPath:@
updateAttachmentsFromPathSelector :: Selector
updateAttachmentsFromPathSelector = mkSelector "updateAttachmentsFromPath:"

-- | @Selector@ for @readFromURL:options:documentAttributes:@
readFromURL_options_documentAttributesSelector :: Selector
readFromURL_options_documentAttributesSelector = mkSelector "readFromURL:options:documentAttributes:"

-- | @Selector@ for @readFromData:options:documentAttributes:@
readFromData_options_documentAttributesSelector :: Selector
readFromData_options_documentAttributesSelector = mkSelector "readFromData:options:documentAttributes:"

