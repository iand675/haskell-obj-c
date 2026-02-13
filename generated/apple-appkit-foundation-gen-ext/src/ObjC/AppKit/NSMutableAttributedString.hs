{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , superscriptRange
  , subscriptRange
  , unscriptRange
  , applyFontTraits_range
  , setAlignment_range
  , setBaseWritingDirection_range
  , fixFontAttributeInRange
  , fixParagraphStyleAttributeInRange
  , fixAttachmentAttributeInRange
  , readFromURL_options_documentAttributes_error
  , readFromData_options_documentAttributes_error
  , fixAttributesInRange
  , applyFontTraits_rangeSelector
  , fixAttachmentAttributeInRangeSelector
  , fixAttributesInRangeSelector
  , fixFontAttributeInRangeSelector
  , fixParagraphStyleAttributeInRangeSelector
  , readFromData_options_documentAttributesSelector
  , readFromData_options_documentAttributes_errorSelector
  , readFromURL_options_documentAttributesSelector
  , readFromURL_options_documentAttributes_errorSelector
  , setAlignment_rangeSelector
  , setBaseWritingDirection_rangeSelector
  , subscriptRangeSelector
  , superscriptRangeSelector
  , unscriptRangeSelector
  , updateAttachmentsFromPathSelector

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

-- | @- updateAttachmentsFromPath:@
updateAttachmentsFromPath :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString path) => nsMutableAttributedString -> path -> IO ()
updateAttachmentsFromPath nsMutableAttributedString path =
  sendMessage nsMutableAttributedString updateAttachmentsFromPathSelector (toNSString path)

-- | @- readFromURL:options:documentAttributes:@
readFromURL_options_documentAttributes :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSURL url, IsNSDictionary options, IsNSDictionary dict) => nsMutableAttributedString -> url -> options -> dict -> IO Bool
readFromURL_options_documentAttributes nsMutableAttributedString url options dict =
  sendMessage nsMutableAttributedString readFromURL_options_documentAttributesSelector (toNSURL url) (toNSDictionary options) (toNSDictionary dict)

-- | @- readFromData:options:documentAttributes:@
readFromData_options_documentAttributes :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSData data_, IsNSDictionary options, IsNSDictionary dict) => nsMutableAttributedString -> data_ -> options -> dict -> IO Bool
readFromData_options_documentAttributes nsMutableAttributedString data_ options dict =
  sendMessage nsMutableAttributedString readFromData_options_documentAttributesSelector (toNSData data_) (toNSDictionary options) (toNSDictionary dict)

-- | @- superscriptRange:@
superscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
superscriptRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString superscriptRangeSelector range

-- | @- subscriptRange:@
subscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
subscriptRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString subscriptRangeSelector range

-- | @- unscriptRange:@
unscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
unscriptRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString unscriptRangeSelector range

-- | @- applyFontTraits:range:@
applyFontTraits_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSFontTraitMask -> NSRange -> IO ()
applyFontTraits_range nsMutableAttributedString traitMask range =
  sendMessage nsMutableAttributedString applyFontTraits_rangeSelector traitMask range

-- | @- setAlignment:range:@
setAlignment_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSTextAlignment -> NSRange -> IO ()
setAlignment_range nsMutableAttributedString alignment range =
  sendMessage nsMutableAttributedString setAlignment_rangeSelector alignment range

-- | @- setBaseWritingDirection:range:@
setBaseWritingDirection_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSWritingDirection -> NSRange -> IO ()
setBaseWritingDirection_range nsMutableAttributedString writingDirection range =
  sendMessage nsMutableAttributedString setBaseWritingDirection_rangeSelector writingDirection range

-- | @- fixFontAttributeInRange:@
fixFontAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixFontAttributeInRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString fixFontAttributeInRangeSelector range

-- | @- fixParagraphStyleAttributeInRange:@
fixParagraphStyleAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixParagraphStyleAttributeInRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString fixParagraphStyleAttributeInRangeSelector range

-- | @- fixAttachmentAttributeInRange:@
fixAttachmentAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixAttachmentAttributeInRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString fixAttachmentAttributeInRangeSelector range

-- | @- readFromURL:options:documentAttributes:error:@
readFromURL_options_documentAttributes_error :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSURL url, IsNSDictionary opts, IsNSDictionary dict, IsNSError error_) => nsMutableAttributedString -> url -> opts -> dict -> error_ -> IO Bool
readFromURL_options_documentAttributes_error nsMutableAttributedString url opts dict error_ =
  sendMessage nsMutableAttributedString readFromURL_options_documentAttributes_errorSelector (toNSURL url) (toNSDictionary opts) (toNSDictionary dict) (toNSError error_)

-- | @- readFromData:options:documentAttributes:error:@
readFromData_options_documentAttributes_error :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSData data_, IsNSDictionary opts, IsNSDictionary dict, IsNSError error_) => nsMutableAttributedString -> data_ -> opts -> dict -> error_ -> IO Bool
readFromData_options_documentAttributes_error nsMutableAttributedString data_ opts dict error_ =
  sendMessage nsMutableAttributedString readFromData_options_documentAttributes_errorSelector (toNSData data_) (toNSDictionary opts) (toNSDictionary dict) (toNSError error_)

-- | @- fixAttributesInRange:@
fixAttributesInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixAttributesInRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString fixAttributesInRangeSelector range

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateAttachmentsFromPath:@
updateAttachmentsFromPathSelector :: Selector '[Id NSString] ()
updateAttachmentsFromPathSelector = mkSelector "updateAttachmentsFromPath:"

-- | @Selector@ for @readFromURL:options:documentAttributes:@
readFromURL_options_documentAttributesSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSDictionary] Bool
readFromURL_options_documentAttributesSelector = mkSelector "readFromURL:options:documentAttributes:"

-- | @Selector@ for @readFromData:options:documentAttributes:@
readFromData_options_documentAttributesSelector :: Selector '[Id NSData, Id NSDictionary, Id NSDictionary] Bool
readFromData_options_documentAttributesSelector = mkSelector "readFromData:options:documentAttributes:"

-- | @Selector@ for @superscriptRange:@
superscriptRangeSelector :: Selector '[NSRange] ()
superscriptRangeSelector = mkSelector "superscriptRange:"

-- | @Selector@ for @subscriptRange:@
subscriptRangeSelector :: Selector '[NSRange] ()
subscriptRangeSelector = mkSelector "subscriptRange:"

-- | @Selector@ for @unscriptRange:@
unscriptRangeSelector :: Selector '[NSRange] ()
unscriptRangeSelector = mkSelector "unscriptRange:"

-- | @Selector@ for @applyFontTraits:range:@
applyFontTraits_rangeSelector :: Selector '[NSFontTraitMask, NSRange] ()
applyFontTraits_rangeSelector = mkSelector "applyFontTraits:range:"

-- | @Selector@ for @setAlignment:range:@
setAlignment_rangeSelector :: Selector '[NSTextAlignment, NSRange] ()
setAlignment_rangeSelector = mkSelector "setAlignment:range:"

-- | @Selector@ for @setBaseWritingDirection:range:@
setBaseWritingDirection_rangeSelector :: Selector '[NSWritingDirection, NSRange] ()
setBaseWritingDirection_rangeSelector = mkSelector "setBaseWritingDirection:range:"

-- | @Selector@ for @fixFontAttributeInRange:@
fixFontAttributeInRangeSelector :: Selector '[NSRange] ()
fixFontAttributeInRangeSelector = mkSelector "fixFontAttributeInRange:"

-- | @Selector@ for @fixParagraphStyleAttributeInRange:@
fixParagraphStyleAttributeInRangeSelector :: Selector '[NSRange] ()
fixParagraphStyleAttributeInRangeSelector = mkSelector "fixParagraphStyleAttributeInRange:"

-- | @Selector@ for @fixAttachmentAttributeInRange:@
fixAttachmentAttributeInRangeSelector :: Selector '[NSRange] ()
fixAttachmentAttributeInRangeSelector = mkSelector "fixAttachmentAttributeInRange:"

-- | @Selector@ for @readFromURL:options:documentAttributes:error:@
readFromURL_options_documentAttributes_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSDictionary, Id NSError] Bool
readFromURL_options_documentAttributes_errorSelector = mkSelector "readFromURL:options:documentAttributes:error:"

-- | @Selector@ for @readFromData:options:documentAttributes:error:@
readFromData_options_documentAttributes_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSDictionary, Id NSError] Bool
readFromData_options_documentAttributes_errorSelector = mkSelector "readFromData:options:documentAttributes:error:"

-- | @Selector@ for @fixAttributesInRange:@
fixAttributesInRangeSelector :: Selector '[NSRange] ()
fixAttributesInRangeSelector = mkSelector "fixAttributesInRange:"

