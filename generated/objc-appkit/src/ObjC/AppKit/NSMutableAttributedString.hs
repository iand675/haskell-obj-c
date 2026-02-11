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
  , updateAttachmentsFromPathSelector
  , readFromURL_options_documentAttributesSelector
  , readFromData_options_documentAttributesSelector
  , superscriptRangeSelector
  , subscriptRangeSelector
  , unscriptRangeSelector
  , applyFontTraits_rangeSelector
  , setAlignment_rangeSelector
  , setBaseWritingDirection_rangeSelector
  , fixFontAttributeInRangeSelector
  , fixParagraphStyleAttributeInRangeSelector
  , fixAttachmentAttributeInRangeSelector
  , readFromURL_options_documentAttributes_errorSelector
  , readFromData_options_documentAttributes_errorSelector
  , fixAttributesInRangeSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- updateAttachmentsFromPath:@
updateAttachmentsFromPath :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString path) => nsMutableAttributedString -> path -> IO ()
updateAttachmentsFromPath nsMutableAttributedString  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsMutableAttributedString (mkSelector "updateAttachmentsFromPath:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- readFromURL:options:documentAttributes:@
readFromURL_options_documentAttributes :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSURL url, IsNSDictionary options, IsNSDictionary dict) => nsMutableAttributedString -> url -> options -> dict -> IO Bool
readFromURL_options_documentAttributes nsMutableAttributedString  url options dict =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr dict $ \raw_dict ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromURL:options:documentAttributes:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())]

-- | @- readFromData:options:documentAttributes:@
readFromData_options_documentAttributes :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSData data_, IsNSDictionary options, IsNSDictionary dict) => nsMutableAttributedString -> data_ -> options -> dict -> IO Bool
readFromData_options_documentAttributes nsMutableAttributedString  data_ options dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr dict $ \raw_dict ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromData:options:documentAttributes:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())]

-- | @- superscriptRange:@
superscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
superscriptRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "superscriptRange:") retVoid [argNSRange range]

-- | @- subscriptRange:@
subscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
subscriptRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "subscriptRange:") retVoid [argNSRange range]

-- | @- unscriptRange:@
unscriptRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
unscriptRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "unscriptRange:") retVoid [argNSRange range]

-- | @- applyFontTraits:range:@
applyFontTraits_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSFontTraitMask -> NSRange -> IO ()
applyFontTraits_range nsMutableAttributedString  traitMask range =
  sendMsg nsMutableAttributedString (mkSelector "applyFontTraits:range:") retVoid [argCULong (coerce traitMask), argNSRange range]

-- | @- setAlignment:range:@
setAlignment_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSTextAlignment -> NSRange -> IO ()
setAlignment_range nsMutableAttributedString  alignment range =
  sendMsg nsMutableAttributedString (mkSelector "setAlignment:range:") retVoid [argCLong (coerce alignment), argNSRange range]

-- | @- setBaseWritingDirection:range:@
setBaseWritingDirection_range :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSWritingDirection -> NSRange -> IO ()
setBaseWritingDirection_range nsMutableAttributedString  writingDirection range =
  sendMsg nsMutableAttributedString (mkSelector "setBaseWritingDirection:range:") retVoid [argCLong (coerce writingDirection), argNSRange range]

-- | @- fixFontAttributeInRange:@
fixFontAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixFontAttributeInRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "fixFontAttributeInRange:") retVoid [argNSRange range]

-- | @- fixParagraphStyleAttributeInRange:@
fixParagraphStyleAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixParagraphStyleAttributeInRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "fixParagraphStyleAttributeInRange:") retVoid [argNSRange range]

-- | @- fixAttachmentAttributeInRange:@
fixAttachmentAttributeInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixAttachmentAttributeInRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "fixAttachmentAttributeInRange:") retVoid [argNSRange range]

-- | @- readFromURL:options:documentAttributes:error:@
readFromURL_options_documentAttributes_error :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSURL url, IsNSDictionary opts, IsNSDictionary dict, IsNSError error_) => nsMutableAttributedString -> url -> opts -> dict -> error_ -> IO Bool
readFromURL_options_documentAttributes_error nsMutableAttributedString  url opts dict error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr opts $ \raw_opts ->
    withObjCPtr dict $ \raw_dict ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromURL:options:documentAttributes:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_opts :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- readFromData:options:documentAttributes:error:@
readFromData_options_documentAttributes_error :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSData data_, IsNSDictionary opts, IsNSDictionary dict, IsNSError error_) => nsMutableAttributedString -> data_ -> opts -> dict -> error_ -> IO Bool
readFromData_options_documentAttributes_error nsMutableAttributedString  data_ opts dict error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr opts $ \raw_opts ->
    withObjCPtr dict $ \raw_dict ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableAttributedString (mkSelector "readFromData:options:documentAttributes:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_opts :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- fixAttributesInRange:@
fixAttributesInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
fixAttributesInRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "fixAttributesInRange:") retVoid [argNSRange range]

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

-- | @Selector@ for @superscriptRange:@
superscriptRangeSelector :: Selector
superscriptRangeSelector = mkSelector "superscriptRange:"

-- | @Selector@ for @subscriptRange:@
subscriptRangeSelector :: Selector
subscriptRangeSelector = mkSelector "subscriptRange:"

-- | @Selector@ for @unscriptRange:@
unscriptRangeSelector :: Selector
unscriptRangeSelector = mkSelector "unscriptRange:"

-- | @Selector@ for @applyFontTraits:range:@
applyFontTraits_rangeSelector :: Selector
applyFontTraits_rangeSelector = mkSelector "applyFontTraits:range:"

-- | @Selector@ for @setAlignment:range:@
setAlignment_rangeSelector :: Selector
setAlignment_rangeSelector = mkSelector "setAlignment:range:"

-- | @Selector@ for @setBaseWritingDirection:range:@
setBaseWritingDirection_rangeSelector :: Selector
setBaseWritingDirection_rangeSelector = mkSelector "setBaseWritingDirection:range:"

-- | @Selector@ for @fixFontAttributeInRange:@
fixFontAttributeInRangeSelector :: Selector
fixFontAttributeInRangeSelector = mkSelector "fixFontAttributeInRange:"

-- | @Selector@ for @fixParagraphStyleAttributeInRange:@
fixParagraphStyleAttributeInRangeSelector :: Selector
fixParagraphStyleAttributeInRangeSelector = mkSelector "fixParagraphStyleAttributeInRange:"

-- | @Selector@ for @fixAttachmentAttributeInRange:@
fixAttachmentAttributeInRangeSelector :: Selector
fixAttachmentAttributeInRangeSelector = mkSelector "fixAttachmentAttributeInRange:"

-- | @Selector@ for @readFromURL:options:documentAttributes:error:@
readFromURL_options_documentAttributes_errorSelector :: Selector
readFromURL_options_documentAttributes_errorSelector = mkSelector "readFromURL:options:documentAttributes:error:"

-- | @Selector@ for @readFromData:options:documentAttributes:error:@
readFromData_options_documentAttributes_errorSelector :: Selector
readFromData_options_documentAttributes_errorSelector = mkSelector "readFromData:options:documentAttributes:error:"

-- | @Selector@ for @fixAttributesInRange:@
fixAttributesInRangeSelector :: Selector
fixAttributesInRangeSelector = mkSelector "fixAttributesInRange:"

