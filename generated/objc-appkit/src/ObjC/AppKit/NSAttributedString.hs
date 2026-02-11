{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedString@.
module ObjC.AppKit.NSAttributedString
  ( NSAttributedString
  , IsNSAttributedString(..)
  , attributedStringWithAdaptiveImageGlyph_attributes
  , drawWithRect_options
  , boundingRectWithSize_options
  , attributedStringWithAttachment
  , attributedStringWithAttachment_attributes
  , textFileTypes
  , textPasteboardTypes
  , textUnfilteredFileTypes
  , textUnfilteredPasteboardTypes
  , initWithURL_documentAttributes
  , initWithPath_documentAttributes
  , urlAtIndex_effectiveRange
  , fontAttributesInRange
  , rulerAttributesInRange
  , lineBreakBeforeIndex_withinRange
  , lineBreakByHyphenatingBeforeIndex_withinRange
  , doubleClickAtIndex
  , nextWordFromIndex_forward
  , rangeOfTextBlock_atIndex
  , rangeOfTextTable_atIndex
  , rangeOfTextList_atIndex
  , itemNumberInTextList_atIndex
  , initWithRTF_documentAttributes
  , initWithRTFD_documentAttributes
  , initWithHTML_documentAttributes
  , initWithHTML_baseURL_documentAttributes
  , initWithDocFormat_documentAttributes
  , initWithHTML_options_documentAttributes
  , initWithRTFDFileWrapper_documentAttributes
  , rtfFromRange_documentAttributes
  , rtfdFromRange_documentAttributes
  , rtfdFileWrapperFromRange_documentAttributes
  , docFormatFromRange_documentAttributes
  , containsAttachmentsInRange
  , prefersRTFDInRange
  , initWithURL_options_documentAttributes_error
  , initWithData_options_documentAttributes_error
  , dataFromRange_documentAttributes_error
  , fileWrapperFromRange_documentAttributes_error
  , containsAttachments
  , attributedStringWithAdaptiveImageGlyph_attributesSelector
  , drawWithRect_optionsSelector
  , boundingRectWithSize_optionsSelector
  , attributedStringWithAttachmentSelector
  , attributedStringWithAttachment_attributesSelector
  , textFileTypesSelector
  , textPasteboardTypesSelector
  , textUnfilteredFileTypesSelector
  , textUnfilteredPasteboardTypesSelector
  , initWithURL_documentAttributesSelector
  , initWithPath_documentAttributesSelector
  , urlAtIndex_effectiveRangeSelector
  , fontAttributesInRangeSelector
  , rulerAttributesInRangeSelector
  , lineBreakBeforeIndex_withinRangeSelector
  , lineBreakByHyphenatingBeforeIndex_withinRangeSelector
  , doubleClickAtIndexSelector
  , nextWordFromIndex_forwardSelector
  , rangeOfTextBlock_atIndexSelector
  , rangeOfTextTable_atIndexSelector
  , rangeOfTextList_atIndexSelector
  , itemNumberInTextList_atIndexSelector
  , initWithRTF_documentAttributesSelector
  , initWithRTFD_documentAttributesSelector
  , initWithHTML_documentAttributesSelector
  , initWithHTML_baseURL_documentAttributesSelector
  , initWithDocFormat_documentAttributesSelector
  , initWithHTML_options_documentAttributesSelector
  , initWithRTFDFileWrapper_documentAttributesSelector
  , rtfFromRange_documentAttributesSelector
  , rtfdFromRange_documentAttributesSelector
  , rtfdFileWrapperFromRange_documentAttributesSelector
  , docFormatFromRange_documentAttributesSelector
  , containsAttachmentsInRangeSelector
  , prefersRTFDInRangeSelector
  , initWithURL_options_documentAttributes_errorSelector
  , initWithData_options_documentAttributes_errorSelector
  , dataFromRange_documentAttributes_errorSelector
  , fileWrapperFromRange_documentAttributes_errorSelector
  , containsAttachmentsSelector

  -- * Enum types
  , NSStringDrawingOptions(NSStringDrawingOptions)
  , pattern NSStringDrawingUsesLineFragmentOrigin
  , pattern NSStringDrawingUsesFontLeading
  , pattern NSStringDrawingUsesDeviceMetrics
  , pattern NSStringDrawingTruncatesLastVisibleLine
  , pattern NSStringDrawingOptionsResolvesNaturalAlignmentWithBaseWritingDirection
  , pattern NSStringDrawingDisableScreenFontSubstitution
  , pattern NSStringDrawingOneShot

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

-- | @+ attributedStringWithAdaptiveImageGlyph:attributes:@
attributedStringWithAdaptiveImageGlyph_attributes :: (IsNSAdaptiveImageGlyph adaptiveImageGlyph, IsNSDictionary attributes) => adaptiveImageGlyph -> attributes -> IO (Id NSAttributedString)
attributedStringWithAdaptiveImageGlyph_attributes adaptiveImageGlyph attributes =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr adaptiveImageGlyph $ \raw_adaptiveImageGlyph ->
      withObjCPtr attributes $ \raw_attributes ->
        sendClassMsg cls' (mkSelector "attributedStringWithAdaptiveImageGlyph:attributes:") (retPtr retVoid) [argPtr (castPtr raw_adaptiveImageGlyph :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @- drawWithRect:options:@
drawWithRect_options :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRect -> NSStringDrawingOptions -> IO ()
drawWithRect_options nsAttributedString  rect options =
  sendMsg nsAttributedString (mkSelector "drawWithRect:options:") retVoid [argNSRect rect, argCLong (coerce options)]

-- | @- boundingRectWithSize:options:@
boundingRectWithSize_options :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSSize -> NSStringDrawingOptions -> IO NSRect
boundingRectWithSize_options nsAttributedString  size options =
  sendMsgStret nsAttributedString (mkSelector "boundingRectWithSize:options:") retNSRect [argNSSize size, argCLong (coerce options)]

-- | @+ attributedStringWithAttachment:@
attributedStringWithAttachment :: IsNSTextAttachment attachment => attachment -> IO (Id NSAttributedString)
attributedStringWithAttachment attachment =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr attachment $ \raw_attachment ->
      sendClassMsg cls' (mkSelector "attributedStringWithAttachment:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ())] >>= retainedObject . castPtr

-- | @+ attributedStringWithAttachment:attributes:@
attributedStringWithAttachment_attributes :: (IsNSTextAttachment attachment, IsNSDictionary attributes) => attachment -> attributes -> IO (Id NSAttributedString)
attributedStringWithAttachment_attributes attachment attributes =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr attachment $ \raw_attachment ->
      withObjCPtr attributes $ \raw_attributes ->
        sendClassMsg cls' (mkSelector "attributedStringWithAttachment:attributes:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textFileTypes@
textFileTypes :: IO (Id NSArray)
textFileTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMsg cls' (mkSelector "textFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textPasteboardTypes@
textPasteboardTypes :: IO (Id NSArray)
textPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMsg cls' (mkSelector "textPasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textUnfilteredFileTypes@
textUnfilteredFileTypes :: IO (Id NSArray)
textUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMsg cls' (mkSelector "textUnfilteredFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textUnfilteredPasteboardTypes@
textUnfilteredPasteboardTypes :: IO (Id NSArray)
textUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMsg cls' (mkSelector "textUnfilteredPasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithURL:documentAttributes:@
initWithURL_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSURL url, IsNSDictionary dict) => nsAttributedString -> url -> dict -> IO (Id NSAttributedString)
initWithURL_documentAttributes nsAttributedString  url dict =
withObjCPtr url $ \raw_url ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithURL:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPath:documentAttributes:@
initWithPath_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSString path, IsNSDictionary dict) => nsAttributedString -> path -> dict -> IO (Id NSAttributedString)
initWithPath_documentAttributes nsAttributedString  path dict =
withObjCPtr path $ \raw_path ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithPath:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- URLAtIndex:effectiveRange:@
urlAtIndex_effectiveRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Ptr NSRange -> IO (Id NSURL)
urlAtIndex_effectiveRange nsAttributedString  location effectiveRange =
  sendMsg nsAttributedString (mkSelector "URLAtIndex:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral location), argPtr effectiveRange] >>= retainedObject . castPtr

-- | @- fontAttributesInRange:@
fontAttributesInRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO (Id NSDictionary)
fontAttributesInRange nsAttributedString  range =
  sendMsg nsAttributedString (mkSelector "fontAttributesInRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- rulerAttributesInRange:@
rulerAttributesInRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO (Id NSDictionary)
rulerAttributesInRange nsAttributedString  range =
  sendMsg nsAttributedString (mkSelector "rulerAttributesInRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- lineBreakBeforeIndex:withinRange:@
lineBreakBeforeIndex_withinRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> NSRange -> IO CULong
lineBreakBeforeIndex_withinRange nsAttributedString  location aRange =
  sendMsg nsAttributedString (mkSelector "lineBreakBeforeIndex:withinRange:") retCULong [argCULong (fromIntegral location), argNSRange aRange]

-- | @- lineBreakByHyphenatingBeforeIndex:withinRange:@
lineBreakByHyphenatingBeforeIndex_withinRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> NSRange -> IO CULong
lineBreakByHyphenatingBeforeIndex_withinRange nsAttributedString  location aRange =
  sendMsg nsAttributedString (mkSelector "lineBreakByHyphenatingBeforeIndex:withinRange:") retCULong [argCULong (fromIntegral location), argNSRange aRange]

-- | @- doubleClickAtIndex:@
doubleClickAtIndex :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> IO NSRange
doubleClickAtIndex nsAttributedString  location =
  sendMsgStret nsAttributedString (mkSelector "doubleClickAtIndex:") retNSRange [argCULong (fromIntegral location)]

-- | @- nextWordFromIndex:forward:@
nextWordFromIndex_forward :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Bool -> IO CULong
nextWordFromIndex_forward nsAttributedString  location isForward =
  sendMsg nsAttributedString (mkSelector "nextWordFromIndex:forward:") retCULong [argCULong (fromIntegral location), argCULong (if isForward then 1 else 0)]

-- | @- rangeOfTextBlock:atIndex:@
rangeOfTextBlock_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextBlock block) => nsAttributedString -> block -> CULong -> IO NSRange
rangeOfTextBlock_atIndex nsAttributedString  block location =
withObjCPtr block $ \raw_block ->
    sendMsgStret nsAttributedString (mkSelector "rangeOfTextBlock:atIndex:") retNSRange [argPtr (castPtr raw_block :: Ptr ()), argCULong (fromIntegral location)]

-- | @- rangeOfTextTable:atIndex:@
rangeOfTextTable_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextTable table) => nsAttributedString -> table -> CULong -> IO NSRange
rangeOfTextTable_atIndex nsAttributedString  table location =
withObjCPtr table $ \raw_table ->
    sendMsgStret nsAttributedString (mkSelector "rangeOfTextTable:atIndex:") retNSRange [argPtr (castPtr raw_table :: Ptr ()), argCULong (fromIntegral location)]

-- | @- rangeOfTextList:atIndex:@
rangeOfTextList_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextList list) => nsAttributedString -> list -> CULong -> IO NSRange
rangeOfTextList_atIndex nsAttributedString  list location =
withObjCPtr list $ \raw_list ->
    sendMsgStret nsAttributedString (mkSelector "rangeOfTextList:atIndex:") retNSRange [argPtr (castPtr raw_list :: Ptr ()), argCULong (fromIntegral location)]

-- | @- itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextList list) => nsAttributedString -> list -> CULong -> IO CLong
itemNumberInTextList_atIndex nsAttributedString  list location =
withObjCPtr list $ \raw_list ->
    sendMsg nsAttributedString (mkSelector "itemNumberInTextList:atIndex:") retCLong [argPtr (castPtr raw_list :: Ptr ()), argCULong (fromIntegral location)]

-- | @- initWithRTF:documentAttributes:@
initWithRTF_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary dict) => nsAttributedString -> data_ -> dict -> IO (Id NSAttributedString)
initWithRTF_documentAttributes nsAttributedString  data_ dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithRTF:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRTFD:documentAttributes:@
initWithRTFD_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary dict) => nsAttributedString -> data_ -> dict -> IO (Id NSAttributedString)
initWithRTFD_documentAttributes nsAttributedString  data_ dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithRTFD:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHTML:documentAttributes:@
initWithHTML_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary dict) => nsAttributedString -> data_ -> dict -> IO (Id NSAttributedString)
initWithHTML_documentAttributes nsAttributedString  data_ dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithHTML:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHTML:baseURL:documentAttributes:@
initWithHTML_baseURL_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSURL base, IsNSDictionary dict) => nsAttributedString -> data_ -> base -> dict -> IO (Id NSAttributedString)
initWithHTML_baseURL_documentAttributes nsAttributedString  data_ base dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr base $ \raw_base ->
    withObjCPtr dict $ \raw_dict ->
        sendMsg nsAttributedString (mkSelector "initWithHTML:baseURL:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_base :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDocFormat:documentAttributes:@
initWithDocFormat_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary dict) => nsAttributedString -> data_ -> dict -> IO (Id NSAttributedString)
initWithDocFormat_documentAttributes nsAttributedString  data_ dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithDocFormat:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHTML:options:documentAttributes:@
initWithHTML_options_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary options, IsNSDictionary dict) => nsAttributedString -> data_ -> options -> dict -> IO (Id NSAttributedString)
initWithHTML_options_documentAttributes nsAttributedString  data_ options dict =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr dict $ \raw_dict ->
        sendMsg nsAttributedString (mkSelector "initWithHTML:options:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRTFDFileWrapper:documentAttributes:@
initWithRTFDFileWrapper_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSFileWrapper wrapper, IsNSDictionary dict) => nsAttributedString -> wrapper -> dict -> IO (Id NSAttributedString)
initWithRTFDFileWrapper_documentAttributes nsAttributedString  wrapper dict =
withObjCPtr wrapper $ \raw_wrapper ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg nsAttributedString (mkSelector "initWithRTFDFileWrapper:documentAttributes:") (retPtr retVoid) [argPtr (castPtr raw_wrapper :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- RTFFromRange:documentAttributes:@
rtfFromRange_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict) => nsAttributedString -> NSRange -> dict -> IO (Id NSData)
rtfFromRange_documentAttributes nsAttributedString  range dict =
withObjCPtr dict $ \raw_dict ->
    sendMsg nsAttributedString (mkSelector "RTFFromRange:documentAttributes:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- RTFDFromRange:documentAttributes:@
rtfdFromRange_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict) => nsAttributedString -> NSRange -> dict -> IO (Id NSData)
rtfdFromRange_documentAttributes nsAttributedString  range dict =
withObjCPtr dict $ \raw_dict ->
    sendMsg nsAttributedString (mkSelector "RTFDFromRange:documentAttributes:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- RTFDFileWrapperFromRange:documentAttributes:@
rtfdFileWrapperFromRange_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict) => nsAttributedString -> NSRange -> dict -> IO (Id NSFileWrapper)
rtfdFileWrapperFromRange_documentAttributes nsAttributedString  range dict =
withObjCPtr dict $ \raw_dict ->
    sendMsg nsAttributedString (mkSelector "RTFDFileWrapperFromRange:documentAttributes:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- docFormatFromRange:documentAttributes:@
docFormatFromRange_documentAttributes :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict) => nsAttributedString -> NSRange -> dict -> IO (Id NSData)
docFormatFromRange_documentAttributes nsAttributedString  range dict =
withObjCPtr dict $ \raw_dict ->
    sendMsg nsAttributedString (mkSelector "docFormatFromRange:documentAttributes:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- containsAttachmentsInRange:@
containsAttachmentsInRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO Bool
containsAttachmentsInRange nsAttributedString  range =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "containsAttachmentsInRange:") retCULong [argNSRange range]

-- | @- prefersRTFDInRange:@
prefersRTFDInRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO Bool
prefersRTFDInRange nsAttributedString  range =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "prefersRTFDInRange:") retCULong [argNSRange range]

-- | @- initWithURL:options:documentAttributes:error:@
initWithURL_options_documentAttributes_error :: (IsNSAttributedString nsAttributedString, IsNSURL url, IsNSDictionary options, IsNSDictionary dict, IsNSError error_) => nsAttributedString -> url -> options -> dict -> error_ -> IO (Id NSAttributedString)
initWithURL_options_documentAttributes_error nsAttributedString  url options dict error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr dict $ \raw_dict ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsAttributedString (mkSelector "initWithURL:options:documentAttributes:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:options:documentAttributes:error:@
initWithData_options_documentAttributes_error :: (IsNSAttributedString nsAttributedString, IsNSData data_, IsNSDictionary options, IsNSDictionary dict, IsNSError error_) => nsAttributedString -> data_ -> options -> dict -> error_ -> IO (Id NSAttributedString)
initWithData_options_documentAttributes_error nsAttributedString  data_ options dict error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr dict $ \raw_dict ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsAttributedString (mkSelector "initWithData:options:documentAttributes:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- dataFromRange:documentAttributes:error:@
dataFromRange_documentAttributes_error :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict, IsNSError error_) => nsAttributedString -> NSRange -> dict -> error_ -> IO (Id NSData)
dataFromRange_documentAttributes_error nsAttributedString  range dict error_ =
withObjCPtr dict $ \raw_dict ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsAttributedString (mkSelector "dataFromRange:documentAttributes:error:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- fileWrapperFromRange:documentAttributes:error:@
fileWrapperFromRange_documentAttributes_error :: (IsNSAttributedString nsAttributedString, IsNSDictionary dict, IsNSError error_) => nsAttributedString -> NSRange -> dict -> error_ -> IO (Id NSFileWrapper)
fileWrapperFromRange_documentAttributes_error nsAttributedString  range dict error_ =
withObjCPtr dict $ \raw_dict ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsAttributedString (mkSelector "fileWrapperFromRange:documentAttributes:error:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_dict :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- containsAttachments@
containsAttachments :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO Bool
containsAttachments nsAttributedString  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "containsAttachments") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedStringWithAdaptiveImageGlyph:attributes:@
attributedStringWithAdaptiveImageGlyph_attributesSelector :: Selector
attributedStringWithAdaptiveImageGlyph_attributesSelector = mkSelector "attributedStringWithAdaptiveImageGlyph:attributes:"

-- | @Selector@ for @drawWithRect:options:@
drawWithRect_optionsSelector :: Selector
drawWithRect_optionsSelector = mkSelector "drawWithRect:options:"

-- | @Selector@ for @boundingRectWithSize:options:@
boundingRectWithSize_optionsSelector :: Selector
boundingRectWithSize_optionsSelector = mkSelector "boundingRectWithSize:options:"

-- | @Selector@ for @attributedStringWithAttachment:@
attributedStringWithAttachmentSelector :: Selector
attributedStringWithAttachmentSelector = mkSelector "attributedStringWithAttachment:"

-- | @Selector@ for @attributedStringWithAttachment:attributes:@
attributedStringWithAttachment_attributesSelector :: Selector
attributedStringWithAttachment_attributesSelector = mkSelector "attributedStringWithAttachment:attributes:"

-- | @Selector@ for @textFileTypes@
textFileTypesSelector :: Selector
textFileTypesSelector = mkSelector "textFileTypes"

-- | @Selector@ for @textPasteboardTypes@
textPasteboardTypesSelector :: Selector
textPasteboardTypesSelector = mkSelector "textPasteboardTypes"

-- | @Selector@ for @textUnfilteredFileTypes@
textUnfilteredFileTypesSelector :: Selector
textUnfilteredFileTypesSelector = mkSelector "textUnfilteredFileTypes"

-- | @Selector@ for @textUnfilteredPasteboardTypes@
textUnfilteredPasteboardTypesSelector :: Selector
textUnfilteredPasteboardTypesSelector = mkSelector "textUnfilteredPasteboardTypes"

-- | @Selector@ for @initWithURL:documentAttributes:@
initWithURL_documentAttributesSelector :: Selector
initWithURL_documentAttributesSelector = mkSelector "initWithURL:documentAttributes:"

-- | @Selector@ for @initWithPath:documentAttributes:@
initWithPath_documentAttributesSelector :: Selector
initWithPath_documentAttributesSelector = mkSelector "initWithPath:documentAttributes:"

-- | @Selector@ for @URLAtIndex:effectiveRange:@
urlAtIndex_effectiveRangeSelector :: Selector
urlAtIndex_effectiveRangeSelector = mkSelector "URLAtIndex:effectiveRange:"

-- | @Selector@ for @fontAttributesInRange:@
fontAttributesInRangeSelector :: Selector
fontAttributesInRangeSelector = mkSelector "fontAttributesInRange:"

-- | @Selector@ for @rulerAttributesInRange:@
rulerAttributesInRangeSelector :: Selector
rulerAttributesInRangeSelector = mkSelector "rulerAttributesInRange:"

-- | @Selector@ for @lineBreakBeforeIndex:withinRange:@
lineBreakBeforeIndex_withinRangeSelector :: Selector
lineBreakBeforeIndex_withinRangeSelector = mkSelector "lineBreakBeforeIndex:withinRange:"

-- | @Selector@ for @lineBreakByHyphenatingBeforeIndex:withinRange:@
lineBreakByHyphenatingBeforeIndex_withinRangeSelector :: Selector
lineBreakByHyphenatingBeforeIndex_withinRangeSelector = mkSelector "lineBreakByHyphenatingBeforeIndex:withinRange:"

-- | @Selector@ for @doubleClickAtIndex:@
doubleClickAtIndexSelector :: Selector
doubleClickAtIndexSelector = mkSelector "doubleClickAtIndex:"

-- | @Selector@ for @nextWordFromIndex:forward:@
nextWordFromIndex_forwardSelector :: Selector
nextWordFromIndex_forwardSelector = mkSelector "nextWordFromIndex:forward:"

-- | @Selector@ for @rangeOfTextBlock:atIndex:@
rangeOfTextBlock_atIndexSelector :: Selector
rangeOfTextBlock_atIndexSelector = mkSelector "rangeOfTextBlock:atIndex:"

-- | @Selector@ for @rangeOfTextTable:atIndex:@
rangeOfTextTable_atIndexSelector :: Selector
rangeOfTextTable_atIndexSelector = mkSelector "rangeOfTextTable:atIndex:"

-- | @Selector@ for @rangeOfTextList:atIndex:@
rangeOfTextList_atIndexSelector :: Selector
rangeOfTextList_atIndexSelector = mkSelector "rangeOfTextList:atIndex:"

-- | @Selector@ for @itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndexSelector :: Selector
itemNumberInTextList_atIndexSelector = mkSelector "itemNumberInTextList:atIndex:"

-- | @Selector@ for @initWithRTF:documentAttributes:@
initWithRTF_documentAttributesSelector :: Selector
initWithRTF_documentAttributesSelector = mkSelector "initWithRTF:documentAttributes:"

-- | @Selector@ for @initWithRTFD:documentAttributes:@
initWithRTFD_documentAttributesSelector :: Selector
initWithRTFD_documentAttributesSelector = mkSelector "initWithRTFD:documentAttributes:"

-- | @Selector@ for @initWithHTML:documentAttributes:@
initWithHTML_documentAttributesSelector :: Selector
initWithHTML_documentAttributesSelector = mkSelector "initWithHTML:documentAttributes:"

-- | @Selector@ for @initWithHTML:baseURL:documentAttributes:@
initWithHTML_baseURL_documentAttributesSelector :: Selector
initWithHTML_baseURL_documentAttributesSelector = mkSelector "initWithHTML:baseURL:documentAttributes:"

-- | @Selector@ for @initWithDocFormat:documentAttributes:@
initWithDocFormat_documentAttributesSelector :: Selector
initWithDocFormat_documentAttributesSelector = mkSelector "initWithDocFormat:documentAttributes:"

-- | @Selector@ for @initWithHTML:options:documentAttributes:@
initWithHTML_options_documentAttributesSelector :: Selector
initWithHTML_options_documentAttributesSelector = mkSelector "initWithHTML:options:documentAttributes:"

-- | @Selector@ for @initWithRTFDFileWrapper:documentAttributes:@
initWithRTFDFileWrapper_documentAttributesSelector :: Selector
initWithRTFDFileWrapper_documentAttributesSelector = mkSelector "initWithRTFDFileWrapper:documentAttributes:"

-- | @Selector@ for @RTFFromRange:documentAttributes:@
rtfFromRange_documentAttributesSelector :: Selector
rtfFromRange_documentAttributesSelector = mkSelector "RTFFromRange:documentAttributes:"

-- | @Selector@ for @RTFDFromRange:documentAttributes:@
rtfdFromRange_documentAttributesSelector :: Selector
rtfdFromRange_documentAttributesSelector = mkSelector "RTFDFromRange:documentAttributes:"

-- | @Selector@ for @RTFDFileWrapperFromRange:documentAttributes:@
rtfdFileWrapperFromRange_documentAttributesSelector :: Selector
rtfdFileWrapperFromRange_documentAttributesSelector = mkSelector "RTFDFileWrapperFromRange:documentAttributes:"

-- | @Selector@ for @docFormatFromRange:documentAttributes:@
docFormatFromRange_documentAttributesSelector :: Selector
docFormatFromRange_documentAttributesSelector = mkSelector "docFormatFromRange:documentAttributes:"

-- | @Selector@ for @containsAttachmentsInRange:@
containsAttachmentsInRangeSelector :: Selector
containsAttachmentsInRangeSelector = mkSelector "containsAttachmentsInRange:"

-- | @Selector@ for @prefersRTFDInRange:@
prefersRTFDInRangeSelector :: Selector
prefersRTFDInRangeSelector = mkSelector "prefersRTFDInRange:"

-- | @Selector@ for @initWithURL:options:documentAttributes:error:@
initWithURL_options_documentAttributes_errorSelector :: Selector
initWithURL_options_documentAttributes_errorSelector = mkSelector "initWithURL:options:documentAttributes:error:"

-- | @Selector@ for @initWithData:options:documentAttributes:error:@
initWithData_options_documentAttributes_errorSelector :: Selector
initWithData_options_documentAttributes_errorSelector = mkSelector "initWithData:options:documentAttributes:error:"

-- | @Selector@ for @dataFromRange:documentAttributes:error:@
dataFromRange_documentAttributes_errorSelector :: Selector
dataFromRange_documentAttributes_errorSelector = mkSelector "dataFromRange:documentAttributes:error:"

-- | @Selector@ for @fileWrapperFromRange:documentAttributes:error:@
fileWrapperFromRange_documentAttributes_errorSelector :: Selector
fileWrapperFromRange_documentAttributes_errorSelector = mkSelector "fileWrapperFromRange:documentAttributes:error:"

-- | @Selector@ for @containsAttachments@
containsAttachmentsSelector :: Selector
containsAttachmentsSelector = mkSelector "containsAttachments"

