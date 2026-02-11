{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedString@.
module ObjC.AppKit.NSAttributedString
  ( NSAttributedString
  , IsNSAttributedString(..)
  , attributedStringWithAttachment
  , textFileTypes
  , textPasteboardTypes
  , textUnfilteredFileTypes
  , textUnfilteredPasteboardTypes
  , initWithURL_documentAttributes
  , initWithPath_documentAttributes
  , urlAtIndex_effectiveRange
  , nextWordFromIndex_forward
  , itemNumberInTextList_atIndex
  , containsAttachments
  , attributedStringWithAttachmentSelector
  , textFileTypesSelector
  , textPasteboardTypesSelector
  , textUnfilteredFileTypesSelector
  , textUnfilteredPasteboardTypesSelector
  , initWithURL_documentAttributesSelector
  , initWithPath_documentAttributesSelector
  , urlAtIndex_effectiveRangeSelector
  , nextWordFromIndex_forwardSelector
  , itemNumberInTextList_atIndexSelector
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
import ObjC.AppKit.Internal.Enums

-- | @+ attributedStringWithAttachment:@
attributedStringWithAttachment :: IsNSTextAttachment attachment => attachment -> IO RawId
attributedStringWithAttachment attachment =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr attachment $ \raw_attachment ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "attributedStringWithAttachment:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ())]

-- | @+ textFileTypes@
textFileTypes :: IO RawId
textFileTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "textFileTypes") (retPtr retVoid) []

-- | @+ textPasteboardTypes@
textPasteboardTypes :: IO RawId
textPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "textPasteboardTypes") (retPtr retVoid) []

-- | @+ textUnfilteredFileTypes@
textUnfilteredFileTypes :: IO RawId
textUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "textUnfilteredFileTypes") (retPtr retVoid) []

-- | @+ textUnfilteredPasteboardTypes@
textUnfilteredPasteboardTypes :: IO RawId
textUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSAttributedString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "textUnfilteredPasteboardTypes") (retPtr retVoid) []

-- | @- initWithURL:documentAttributes:@
initWithURL_documentAttributes :: IsNSAttributedString nsAttributedString => nsAttributedString -> RawId -> RawId -> IO (Id NSAttributedString)
initWithURL_documentAttributes nsAttributedString  url dict =
    sendMsg nsAttributedString (mkSelector "initWithURL:documentAttributes:") (retPtr retVoid) [argPtr (castPtr (unRawId url) :: Ptr ()), argPtr (castPtr (unRawId dict) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPath:documentAttributes:@
initWithPath_documentAttributes :: IsNSAttributedString nsAttributedString => nsAttributedString -> RawId -> RawId -> IO (Id NSAttributedString)
initWithPath_documentAttributes nsAttributedString  path dict =
    sendMsg nsAttributedString (mkSelector "initWithPath:documentAttributes:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ()), argPtr (castPtr (unRawId dict) :: Ptr ())] >>= ownedObject . castPtr

-- | @- URLAtIndex:effectiveRange:@
urlAtIndex_effectiveRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> RawId -> IO RawId
urlAtIndex_effectiveRange nsAttributedString  location effectiveRange =
    fmap (RawId . castPtr) $ sendMsg nsAttributedString (mkSelector "URLAtIndex:effectiveRange:") (retPtr retVoid) [argCULong location, argPtr (castPtr (unRawId effectiveRange) :: Ptr ())]

-- | @- nextWordFromIndex:forward:@
nextWordFromIndex_forward :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Bool -> IO CULong
nextWordFromIndex_forward nsAttributedString  location isForward =
    sendMsg nsAttributedString (mkSelector "nextWordFromIndex:forward:") retCULong [argCULong location, argCULong (if isForward then 1 else 0)]

-- | @- itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndex :: (IsNSAttributedString nsAttributedString, IsNSTextList list) => nsAttributedString -> list -> CULong -> IO CLong
itemNumberInTextList_atIndex nsAttributedString  list location =
  withObjCPtr list $ \raw_list ->
      sendMsg nsAttributedString (mkSelector "itemNumberInTextList:atIndex:") retCLong [argPtr (castPtr raw_list :: Ptr ()), argCULong location]

-- | @- containsAttachments@
containsAttachments :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO Bool
containsAttachments nsAttributedString  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "containsAttachments") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedStringWithAttachment:@
attributedStringWithAttachmentSelector :: Selector
attributedStringWithAttachmentSelector = mkSelector "attributedStringWithAttachment:"

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

-- | @Selector@ for @nextWordFromIndex:forward:@
nextWordFromIndex_forwardSelector :: Selector
nextWordFromIndex_forwardSelector = mkSelector "nextWordFromIndex:forward:"

-- | @Selector@ for @itemNumberInTextList:atIndex:@
itemNumberInTextList_atIndexSelector :: Selector
itemNumberInTextList_atIndexSelector = mkSelector "itemNumberInTextList:atIndex:"

-- | @Selector@ for @containsAttachments@
containsAttachmentsSelector :: Selector
containsAttachmentsSelector = mkSelector "containsAttachments"

