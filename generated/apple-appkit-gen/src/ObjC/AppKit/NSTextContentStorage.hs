{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextContentStorage@.
module ObjC.AppKit.NSTextContentStorage
  ( NSTextContentStorage
  , IsNSTextContentStorage(..)
  , attributedStringForTextElement
  , textElementForAttributedString
  , locationFromLocation_withOffset
  , offsetFromLocation_toLocation
  , adjustedRangeFromRange_forEditingTextSelection
  , delegate
  , setDelegate
  , includesTextListMarkers
  , setIncludesTextListMarkers
  , attributedString
  , setAttributedString
  , attributedStringForTextElementSelector
  , textElementForAttributedStringSelector
  , locationFromLocation_withOffsetSelector
  , offsetFromLocation_toLocationSelector
  , adjustedRangeFromRange_forEditingTextSelectionSelector
  , delegateSelector
  , setDelegateSelector
  , includesTextListMarkersSelector
  , setIncludesTextListMarkersSelector
  , attributedStringSelector
  , setAttributedStringSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- attributedStringForTextElement:@
attributedStringForTextElement :: (IsNSTextContentStorage nsTextContentStorage, IsNSTextElement textElement) => nsTextContentStorage -> textElement -> IO (Id NSAttributedString)
attributedStringForTextElement nsTextContentStorage  textElement =
  withObjCPtr textElement $ \raw_textElement ->
      sendMsg nsTextContentStorage (mkSelector "attributedStringForTextElement:") (retPtr retVoid) [argPtr (castPtr raw_textElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- textElementForAttributedString:@
textElementForAttributedString :: (IsNSTextContentStorage nsTextContentStorage, IsNSAttributedString attributedString) => nsTextContentStorage -> attributedString -> IO (Id NSTextElement)
textElementForAttributedString nsTextContentStorage  attributedString =
  withObjCPtr attributedString $ \raw_attributedString ->
      sendMsg nsTextContentStorage (mkSelector "textElementForAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_attributedString :: Ptr ())] >>= retainedObject . castPtr

-- | @- locationFromLocation:withOffset:@
locationFromLocation_withOffset :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> CLong -> IO RawId
locationFromLocation_withOffset nsTextContentStorage  location offset =
    fmap (RawId . castPtr) $ sendMsg nsTextContentStorage (mkSelector "locationFromLocation:withOffset:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argCLong offset]

-- | @- offsetFromLocation:toLocation:@
offsetFromLocation_toLocation :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> RawId -> IO CLong
offsetFromLocation_toLocation nsTextContentStorage  from to =
    sendMsg nsTextContentStorage (mkSelector "offsetFromLocation:toLocation:") retCLong [argPtr (castPtr (unRawId from) :: Ptr ()), argPtr (castPtr (unRawId to) :: Ptr ())]

-- | @- adjustedRangeFromRange:forEditingTextSelection:@
adjustedRangeFromRange_forEditingTextSelection :: (IsNSTextContentStorage nsTextContentStorage, IsNSTextRange textRange) => nsTextContentStorage -> textRange -> Bool -> IO (Id NSTextRange)
adjustedRangeFromRange_forEditingTextSelection nsTextContentStorage  textRange forEditingTextSelection =
  withObjCPtr textRange $ \raw_textRange ->
      sendMsg nsTextContentStorage (mkSelector "adjustedRangeFromRange:forEditingTextSelection:") (retPtr retVoid) [argPtr (castPtr raw_textRange :: Ptr ()), argCULong (if forEditingTextSelection then 1 else 0)] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO RawId
delegate nsTextContentStorage  =
    fmap (RawId . castPtr) $ sendMsg nsTextContentStorage (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> IO ()
setDelegate nsTextContentStorage  value =
    sendMsg nsTextContentStorage (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- includesTextListMarkers@
includesTextListMarkers :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO Bool
includesTextListMarkers nsTextContentStorage  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContentStorage (mkSelector "includesTextListMarkers") retCULong []

-- | @- setIncludesTextListMarkers:@
setIncludesTextListMarkers :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> Bool -> IO ()
setIncludesTextListMarkers nsTextContentStorage  value =
    sendMsg nsTextContentStorage (mkSelector "setIncludesTextListMarkers:") retVoid [argCULong (if value then 1 else 0)]

-- | @- attributedString@
attributedString :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO (Id NSAttributedString)
attributedString nsTextContentStorage  =
    sendMsg nsTextContentStorage (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedString:@
setAttributedString :: (IsNSTextContentStorage nsTextContentStorage, IsNSAttributedString value) => nsTextContentStorage -> value -> IO ()
setAttributedString nsTextContentStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextContentStorage (mkSelector "setAttributedString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedStringForTextElement:@
attributedStringForTextElementSelector :: Selector
attributedStringForTextElementSelector = mkSelector "attributedStringForTextElement:"

-- | @Selector@ for @textElementForAttributedString:@
textElementForAttributedStringSelector :: Selector
textElementForAttributedStringSelector = mkSelector "textElementForAttributedString:"

-- | @Selector@ for @locationFromLocation:withOffset:@
locationFromLocation_withOffsetSelector :: Selector
locationFromLocation_withOffsetSelector = mkSelector "locationFromLocation:withOffset:"

-- | @Selector@ for @offsetFromLocation:toLocation:@
offsetFromLocation_toLocationSelector :: Selector
offsetFromLocation_toLocationSelector = mkSelector "offsetFromLocation:toLocation:"

-- | @Selector@ for @adjustedRangeFromRange:forEditingTextSelection:@
adjustedRangeFromRange_forEditingTextSelectionSelector :: Selector
adjustedRangeFromRange_forEditingTextSelectionSelector = mkSelector "adjustedRangeFromRange:forEditingTextSelection:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @includesTextListMarkers@
includesTextListMarkersSelector :: Selector
includesTextListMarkersSelector = mkSelector "includesTextListMarkers"

-- | @Selector@ for @setIncludesTextListMarkers:@
setIncludesTextListMarkersSelector :: Selector
setIncludesTextListMarkersSelector = mkSelector "setIncludesTextListMarkers:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector
setAttributedStringSelector = mkSelector "setAttributedString:"

