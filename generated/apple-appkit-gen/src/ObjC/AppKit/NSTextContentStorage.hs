{-# LANGUAGE DataKinds #-}
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
  , adjustedRangeFromRange_forEditingTextSelectionSelector
  , attributedStringForTextElementSelector
  , attributedStringSelector
  , delegateSelector
  , includesTextListMarkersSelector
  , locationFromLocation_withOffsetSelector
  , offsetFromLocation_toLocationSelector
  , setAttributedStringSelector
  , setDelegateSelector
  , setIncludesTextListMarkersSelector
  , textElementForAttributedStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- attributedStringForTextElement:@
attributedStringForTextElement :: (IsNSTextContentStorage nsTextContentStorage, IsNSTextElement textElement) => nsTextContentStorage -> textElement -> IO (Id NSAttributedString)
attributedStringForTextElement nsTextContentStorage textElement =
  sendMessage nsTextContentStorage attributedStringForTextElementSelector (toNSTextElement textElement)

-- | @- textElementForAttributedString:@
textElementForAttributedString :: (IsNSTextContentStorage nsTextContentStorage, IsNSAttributedString attributedString) => nsTextContentStorage -> attributedString -> IO (Id NSTextElement)
textElementForAttributedString nsTextContentStorage attributedString =
  sendMessage nsTextContentStorage textElementForAttributedStringSelector (toNSAttributedString attributedString)

-- | @- locationFromLocation:withOffset:@
locationFromLocation_withOffset :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> CLong -> IO RawId
locationFromLocation_withOffset nsTextContentStorage location offset =
  sendMessage nsTextContentStorage locationFromLocation_withOffsetSelector location offset

-- | @- offsetFromLocation:toLocation:@
offsetFromLocation_toLocation :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> RawId -> IO CLong
offsetFromLocation_toLocation nsTextContentStorage from to =
  sendMessage nsTextContentStorage offsetFromLocation_toLocationSelector from to

-- | @- adjustedRangeFromRange:forEditingTextSelection:@
adjustedRangeFromRange_forEditingTextSelection :: (IsNSTextContentStorage nsTextContentStorage, IsNSTextRange textRange) => nsTextContentStorage -> textRange -> Bool -> IO (Id NSTextRange)
adjustedRangeFromRange_forEditingTextSelection nsTextContentStorage textRange forEditingTextSelection =
  sendMessage nsTextContentStorage adjustedRangeFromRange_forEditingTextSelectionSelector (toNSTextRange textRange) forEditingTextSelection

-- | @- delegate@
delegate :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO RawId
delegate nsTextContentStorage =
  sendMessage nsTextContentStorage delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> RawId -> IO ()
setDelegate nsTextContentStorage value =
  sendMessage nsTextContentStorage setDelegateSelector value

-- | @- includesTextListMarkers@
includesTextListMarkers :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO Bool
includesTextListMarkers nsTextContentStorage =
  sendMessage nsTextContentStorage includesTextListMarkersSelector

-- | @- setIncludesTextListMarkers:@
setIncludesTextListMarkers :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> Bool -> IO ()
setIncludesTextListMarkers nsTextContentStorage value =
  sendMessage nsTextContentStorage setIncludesTextListMarkersSelector value

-- | @- attributedString@
attributedString :: IsNSTextContentStorage nsTextContentStorage => nsTextContentStorage -> IO (Id NSAttributedString)
attributedString nsTextContentStorage =
  sendMessage nsTextContentStorage attributedStringSelector

-- | @- setAttributedString:@
setAttributedString :: (IsNSTextContentStorage nsTextContentStorage, IsNSAttributedString value) => nsTextContentStorage -> value -> IO ()
setAttributedString nsTextContentStorage value =
  sendMessage nsTextContentStorage setAttributedStringSelector (toNSAttributedString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributedStringForTextElement:@
attributedStringForTextElementSelector :: Selector '[Id NSTextElement] (Id NSAttributedString)
attributedStringForTextElementSelector = mkSelector "attributedStringForTextElement:"

-- | @Selector@ for @textElementForAttributedString:@
textElementForAttributedStringSelector :: Selector '[Id NSAttributedString] (Id NSTextElement)
textElementForAttributedStringSelector = mkSelector "textElementForAttributedString:"

-- | @Selector@ for @locationFromLocation:withOffset:@
locationFromLocation_withOffsetSelector :: Selector '[RawId, CLong] RawId
locationFromLocation_withOffsetSelector = mkSelector "locationFromLocation:withOffset:"

-- | @Selector@ for @offsetFromLocation:toLocation:@
offsetFromLocation_toLocationSelector :: Selector '[RawId, RawId] CLong
offsetFromLocation_toLocationSelector = mkSelector "offsetFromLocation:toLocation:"

-- | @Selector@ for @adjustedRangeFromRange:forEditingTextSelection:@
adjustedRangeFromRange_forEditingTextSelectionSelector :: Selector '[Id NSTextRange, Bool] (Id NSTextRange)
adjustedRangeFromRange_forEditingTextSelectionSelector = mkSelector "adjustedRangeFromRange:forEditingTextSelection:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @includesTextListMarkers@
includesTextListMarkersSelector :: Selector '[] Bool
includesTextListMarkersSelector = mkSelector "includesTextListMarkers"

-- | @Selector@ for @setIncludesTextListMarkers:@
setIncludesTextListMarkersSelector :: Selector '[Bool] ()
setIncludesTextListMarkersSelector = mkSelector "setIncludesTextListMarkers:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringSelector = mkSelector "setAttributedString:"

