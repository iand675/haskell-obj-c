{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableAttributedString@.
module ObjC.Foundation.NSMutableAttributedString
  ( NSMutableAttributedString
  , IsNSMutableAttributedString(..)
  , replaceCharactersInRange_withString
  , setAttributes_range
  , appendLocalizedFormat
  , addAttribute_value_range
  , addAttributes_range
  , removeAttribute_range
  , replaceCharactersInRange_withAttributedString
  , insertAttributedString_atIndex
  , appendAttributedString
  , deleteCharactersInRange
  , setAttributedString
  , beginEditing
  , endEditing
  , mutableString
  , addAttribute_value_rangeSelector
  , addAttributes_rangeSelector
  , appendAttributedStringSelector
  , appendLocalizedFormatSelector
  , beginEditingSelector
  , deleteCharactersInRangeSelector
  , endEditingSelector
  , insertAttributedString_atIndexSelector
  , mutableStringSelector
  , removeAttribute_rangeSelector
  , replaceCharactersInRange_withAttributedStringSelector
  , replaceCharactersInRange_withStringSelector
  , setAttributedStringSelector
  , setAttributes_rangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString str) => nsMutableAttributedString -> NSRange -> str -> IO ()
replaceCharactersInRange_withString nsMutableAttributedString range str =
  sendMessage nsMutableAttributedString replaceCharactersInRange_withStringSelector range (toNSString str)

-- | @- setAttributes:range:@
setAttributes_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSDictionary attrs) => nsMutableAttributedString -> attrs -> NSRange -> IO ()
setAttributes_range nsMutableAttributedString attrs range =
  sendMessage nsMutableAttributedString setAttributes_rangeSelector (toNSDictionary attrs) range

-- | Formats the specified string and arguments with the current locale, then appends the result to the receiver.
--
-- ObjC selector: @- appendLocalizedFormat:@
appendLocalizedFormat :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString format) => nsMutableAttributedString -> format -> IO ()
appendLocalizedFormat nsMutableAttributedString format =
  sendMessage nsMutableAttributedString appendLocalizedFormatSelector (toNSAttributedString format)

-- | @- addAttribute:value:range:@
addAttribute_value_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString name) => nsMutableAttributedString -> name -> RawId -> NSRange -> IO ()
addAttribute_value_range nsMutableAttributedString name value range =
  sendMessage nsMutableAttributedString addAttribute_value_rangeSelector (toNSString name) value range

-- | @- addAttributes:range:@
addAttributes_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSDictionary attrs) => nsMutableAttributedString -> attrs -> NSRange -> IO ()
addAttributes_range nsMutableAttributedString attrs range =
  sendMessage nsMutableAttributedString addAttributes_rangeSelector (toNSDictionary attrs) range

-- | @- removeAttribute:range:@
removeAttribute_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString name) => nsMutableAttributedString -> name -> NSRange -> IO ()
removeAttribute_range nsMutableAttributedString name range =
  sendMessage nsMutableAttributedString removeAttribute_rangeSelector (toNSString name) range

-- | @- replaceCharactersInRange:withAttributedString:@
replaceCharactersInRange_withAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> NSRange -> attrString -> IO ()
replaceCharactersInRange_withAttributedString nsMutableAttributedString range attrString =
  sendMessage nsMutableAttributedString replaceCharactersInRange_withAttributedStringSelector range (toNSAttributedString attrString)

-- | @- insertAttributedString:atIndex:@
insertAttributedString_atIndex :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> CULong -> IO ()
insertAttributedString_atIndex nsMutableAttributedString attrString loc =
  sendMessage nsMutableAttributedString insertAttributedString_atIndexSelector (toNSAttributedString attrString) loc

-- | @- appendAttributedString:@
appendAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> IO ()
appendAttributedString nsMutableAttributedString attrString =
  sendMessage nsMutableAttributedString appendAttributedStringSelector (toNSAttributedString attrString)

-- | @- deleteCharactersInRange:@
deleteCharactersInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
deleteCharactersInRange nsMutableAttributedString range =
  sendMessage nsMutableAttributedString deleteCharactersInRangeSelector range

-- | @- setAttributedString:@
setAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> IO ()
setAttributedString nsMutableAttributedString attrString =
  sendMessage nsMutableAttributedString setAttributedStringSelector (toNSAttributedString attrString)

-- | @- beginEditing@
beginEditing :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO ()
beginEditing nsMutableAttributedString =
  sendMessage nsMutableAttributedString beginEditingSelector

-- | @- endEditing@
endEditing :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO ()
endEditing nsMutableAttributedString =
  sendMessage nsMutableAttributedString endEditingSelector

-- | @- mutableString@
mutableString :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO (Id NSMutableString)
mutableString nsMutableAttributedString =
  sendMessage nsMutableAttributedString mutableStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector '[NSRange, Id NSString] ()
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @setAttributes:range:@
setAttributes_rangeSelector :: Selector '[Id NSDictionary, NSRange] ()
setAttributes_rangeSelector = mkSelector "setAttributes:range:"

-- | @Selector@ for @appendLocalizedFormat:@
appendLocalizedFormatSelector :: Selector '[Id NSAttributedString] ()
appendLocalizedFormatSelector = mkSelector "appendLocalizedFormat:"

-- | @Selector@ for @addAttribute:value:range:@
addAttribute_value_rangeSelector :: Selector '[Id NSString, RawId, NSRange] ()
addAttribute_value_rangeSelector = mkSelector "addAttribute:value:range:"

-- | @Selector@ for @addAttributes:range:@
addAttributes_rangeSelector :: Selector '[Id NSDictionary, NSRange] ()
addAttributes_rangeSelector = mkSelector "addAttributes:range:"

-- | @Selector@ for @removeAttribute:range:@
removeAttribute_rangeSelector :: Selector '[Id NSString, NSRange] ()
removeAttribute_rangeSelector = mkSelector "removeAttribute:range:"

-- | @Selector@ for @replaceCharactersInRange:withAttributedString:@
replaceCharactersInRange_withAttributedStringSelector :: Selector '[NSRange, Id NSAttributedString] ()
replaceCharactersInRange_withAttributedStringSelector = mkSelector "replaceCharactersInRange:withAttributedString:"

-- | @Selector@ for @insertAttributedString:atIndex:@
insertAttributedString_atIndexSelector :: Selector '[Id NSAttributedString, CULong] ()
insertAttributedString_atIndexSelector = mkSelector "insertAttributedString:atIndex:"

-- | @Selector@ for @appendAttributedString:@
appendAttributedStringSelector :: Selector '[Id NSAttributedString] ()
appendAttributedStringSelector = mkSelector "appendAttributedString:"

-- | @Selector@ for @deleteCharactersInRange:@
deleteCharactersInRangeSelector :: Selector '[NSRange] ()
deleteCharactersInRangeSelector = mkSelector "deleteCharactersInRange:"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringSelector = mkSelector "setAttributedString:"

-- | @Selector@ for @beginEditing@
beginEditingSelector :: Selector '[] ()
beginEditingSelector = mkSelector "beginEditing"

-- | @Selector@ for @endEditing@
endEditingSelector :: Selector '[] ()
endEditingSelector = mkSelector "endEditing"

-- | @Selector@ for @mutableString@
mutableStringSelector :: Selector '[] (Id NSMutableString)
mutableStringSelector = mkSelector "mutableString"

