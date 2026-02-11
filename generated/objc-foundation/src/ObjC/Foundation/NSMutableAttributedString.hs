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
  , replaceCharactersInRange_withStringSelector
  , setAttributes_rangeSelector
  , appendLocalizedFormatSelector
  , addAttribute_value_rangeSelector
  , addAttributes_rangeSelector
  , removeAttribute_rangeSelector
  , replaceCharactersInRange_withAttributedStringSelector
  , insertAttributedString_atIndexSelector
  , appendAttributedStringSelector
  , deleteCharactersInRangeSelector
  , setAttributedStringSelector
  , beginEditingSelector
  , endEditingSelector
  , mutableStringSelector


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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString str) => nsMutableAttributedString -> NSRange -> str -> IO ()
replaceCharactersInRange_withString nsMutableAttributedString  range str =
withObjCPtr str $ \raw_str ->
    sendMsg nsMutableAttributedString (mkSelector "replaceCharactersInRange:withString:") retVoid [argNSRange range, argPtr (castPtr raw_str :: Ptr ())]

-- | @- setAttributes:range:@
setAttributes_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSDictionary attrs) => nsMutableAttributedString -> attrs -> NSRange -> IO ()
setAttributes_range nsMutableAttributedString  attrs range =
withObjCPtr attrs $ \raw_attrs ->
    sendMsg nsMutableAttributedString (mkSelector "setAttributes:range:") retVoid [argPtr (castPtr raw_attrs :: Ptr ()), argNSRange range]

-- | Formats the specified string and arguments with the current locale, then appends the result to the receiver.
--
-- ObjC selector: @- appendLocalizedFormat:@
appendLocalizedFormat :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString format) => nsMutableAttributedString -> format -> IO ()
appendLocalizedFormat nsMutableAttributedString  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsMutableAttributedString (mkSelector "appendLocalizedFormat:") retVoid [argPtr (castPtr raw_format :: Ptr ())]

-- | @- addAttribute:value:range:@
addAttribute_value_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString name) => nsMutableAttributedString -> name -> RawId -> NSRange -> IO ()
addAttribute_value_range nsMutableAttributedString  name value range =
withObjCPtr name $ \raw_name ->
    sendMsg nsMutableAttributedString (mkSelector "addAttribute:value:range:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argNSRange range]

-- | @- addAttributes:range:@
addAttributes_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSDictionary attrs) => nsMutableAttributedString -> attrs -> NSRange -> IO ()
addAttributes_range nsMutableAttributedString  attrs range =
withObjCPtr attrs $ \raw_attrs ->
    sendMsg nsMutableAttributedString (mkSelector "addAttributes:range:") retVoid [argPtr (castPtr raw_attrs :: Ptr ()), argNSRange range]

-- | @- removeAttribute:range:@
removeAttribute_range :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSString name) => nsMutableAttributedString -> name -> NSRange -> IO ()
removeAttribute_range nsMutableAttributedString  name range =
withObjCPtr name $ \raw_name ->
    sendMsg nsMutableAttributedString (mkSelector "removeAttribute:range:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argNSRange range]

-- | @- replaceCharactersInRange:withAttributedString:@
replaceCharactersInRange_withAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> NSRange -> attrString -> IO ()
replaceCharactersInRange_withAttributedString nsMutableAttributedString  range attrString =
withObjCPtr attrString $ \raw_attrString ->
    sendMsg nsMutableAttributedString (mkSelector "replaceCharactersInRange:withAttributedString:") retVoid [argNSRange range, argPtr (castPtr raw_attrString :: Ptr ())]

-- | @- insertAttributedString:atIndex:@
insertAttributedString_atIndex :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> CULong -> IO ()
insertAttributedString_atIndex nsMutableAttributedString  attrString loc =
withObjCPtr attrString $ \raw_attrString ->
    sendMsg nsMutableAttributedString (mkSelector "insertAttributedString:atIndex:") retVoid [argPtr (castPtr raw_attrString :: Ptr ()), argCULong (fromIntegral loc)]

-- | @- appendAttributedString:@
appendAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> IO ()
appendAttributedString nsMutableAttributedString  attrString =
withObjCPtr attrString $ \raw_attrString ->
    sendMsg nsMutableAttributedString (mkSelector "appendAttributedString:") retVoid [argPtr (castPtr raw_attrString :: Ptr ())]

-- | @- deleteCharactersInRange:@
deleteCharactersInRange :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> NSRange -> IO ()
deleteCharactersInRange nsMutableAttributedString  range =
  sendMsg nsMutableAttributedString (mkSelector "deleteCharactersInRange:") retVoid [argNSRange range]

-- | @- setAttributedString:@
setAttributedString :: (IsNSMutableAttributedString nsMutableAttributedString, IsNSAttributedString attrString) => nsMutableAttributedString -> attrString -> IO ()
setAttributedString nsMutableAttributedString  attrString =
withObjCPtr attrString $ \raw_attrString ->
    sendMsg nsMutableAttributedString (mkSelector "setAttributedString:") retVoid [argPtr (castPtr raw_attrString :: Ptr ())]

-- | @- beginEditing@
beginEditing :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO ()
beginEditing nsMutableAttributedString  =
  sendMsg nsMutableAttributedString (mkSelector "beginEditing") retVoid []

-- | @- endEditing@
endEditing :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO ()
endEditing nsMutableAttributedString  =
  sendMsg nsMutableAttributedString (mkSelector "endEditing") retVoid []

-- | @- mutableString@
mutableString :: IsNSMutableAttributedString nsMutableAttributedString => nsMutableAttributedString -> IO (Id NSMutableString)
mutableString nsMutableAttributedString  =
  sendMsg nsMutableAttributedString (mkSelector "mutableString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @setAttributes:range:@
setAttributes_rangeSelector :: Selector
setAttributes_rangeSelector = mkSelector "setAttributes:range:"

-- | @Selector@ for @appendLocalizedFormat:@
appendLocalizedFormatSelector :: Selector
appendLocalizedFormatSelector = mkSelector "appendLocalizedFormat:"

-- | @Selector@ for @addAttribute:value:range:@
addAttribute_value_rangeSelector :: Selector
addAttribute_value_rangeSelector = mkSelector "addAttribute:value:range:"

-- | @Selector@ for @addAttributes:range:@
addAttributes_rangeSelector :: Selector
addAttributes_rangeSelector = mkSelector "addAttributes:range:"

-- | @Selector@ for @removeAttribute:range:@
removeAttribute_rangeSelector :: Selector
removeAttribute_rangeSelector = mkSelector "removeAttribute:range:"

-- | @Selector@ for @replaceCharactersInRange:withAttributedString:@
replaceCharactersInRange_withAttributedStringSelector :: Selector
replaceCharactersInRange_withAttributedStringSelector = mkSelector "replaceCharactersInRange:withAttributedString:"

-- | @Selector@ for @insertAttributedString:atIndex:@
insertAttributedString_atIndexSelector :: Selector
insertAttributedString_atIndexSelector = mkSelector "insertAttributedString:atIndex:"

-- | @Selector@ for @appendAttributedString:@
appendAttributedStringSelector :: Selector
appendAttributedStringSelector = mkSelector "appendAttributedString:"

-- | @Selector@ for @deleteCharactersInRange:@
deleteCharactersInRangeSelector :: Selector
deleteCharactersInRangeSelector = mkSelector "deleteCharactersInRange:"

-- | @Selector@ for @setAttributedString:@
setAttributedStringSelector :: Selector
setAttributedStringSelector = mkSelector "setAttributedString:"

-- | @Selector@ for @beginEditing@
beginEditingSelector :: Selector
beginEditingSelector = mkSelector "beginEditing"

-- | @Selector@ for @endEditing@
endEditingSelector :: Selector
endEditingSelector = mkSelector "endEditing"

-- | @Selector@ for @mutableString@
mutableStringSelector :: Selector
mutableStringSelector = mkSelector "mutableString"

