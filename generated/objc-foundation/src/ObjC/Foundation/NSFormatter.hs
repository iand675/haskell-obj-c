{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFormatter@.
module ObjC.Foundation.NSFormatter
  ( NSFormatter
  , IsNSFormatter(..)
  , stringForObjectValue
  , attributedStringForObjectValue_withDefaultAttributes
  , editingStringForObjectValue
  , getObjectValue_forString_errorDescription
  , isPartialStringValid_newEditingString_errorDescription
  , isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription
  , stringForObjectValueSelector
  , attributedStringForObjectValue_withDefaultAttributesSelector
  , editingStringForObjectValueSelector
  , getObjectValue_forString_errorDescriptionSelector
  , isPartialStringValid_newEditingString_errorDescriptionSelector
  , isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector


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

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSFormatter nsFormatter => nsFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsFormatter  obj_ =
  sendMsg nsFormatter (mkSelector "stringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- attributedStringForObjectValue:withDefaultAttributes:@
attributedStringForObjectValue_withDefaultAttributes :: (IsNSFormatter nsFormatter, IsNSDictionary attrs) => nsFormatter -> RawId -> attrs -> IO (Id NSAttributedString)
attributedStringForObjectValue_withDefaultAttributes nsFormatter  obj_ attrs =
withObjCPtr attrs $ \raw_attrs ->
    sendMsg nsFormatter (mkSelector "attributedStringForObjectValue:withDefaultAttributes:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr raw_attrs :: Ptr ())] >>= retainedObject . castPtr

-- | @- editingStringForObjectValue:@
editingStringForObjectValue :: IsNSFormatter nsFormatter => nsFormatter -> RawId -> IO (Id NSString)
editingStringForObjectValue nsFormatter  obj_ =
  sendMsg nsFormatter (mkSelector "editingStringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSFormatter nsFormatter, IsNSString string, IsNSString error_) => nsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- isPartialStringValid:newEditingString:errorDescription:@
isPartialStringValid_newEditingString_errorDescription :: (IsNSFormatter nsFormatter, IsNSString partialString, IsNSString newString, IsNSString error_) => nsFormatter -> partialString -> newString -> error_ -> IO Bool
isPartialStringValid_newEditingString_errorDescription nsFormatter  partialString newString error_ =
withObjCPtr partialString $ \raw_partialString ->
  withObjCPtr newString $ \raw_newString ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFormatter (mkSelector "isPartialStringValid:newEditingString:errorDescription:") retCULong [argPtr (castPtr raw_partialString :: Ptr ()), argPtr (castPtr raw_newString :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:@
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription :: (IsNSFormatter nsFormatter, IsNSString partialStringPtr, IsNSString origString, IsNSString error_) => nsFormatter -> partialStringPtr -> Ptr NSRange -> origString -> NSRange -> error_ -> IO Bool
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription nsFormatter  partialStringPtr proposedSelRangePtr origString origSelRange error_ =
withObjCPtr partialStringPtr $ \raw_partialStringPtr ->
  withObjCPtr origString $ \raw_origString ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFormatter (mkSelector "isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:") retCULong [argPtr (castPtr raw_partialStringPtr :: Ptr ()), argPtr proposedSelRangePtr, argPtr (castPtr raw_origString :: Ptr ()), argNSRange origSelRange, argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @attributedStringForObjectValue:withDefaultAttributes:@
attributedStringForObjectValue_withDefaultAttributesSelector :: Selector
attributedStringForObjectValue_withDefaultAttributesSelector = mkSelector "attributedStringForObjectValue:withDefaultAttributes:"

-- | @Selector@ for @editingStringForObjectValue:@
editingStringForObjectValueSelector :: Selector
editingStringForObjectValueSelector = mkSelector "editingStringForObjectValue:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @isPartialStringValid:newEditingString:errorDescription:@
isPartialStringValid_newEditingString_errorDescriptionSelector :: Selector
isPartialStringValid_newEditingString_errorDescriptionSelector = mkSelector "isPartialStringValid:newEditingString:errorDescription:"

-- | @Selector@ for @isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:@
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector :: Selector
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector = mkSelector "isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:"

