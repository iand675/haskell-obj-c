{-# LANGUAGE DataKinds #-}
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
  , attributedStringForObjectValue_withDefaultAttributesSelector
  , editingStringForObjectValueSelector
  , getObjectValue_forString_errorDescriptionSelector
  , isPartialStringValid_newEditingString_errorDescriptionSelector
  , isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector
  , stringForObjectValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSFormatter nsFormatter => nsFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsFormatter obj_ =
  sendMessage nsFormatter stringForObjectValueSelector obj_

-- | @- attributedStringForObjectValue:withDefaultAttributes:@
attributedStringForObjectValue_withDefaultAttributes :: (IsNSFormatter nsFormatter, IsNSDictionary attrs) => nsFormatter -> RawId -> attrs -> IO (Id NSAttributedString)
attributedStringForObjectValue_withDefaultAttributes nsFormatter obj_ attrs =
  sendMessage nsFormatter attributedStringForObjectValue_withDefaultAttributesSelector obj_ (toNSDictionary attrs)

-- | @- editingStringForObjectValue:@
editingStringForObjectValue :: IsNSFormatter nsFormatter => nsFormatter -> RawId -> IO (Id NSString)
editingStringForObjectValue nsFormatter obj_ =
  sendMessage nsFormatter editingStringForObjectValueSelector obj_

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSFormatter nsFormatter, IsNSString string, IsNSString error_) => nsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsFormatter obj_ string error_ =
  sendMessage nsFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- isPartialStringValid:newEditingString:errorDescription:@
isPartialStringValid_newEditingString_errorDescription :: (IsNSFormatter nsFormatter, IsNSString partialString, IsNSString newString, IsNSString error_) => nsFormatter -> partialString -> newString -> error_ -> IO Bool
isPartialStringValid_newEditingString_errorDescription nsFormatter partialString newString error_ =
  sendMessage nsFormatter isPartialStringValid_newEditingString_errorDescriptionSelector (toNSString partialString) (toNSString newString) (toNSString error_)

-- | @- isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:@
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription :: (IsNSFormatter nsFormatter, IsNSString partialStringPtr, IsNSString origString, IsNSString error_) => nsFormatter -> partialStringPtr -> Ptr NSRange -> origString -> NSRange -> error_ -> IO Bool
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription nsFormatter partialStringPtr proposedSelRangePtr origString origSelRange error_ =
  sendMessage nsFormatter isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector (toNSString partialStringPtr) proposedSelRangePtr (toNSString origString) origSelRange (toNSString error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector '[RawId] (Id NSString)
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @attributedStringForObjectValue:withDefaultAttributes:@
attributedStringForObjectValue_withDefaultAttributesSelector :: Selector '[RawId, Id NSDictionary] (Id NSAttributedString)
attributedStringForObjectValue_withDefaultAttributesSelector = mkSelector "attributedStringForObjectValue:withDefaultAttributes:"

-- | @Selector@ for @editingStringForObjectValue:@
editingStringForObjectValueSelector :: Selector '[RawId] (Id NSString)
editingStringForObjectValueSelector = mkSelector "editingStringForObjectValue:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector '[Ptr RawId, Id NSString, Id NSString] Bool
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @isPartialStringValid:newEditingString:errorDescription:@
isPartialStringValid_newEditingString_errorDescriptionSelector :: Selector '[Id NSString, Id NSString, Id NSString] Bool
isPartialStringValid_newEditingString_errorDescriptionSelector = mkSelector "isPartialStringValid:newEditingString:errorDescription:"

-- | @Selector@ for @isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:@
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector :: Selector '[Id NSString, Ptr NSRange, Id NSString, NSRange, Id NSString] Bool
isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescriptionSelector = mkSelector "isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:"

