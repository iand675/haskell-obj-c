{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableString@.
module ObjC.Foundation.NSMutableString
  ( NSMutableString
  , IsNSMutableString(..)
  , replaceCharactersInRange_withString
  , insertString_atIndex
  , deleteCharactersInRange
  , appendString
  , appendFormat
  , setString
  , replaceOccurrencesOfString_withString_options_range
  , applyTransform_reverse_range_updatedRange
  , initWithCapacity
  , stringWithCapacity
  , appendFormatSelector
  , appendStringSelector
  , applyTransform_reverse_range_updatedRangeSelector
  , deleteCharactersInRangeSelector
  , initWithCapacitySelector
  , insertString_atIndexSelector
  , replaceCharactersInRange_withStringSelector
  , replaceOccurrencesOfString_withString_options_rangeSelector
  , setStringSelector
  , stringWithCapacitySelector

  -- * Enum types
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> NSRange -> aString -> IO ()
replaceCharactersInRange_withString nsMutableString range aString =
  sendMessage nsMutableString replaceCharactersInRange_withStringSelector range (toNSString aString)

-- | @- insertString:atIndex:@
insertString_atIndex :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> CULong -> IO ()
insertString_atIndex nsMutableString aString loc =
  sendMessage nsMutableString insertString_atIndexSelector (toNSString aString) loc

-- | @- deleteCharactersInRange:@
deleteCharactersInRange :: IsNSMutableString nsMutableString => nsMutableString -> NSRange -> IO ()
deleteCharactersInRange nsMutableString range =
  sendMessage nsMutableString deleteCharactersInRangeSelector range

-- | @- appendString:@
appendString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> IO ()
appendString nsMutableString aString =
  sendMessage nsMutableString appendStringSelector (toNSString aString)

-- | @- appendFormat:@
appendFormat :: (IsNSMutableString nsMutableString, IsNSString format) => nsMutableString -> format -> IO ()
appendFormat nsMutableString format =
  sendMessage nsMutableString appendFormatSelector (toNSString format)

-- | @- setString:@
setString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> IO ()
setString nsMutableString aString =
  sendMessage nsMutableString setStringSelector (toNSString aString)

-- | @- replaceOccurrencesOfString:withString:options:range:@
replaceOccurrencesOfString_withString_options_range :: (IsNSMutableString nsMutableString, IsNSString target, IsNSString replacement) => nsMutableString -> target -> replacement -> NSStringCompareOptions -> NSRange -> IO CULong
replaceOccurrencesOfString_withString_options_range nsMutableString target replacement options searchRange =
  sendMessage nsMutableString replaceOccurrencesOfString_withString_options_rangeSelector (toNSString target) (toNSString replacement) options searchRange

-- | @- applyTransform:reverse:range:updatedRange:@
applyTransform_reverse_range_updatedRange :: (IsNSMutableString nsMutableString, IsNSString transform) => nsMutableString -> transform -> Bool -> NSRange -> Ptr NSRange -> IO Bool
applyTransform_reverse_range_updatedRange nsMutableString transform reverse_ range resultingRange =
  sendMessage nsMutableString applyTransform_reverse_range_updatedRangeSelector (toNSString transform) reverse_ range resultingRange

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableString nsMutableString => nsMutableString -> CULong -> IO (Id NSMutableString)
initWithCapacity nsMutableString capacity =
  sendOwnedMessage nsMutableString initWithCapacitySelector capacity

-- | @+ stringWithCapacity:@
stringWithCapacity :: CULong -> IO (Id NSMutableString)
stringWithCapacity capacity =
  do
    cls' <- getRequiredClass "NSMutableString"
    sendClassMessage cls' stringWithCapacitySelector capacity

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector '[NSRange, Id NSString] ()
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @insertString:atIndex:@
insertString_atIndexSelector :: Selector '[Id NSString, CULong] ()
insertString_atIndexSelector = mkSelector "insertString:atIndex:"

-- | @Selector@ for @deleteCharactersInRange:@
deleteCharactersInRangeSelector :: Selector '[NSRange] ()
deleteCharactersInRangeSelector = mkSelector "deleteCharactersInRange:"

-- | @Selector@ for @appendString:@
appendStringSelector :: Selector '[Id NSString] ()
appendStringSelector = mkSelector "appendString:"

-- | @Selector@ for @appendFormat:@
appendFormatSelector :: Selector '[Id NSString] ()
appendFormatSelector = mkSelector "appendFormat:"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[Id NSString] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @replaceOccurrencesOfString:withString:options:range:@
replaceOccurrencesOfString_withString_options_rangeSelector :: Selector '[Id NSString, Id NSString, NSStringCompareOptions, NSRange] CULong
replaceOccurrencesOfString_withString_options_rangeSelector = mkSelector "replaceOccurrencesOfString:withString:options:range:"

-- | @Selector@ for @applyTransform:reverse:range:updatedRange:@
applyTransform_reverse_range_updatedRangeSelector :: Selector '[Id NSString, Bool, NSRange, Ptr NSRange] Bool
applyTransform_reverse_range_updatedRangeSelector = mkSelector "applyTransform:reverse:range:updatedRange:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableString)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @stringWithCapacity:@
stringWithCapacitySelector :: Selector '[CULong] (Id NSMutableString)
stringWithCapacitySelector = mkSelector "stringWithCapacity:"

