{-# LANGUAGE PatternSynonyms #-}
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
  , replaceCharactersInRange_withStringSelector
  , insertString_atIndexSelector
  , deleteCharactersInRangeSelector
  , appendStringSelector
  , appendFormatSelector
  , setStringSelector
  , replaceOccurrencesOfString_withString_options_rangeSelector
  , applyTransform_reverse_range_updatedRangeSelector
  , initWithCapacitySelector
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
import ObjC.Foundation.Internal.Enums

-- | @- replaceCharactersInRange:withString:@
replaceCharactersInRange_withString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> NSRange -> aString -> IO ()
replaceCharactersInRange_withString nsMutableString  range aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableString (mkSelector "replaceCharactersInRange:withString:") retVoid [argNSRange range, argPtr (castPtr raw_aString :: Ptr ())]

-- | @- insertString:atIndex:@
insertString_atIndex :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> CULong -> IO ()
insertString_atIndex nsMutableString  aString loc =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableString (mkSelector "insertString:atIndex:") retVoid [argPtr (castPtr raw_aString :: Ptr ()), argCULong (fromIntegral loc)]

-- | @- deleteCharactersInRange:@
deleteCharactersInRange :: IsNSMutableString nsMutableString => nsMutableString -> NSRange -> IO ()
deleteCharactersInRange nsMutableString  range =
  sendMsg nsMutableString (mkSelector "deleteCharactersInRange:") retVoid [argNSRange range]

-- | @- appendString:@
appendString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> IO ()
appendString nsMutableString  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableString (mkSelector "appendString:") retVoid [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- appendFormat:@
appendFormat :: (IsNSMutableString nsMutableString, IsNSString format) => nsMutableString -> format -> IO ()
appendFormat nsMutableString  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsMutableString (mkSelector "appendFormat:") retVoid [argPtr (castPtr raw_format :: Ptr ())]

-- | @- setString:@
setString :: (IsNSMutableString nsMutableString, IsNSString aString) => nsMutableString -> aString -> IO ()
setString nsMutableString  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableString (mkSelector "setString:") retVoid [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- replaceOccurrencesOfString:withString:options:range:@
replaceOccurrencesOfString_withString_options_range :: (IsNSMutableString nsMutableString, IsNSString target, IsNSString replacement) => nsMutableString -> target -> replacement -> NSStringCompareOptions -> NSRange -> IO CULong
replaceOccurrencesOfString_withString_options_range nsMutableString  target replacement options searchRange =
withObjCPtr target $ \raw_target ->
  withObjCPtr replacement $ \raw_replacement ->
      sendMsg nsMutableString (mkSelector "replaceOccurrencesOfString:withString:options:range:") retCULong [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_replacement :: Ptr ()), argCULong (coerce options), argNSRange searchRange]

-- | @- applyTransform:reverse:range:updatedRange:@
applyTransform_reverse_range_updatedRange :: (IsNSMutableString nsMutableString, IsNSString transform) => nsMutableString -> transform -> Bool -> NSRange -> Ptr NSRange -> IO Bool
applyTransform_reverse_range_updatedRange nsMutableString  transform reverse_ range resultingRange =
withObjCPtr transform $ \raw_transform ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableString (mkSelector "applyTransform:reverse:range:updatedRange:") retCULong [argPtr (castPtr raw_transform :: Ptr ()), argCULong (if reverse_ then 1 else 0), argNSRange range, argPtr resultingRange]

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableString nsMutableString => nsMutableString -> CULong -> IO (Id NSMutableString)
initWithCapacity nsMutableString  capacity =
  sendMsg nsMutableString (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral capacity)] >>= ownedObject . castPtr

-- | @+ stringWithCapacity:@
stringWithCapacity :: CULong -> IO (Id NSMutableString)
stringWithCapacity capacity =
  do
    cls' <- getRequiredClass "NSMutableString"
    sendClassMsg cls' (mkSelector "stringWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral capacity)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replaceCharactersInRange:withString:@
replaceCharactersInRange_withStringSelector :: Selector
replaceCharactersInRange_withStringSelector = mkSelector "replaceCharactersInRange:withString:"

-- | @Selector@ for @insertString:atIndex:@
insertString_atIndexSelector :: Selector
insertString_atIndexSelector = mkSelector "insertString:atIndex:"

-- | @Selector@ for @deleteCharactersInRange:@
deleteCharactersInRangeSelector :: Selector
deleteCharactersInRangeSelector = mkSelector "deleteCharactersInRange:"

-- | @Selector@ for @appendString:@
appendStringSelector :: Selector
appendStringSelector = mkSelector "appendString:"

-- | @Selector@ for @appendFormat:@
appendFormatSelector :: Selector
appendFormatSelector = mkSelector "appendFormat:"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @replaceOccurrencesOfString:withString:options:range:@
replaceOccurrencesOfString_withString_options_rangeSelector :: Selector
replaceOccurrencesOfString_withString_options_rangeSelector = mkSelector "replaceOccurrencesOfString:withString:options:range:"

-- | @Selector@ for @applyTransform:reverse:range:updatedRange:@
applyTransform_reverse_range_updatedRangeSelector :: Selector
applyTransform_reverse_range_updatedRangeSelector = mkSelector "applyTransform:reverse:range:updatedRange:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @stringWithCapacity:@
stringWithCapacitySelector :: Selector
stringWithCapacitySelector = mkSelector "stringWithCapacity:"

