{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNumber@.
module ObjC.Foundation.NSNumber
  ( NSNumber
  , IsNSNumber(..)
  , initWithCoder
  , initWithChar
  , initWithUnsignedChar
  , initWithShort
  , initWithUnsignedShort
  , initWithInt
  , initWithUnsignedInt
  , initWithLong
  , initWithUnsignedLong
  , initWithLongLong
  , initWithUnsignedLongLong
  , initWithFloat
  , initWithDouble
  , initWithBool
  , initWithInteger
  , initWithUnsignedInteger
  , compare_
  , isEqualToNumber
  , descriptionWithLocale
  , numberWithChar
  , numberWithUnsignedChar
  , numberWithShort
  , numberWithUnsignedShort
  , numberWithInt
  , numberWithUnsignedInt
  , numberWithLong
  , numberWithUnsignedLong
  , numberWithLongLong
  , numberWithUnsignedLongLong
  , numberWithFloat
  , numberWithDouble
  , numberWithBool
  , numberWithInteger
  , numberWithUnsignedInteger
  , charValue
  , unsignedCharValue
  , shortValue
  , unsignedShortValue
  , intValue
  , unsignedIntValue
  , longValue
  , unsignedLongValue
  , longLongValue
  , unsignedLongLongValue
  , floatValue
  , doubleValue
  , boolValue
  , integerValue
  , unsignedIntegerValue
  , stringValue
  , initWithCoderSelector
  , initWithCharSelector
  , initWithUnsignedCharSelector
  , initWithShortSelector
  , initWithUnsignedShortSelector
  , initWithIntSelector
  , initWithUnsignedIntSelector
  , initWithLongSelector
  , initWithUnsignedLongSelector
  , initWithLongLongSelector
  , initWithUnsignedLongLongSelector
  , initWithFloatSelector
  , initWithDoubleSelector
  , initWithBoolSelector
  , initWithIntegerSelector
  , initWithUnsignedIntegerSelector
  , compareSelector
  , isEqualToNumberSelector
  , descriptionWithLocaleSelector
  , numberWithCharSelector
  , numberWithUnsignedCharSelector
  , numberWithShortSelector
  , numberWithUnsignedShortSelector
  , numberWithIntSelector
  , numberWithUnsignedIntSelector
  , numberWithLongSelector
  , numberWithUnsignedLongSelector
  , numberWithLongLongSelector
  , numberWithUnsignedLongLongSelector
  , numberWithFloatSelector
  , numberWithDoubleSelector
  , numberWithBoolSelector
  , numberWithIntegerSelector
  , numberWithUnsignedIntegerSelector
  , charValueSelector
  , unsignedCharValueSelector
  , shortValueSelector
  , unsignedShortValueSelector
  , intValueSelector
  , unsignedIntValueSelector
  , longValueSelector
  , unsignedLongValueSelector
  , longLongValueSelector
  , unsignedLongLongValueSelector
  , floatValueSelector
  , doubleValueSelector
  , boolValueSelector
  , integerValueSelector
  , unsignedIntegerValueSelector
  , stringValueSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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
import ObjC.Foundation.Internal.Enums

-- | @- initWithCoder:@
initWithCoder :: (IsNSNumber nsNumber, IsNSCoder coder) => nsNumber -> coder -> IO (Id NSNumber)
initWithCoder nsNumber  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsNumber (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithChar:@
initWithChar :: IsNSNumber nsNumber => nsNumber -> CChar -> IO (Id NSNumber)
initWithChar nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithChar:") (retPtr retVoid) [argCChar (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedChar:@
initWithUnsignedChar :: IsNSNumber nsNumber => nsNumber -> CUChar -> IO (Id NSNumber)
initWithUnsignedChar nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedChar:") (retPtr retVoid) [argCUChar (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithShort:@
initWithShort :: IsNSNumber nsNumber => nsNumber -> CShort -> IO (Id NSNumber)
initWithShort nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithShort:") (retPtr retVoid) [argCInt (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedShort:@
initWithUnsignedShort :: IsNSNumber nsNumber => nsNumber -> CUShort -> IO (Id NSNumber)
initWithUnsignedShort nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedShort:") (retPtr retVoid) [argCUInt (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithInt:@
initWithInt :: IsNSNumber nsNumber => nsNumber -> CInt -> IO (Id NSNumber)
initWithInt nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithInt:") (retPtr retVoid) [argCInt (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedInt:@
initWithUnsignedInt :: IsNSNumber nsNumber => nsNumber -> CUInt -> IO (Id NSNumber)
initWithUnsignedInt nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedInt:") (retPtr retVoid) [argCUInt (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithLong:@
initWithLong :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithLong nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithLong:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedLong:@
initWithUnsignedLong :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedLong nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedLong:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithLongLong:@
initWithLongLong :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithLongLong nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithLongLong:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedLongLong:@
initWithUnsignedLongLong :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedLongLong nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedLongLong:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithFloat:@
initWithFloat :: IsNSNumber nsNumber => nsNumber -> CFloat -> IO (Id NSNumber)
initWithFloat nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithFloat:") (retPtr retVoid) [argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithDouble:@
initWithDouble :: IsNSNumber nsNumber => nsNumber -> CDouble -> IO (Id NSNumber)
initWithDouble nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithDouble:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithBool:@
initWithBool :: IsNSNumber nsNumber => nsNumber -> Bool -> IO (Id NSNumber)
initWithBool nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithBool:") (retPtr retVoid) [argCULong (if value then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithInteger:@
initWithInteger :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithInteger nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithInteger:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithUnsignedInteger:@
initWithUnsignedInteger :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedInteger nsNumber  value =
  sendMsg nsNumber (mkSelector "initWithUnsignedInteger:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= ownedObject . castPtr

-- | @- compare:@
compare_ :: (IsNSNumber nsNumber, IsNSNumber otherNumber) => nsNumber -> otherNumber -> IO NSComparisonResult
compare_ nsNumber  otherNumber =
withObjCPtr otherNumber $ \raw_otherNumber ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsNumber (mkSelector "compare:") retCLong [argPtr (castPtr raw_otherNumber :: Ptr ())]

-- | @- isEqualToNumber:@
isEqualToNumber :: (IsNSNumber nsNumber, IsNSNumber number) => nsNumber -> number -> IO Bool
isEqualToNumber nsNumber  number =
withObjCPtr number $ \raw_number ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumber (mkSelector "isEqualToNumber:") retCULong [argPtr (castPtr raw_number :: Ptr ())]

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSNumber nsNumber => nsNumber -> RawId -> IO (Id NSString)
descriptionWithLocale nsNumber  locale =
  sendMsg nsNumber (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ numberWithChar:@
numberWithChar :: CChar -> IO (Id NSNumber)
numberWithChar value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithChar:") (retPtr retVoid) [argCChar (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedChar:@
numberWithUnsignedChar :: CUChar -> IO (Id NSNumber)
numberWithUnsignedChar value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedChar:") (retPtr retVoid) [argCUChar (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithShort:@
numberWithShort :: CShort -> IO (Id NSNumber)
numberWithShort value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithShort:") (retPtr retVoid) [argCInt (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedShort:@
numberWithUnsignedShort :: CUShort -> IO (Id NSNumber)
numberWithUnsignedShort value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedShort:") (retPtr retVoid) [argCUInt (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithInt:@
numberWithInt :: CInt -> IO (Id NSNumber)
numberWithInt value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithInt:") (retPtr retVoid) [argCInt (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedInt:@
numberWithUnsignedInt :: CUInt -> IO (Id NSNumber)
numberWithUnsignedInt value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedInt:") (retPtr retVoid) [argCUInt (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithLong:@
numberWithLong :: CLong -> IO (Id NSNumber)
numberWithLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithLong:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedLong:@
numberWithUnsignedLong :: CULong -> IO (Id NSNumber)
numberWithUnsignedLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedLong:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithLongLong:@
numberWithLongLong :: CLong -> IO (Id NSNumber)
numberWithLongLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithLongLong:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedLongLong:@
numberWithUnsignedLongLong :: CULong -> IO (Id NSNumber)
numberWithUnsignedLongLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedLongLong:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithFloat:@
numberWithFloat :: CFloat -> IO (Id NSNumber)
numberWithFloat value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithFloat:") (retPtr retVoid) [argCFloat (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithDouble:@
numberWithDouble :: CDouble -> IO (Id NSNumber)
numberWithDouble value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithDouble:") (retPtr retVoid) [argCDouble (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithBool:@
numberWithBool :: Bool -> IO (Id NSNumber)
numberWithBool value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithBool:") (retPtr retVoid) [argCULong (if value then 1 else 0)] >>= retainedObject . castPtr

-- | @+ numberWithInteger:@
numberWithInteger :: CLong -> IO (Id NSNumber)
numberWithInteger value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithInteger:") (retPtr retVoid) [argCLong (fromIntegral value)] >>= retainedObject . castPtr

-- | @+ numberWithUnsignedInteger:@
numberWithUnsignedInteger :: CULong -> IO (Id NSNumber)
numberWithUnsignedInteger value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMsg cls' (mkSelector "numberWithUnsignedInteger:") (retPtr retVoid) [argCULong (fromIntegral value)] >>= retainedObject . castPtr

-- | @- charValue@
charValue :: IsNSNumber nsNumber => nsNumber -> IO CChar
charValue nsNumber  =
  sendMsg nsNumber (mkSelector "charValue") retCChar []

-- | @- unsignedCharValue@
unsignedCharValue :: IsNSNumber nsNumber => nsNumber -> IO CUChar
unsignedCharValue nsNumber  =
  sendMsg nsNumber (mkSelector "unsignedCharValue") retCUChar []

-- | @- shortValue@
shortValue :: IsNSNumber nsNumber => nsNumber -> IO CShort
shortValue nsNumber  =
  fmap fromIntegral $ sendMsg nsNumber (mkSelector "shortValue") retCInt []

-- | @- unsignedShortValue@
unsignedShortValue :: IsNSNumber nsNumber => nsNumber -> IO CUShort
unsignedShortValue nsNumber  =
  fmap fromIntegral $ sendMsg nsNumber (mkSelector "unsignedShortValue") retCUInt []

-- | @- intValue@
intValue :: IsNSNumber nsNumber => nsNumber -> IO CInt
intValue nsNumber  =
  sendMsg nsNumber (mkSelector "intValue") retCInt []

-- | @- unsignedIntValue@
unsignedIntValue :: IsNSNumber nsNumber => nsNumber -> IO CUInt
unsignedIntValue nsNumber  =
  sendMsg nsNumber (mkSelector "unsignedIntValue") retCUInt []

-- | @- longValue@
longValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
longValue nsNumber  =
  sendMsg nsNumber (mkSelector "longValue") retCLong []

-- | @- unsignedLongValue@
unsignedLongValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedLongValue nsNumber  =
  sendMsg nsNumber (mkSelector "unsignedLongValue") retCULong []

-- | @- longLongValue@
longLongValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
longLongValue nsNumber  =
  sendMsg nsNumber (mkSelector "longLongValue") retCLong []

-- | @- unsignedLongLongValue@
unsignedLongLongValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedLongLongValue nsNumber  =
  sendMsg nsNumber (mkSelector "unsignedLongLongValue") retCULong []

-- | @- floatValue@
floatValue :: IsNSNumber nsNumber => nsNumber -> IO CFloat
floatValue nsNumber  =
  sendMsg nsNumber (mkSelector "floatValue") retCFloat []

-- | @- doubleValue@
doubleValue :: IsNSNumber nsNumber => nsNumber -> IO CDouble
doubleValue nsNumber  =
  sendMsg nsNumber (mkSelector "doubleValue") retCDouble []

-- | @- boolValue@
boolValue :: IsNSNumber nsNumber => nsNumber -> IO Bool
boolValue nsNumber  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumber (mkSelector "boolValue") retCULong []

-- | @- integerValue@
integerValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
integerValue nsNumber  =
  sendMsg nsNumber (mkSelector "integerValue") retCLong []

-- | @- unsignedIntegerValue@
unsignedIntegerValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedIntegerValue nsNumber  =
  sendMsg nsNumber (mkSelector "unsignedIntegerValue") retCULong []

-- | @- stringValue@
stringValue :: IsNSNumber nsNumber => nsNumber -> IO (Id NSString)
stringValue nsNumber  =
  sendMsg nsNumber (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithChar:@
initWithCharSelector :: Selector
initWithCharSelector = mkSelector "initWithChar:"

-- | @Selector@ for @initWithUnsignedChar:@
initWithUnsignedCharSelector :: Selector
initWithUnsignedCharSelector = mkSelector "initWithUnsignedChar:"

-- | @Selector@ for @initWithShort:@
initWithShortSelector :: Selector
initWithShortSelector = mkSelector "initWithShort:"

-- | @Selector@ for @initWithUnsignedShort:@
initWithUnsignedShortSelector :: Selector
initWithUnsignedShortSelector = mkSelector "initWithUnsignedShort:"

-- | @Selector@ for @initWithInt:@
initWithIntSelector :: Selector
initWithIntSelector = mkSelector "initWithInt:"

-- | @Selector@ for @initWithUnsignedInt:@
initWithUnsignedIntSelector :: Selector
initWithUnsignedIntSelector = mkSelector "initWithUnsignedInt:"

-- | @Selector@ for @initWithLong:@
initWithLongSelector :: Selector
initWithLongSelector = mkSelector "initWithLong:"

-- | @Selector@ for @initWithUnsignedLong:@
initWithUnsignedLongSelector :: Selector
initWithUnsignedLongSelector = mkSelector "initWithUnsignedLong:"

-- | @Selector@ for @initWithLongLong:@
initWithLongLongSelector :: Selector
initWithLongLongSelector = mkSelector "initWithLongLong:"

-- | @Selector@ for @initWithUnsignedLongLong:@
initWithUnsignedLongLongSelector :: Selector
initWithUnsignedLongLongSelector = mkSelector "initWithUnsignedLongLong:"

-- | @Selector@ for @initWithFloat:@
initWithFloatSelector :: Selector
initWithFloatSelector = mkSelector "initWithFloat:"

-- | @Selector@ for @initWithDouble:@
initWithDoubleSelector :: Selector
initWithDoubleSelector = mkSelector "initWithDouble:"

-- | @Selector@ for @initWithBool:@
initWithBoolSelector :: Selector
initWithBoolSelector = mkSelector "initWithBool:"

-- | @Selector@ for @initWithInteger:@
initWithIntegerSelector :: Selector
initWithIntegerSelector = mkSelector "initWithInteger:"

-- | @Selector@ for @initWithUnsignedInteger:@
initWithUnsignedIntegerSelector :: Selector
initWithUnsignedIntegerSelector = mkSelector "initWithUnsignedInteger:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToNumber:@
isEqualToNumberSelector :: Selector
isEqualToNumberSelector = mkSelector "isEqualToNumber:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @numberWithChar:@
numberWithCharSelector :: Selector
numberWithCharSelector = mkSelector "numberWithChar:"

-- | @Selector@ for @numberWithUnsignedChar:@
numberWithUnsignedCharSelector :: Selector
numberWithUnsignedCharSelector = mkSelector "numberWithUnsignedChar:"

-- | @Selector@ for @numberWithShort:@
numberWithShortSelector :: Selector
numberWithShortSelector = mkSelector "numberWithShort:"

-- | @Selector@ for @numberWithUnsignedShort:@
numberWithUnsignedShortSelector :: Selector
numberWithUnsignedShortSelector = mkSelector "numberWithUnsignedShort:"

-- | @Selector@ for @numberWithInt:@
numberWithIntSelector :: Selector
numberWithIntSelector = mkSelector "numberWithInt:"

-- | @Selector@ for @numberWithUnsignedInt:@
numberWithUnsignedIntSelector :: Selector
numberWithUnsignedIntSelector = mkSelector "numberWithUnsignedInt:"

-- | @Selector@ for @numberWithLong:@
numberWithLongSelector :: Selector
numberWithLongSelector = mkSelector "numberWithLong:"

-- | @Selector@ for @numberWithUnsignedLong:@
numberWithUnsignedLongSelector :: Selector
numberWithUnsignedLongSelector = mkSelector "numberWithUnsignedLong:"

-- | @Selector@ for @numberWithLongLong:@
numberWithLongLongSelector :: Selector
numberWithLongLongSelector = mkSelector "numberWithLongLong:"

-- | @Selector@ for @numberWithUnsignedLongLong:@
numberWithUnsignedLongLongSelector :: Selector
numberWithUnsignedLongLongSelector = mkSelector "numberWithUnsignedLongLong:"

-- | @Selector@ for @numberWithFloat:@
numberWithFloatSelector :: Selector
numberWithFloatSelector = mkSelector "numberWithFloat:"

-- | @Selector@ for @numberWithDouble:@
numberWithDoubleSelector :: Selector
numberWithDoubleSelector = mkSelector "numberWithDouble:"

-- | @Selector@ for @numberWithBool:@
numberWithBoolSelector :: Selector
numberWithBoolSelector = mkSelector "numberWithBool:"

-- | @Selector@ for @numberWithInteger:@
numberWithIntegerSelector :: Selector
numberWithIntegerSelector = mkSelector "numberWithInteger:"

-- | @Selector@ for @numberWithUnsignedInteger:@
numberWithUnsignedIntegerSelector :: Selector
numberWithUnsignedIntegerSelector = mkSelector "numberWithUnsignedInteger:"

-- | @Selector@ for @charValue@
charValueSelector :: Selector
charValueSelector = mkSelector "charValue"

-- | @Selector@ for @unsignedCharValue@
unsignedCharValueSelector :: Selector
unsignedCharValueSelector = mkSelector "unsignedCharValue"

-- | @Selector@ for @shortValue@
shortValueSelector :: Selector
shortValueSelector = mkSelector "shortValue"

-- | @Selector@ for @unsignedShortValue@
unsignedShortValueSelector :: Selector
unsignedShortValueSelector = mkSelector "unsignedShortValue"

-- | @Selector@ for @intValue@
intValueSelector :: Selector
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @unsignedIntValue@
unsignedIntValueSelector :: Selector
unsignedIntValueSelector = mkSelector "unsignedIntValue"

-- | @Selector@ for @longValue@
longValueSelector :: Selector
longValueSelector = mkSelector "longValue"

-- | @Selector@ for @unsignedLongValue@
unsignedLongValueSelector :: Selector
unsignedLongValueSelector = mkSelector "unsignedLongValue"

-- | @Selector@ for @longLongValue@
longLongValueSelector :: Selector
longLongValueSelector = mkSelector "longLongValue"

-- | @Selector@ for @unsignedLongLongValue@
unsignedLongLongValueSelector :: Selector
unsignedLongLongValueSelector = mkSelector "unsignedLongLongValue"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @boolValue@
boolValueSelector :: Selector
boolValueSelector = mkSelector "boolValue"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @unsignedIntegerValue@
unsignedIntegerValueSelector :: Selector
unsignedIntegerValueSelector = mkSelector "unsignedIntegerValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

