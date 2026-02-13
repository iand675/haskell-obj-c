{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , boolValueSelector
  , charValueSelector
  , compareSelector
  , descriptionWithLocaleSelector
  , doubleValueSelector
  , floatValueSelector
  , initWithBoolSelector
  , initWithCharSelector
  , initWithCoderSelector
  , initWithDoubleSelector
  , initWithFloatSelector
  , initWithIntSelector
  , initWithIntegerSelector
  , initWithLongLongSelector
  , initWithLongSelector
  , initWithShortSelector
  , initWithUnsignedCharSelector
  , initWithUnsignedIntSelector
  , initWithUnsignedIntegerSelector
  , initWithUnsignedLongLongSelector
  , initWithUnsignedLongSelector
  , initWithUnsignedShortSelector
  , intValueSelector
  , integerValueSelector
  , isEqualToNumberSelector
  , longLongValueSelector
  , longValueSelector
  , numberWithBoolSelector
  , numberWithCharSelector
  , numberWithDoubleSelector
  , numberWithFloatSelector
  , numberWithIntSelector
  , numberWithIntegerSelector
  , numberWithLongLongSelector
  , numberWithLongSelector
  , numberWithShortSelector
  , numberWithUnsignedCharSelector
  , numberWithUnsignedIntSelector
  , numberWithUnsignedIntegerSelector
  , numberWithUnsignedLongLongSelector
  , numberWithUnsignedLongSelector
  , numberWithUnsignedShortSelector
  , shortValueSelector
  , stringValueSelector
  , unsignedCharValueSelector
  , unsignedIntValueSelector
  , unsignedIntegerValueSelector
  , unsignedLongLongValueSelector
  , unsignedLongValueSelector
  , unsignedShortValueSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithCoder:@
initWithCoder :: (IsNSNumber nsNumber, IsNSCoder coder) => nsNumber -> coder -> IO (Id NSNumber)
initWithCoder nsNumber coder =
  sendOwnedMessage nsNumber initWithCoderSelector (toNSCoder coder)

-- | @- initWithChar:@
initWithChar :: IsNSNumber nsNumber => nsNumber -> CChar -> IO (Id NSNumber)
initWithChar nsNumber value =
  sendOwnedMessage nsNumber initWithCharSelector value

-- | @- initWithUnsignedChar:@
initWithUnsignedChar :: IsNSNumber nsNumber => nsNumber -> CUChar -> IO (Id NSNumber)
initWithUnsignedChar nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedCharSelector value

-- | @- initWithShort:@
initWithShort :: IsNSNumber nsNumber => nsNumber -> CShort -> IO (Id NSNumber)
initWithShort nsNumber value =
  sendOwnedMessage nsNumber initWithShortSelector value

-- | @- initWithUnsignedShort:@
initWithUnsignedShort :: IsNSNumber nsNumber => nsNumber -> CUShort -> IO (Id NSNumber)
initWithUnsignedShort nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedShortSelector value

-- | @- initWithInt:@
initWithInt :: IsNSNumber nsNumber => nsNumber -> CInt -> IO (Id NSNumber)
initWithInt nsNumber value =
  sendOwnedMessage nsNumber initWithIntSelector value

-- | @- initWithUnsignedInt:@
initWithUnsignedInt :: IsNSNumber nsNumber => nsNumber -> CUInt -> IO (Id NSNumber)
initWithUnsignedInt nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedIntSelector value

-- | @- initWithLong:@
initWithLong :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithLong nsNumber value =
  sendOwnedMessage nsNumber initWithLongSelector value

-- | @- initWithUnsignedLong:@
initWithUnsignedLong :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedLong nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedLongSelector value

-- | @- initWithLongLong:@
initWithLongLong :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithLongLong nsNumber value =
  sendOwnedMessage nsNumber initWithLongLongSelector value

-- | @- initWithUnsignedLongLong:@
initWithUnsignedLongLong :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedLongLong nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedLongLongSelector value

-- | @- initWithFloat:@
initWithFloat :: IsNSNumber nsNumber => nsNumber -> CFloat -> IO (Id NSNumber)
initWithFloat nsNumber value =
  sendOwnedMessage nsNumber initWithFloatSelector value

-- | @- initWithDouble:@
initWithDouble :: IsNSNumber nsNumber => nsNumber -> CDouble -> IO (Id NSNumber)
initWithDouble nsNumber value =
  sendOwnedMessage nsNumber initWithDoubleSelector value

-- | @- initWithBool:@
initWithBool :: IsNSNumber nsNumber => nsNumber -> Bool -> IO (Id NSNumber)
initWithBool nsNumber value =
  sendOwnedMessage nsNumber initWithBoolSelector value

-- | @- initWithInteger:@
initWithInteger :: IsNSNumber nsNumber => nsNumber -> CLong -> IO (Id NSNumber)
initWithInteger nsNumber value =
  sendOwnedMessage nsNumber initWithIntegerSelector value

-- | @- initWithUnsignedInteger:@
initWithUnsignedInteger :: IsNSNumber nsNumber => nsNumber -> CULong -> IO (Id NSNumber)
initWithUnsignedInteger nsNumber value =
  sendOwnedMessage nsNumber initWithUnsignedIntegerSelector value

-- | @- compare:@
compare_ :: (IsNSNumber nsNumber, IsNSNumber otherNumber) => nsNumber -> otherNumber -> IO NSComparisonResult
compare_ nsNumber otherNumber =
  sendMessage nsNumber compareSelector (toNSNumber otherNumber)

-- | @- isEqualToNumber:@
isEqualToNumber :: (IsNSNumber nsNumber, IsNSNumber number) => nsNumber -> number -> IO Bool
isEqualToNumber nsNumber number =
  sendMessage nsNumber isEqualToNumberSelector (toNSNumber number)

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSNumber nsNumber => nsNumber -> RawId -> IO (Id NSString)
descriptionWithLocale nsNumber locale =
  sendMessage nsNumber descriptionWithLocaleSelector locale

-- | @+ numberWithChar:@
numberWithChar :: CChar -> IO (Id NSNumber)
numberWithChar value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithCharSelector value

-- | @+ numberWithUnsignedChar:@
numberWithUnsignedChar :: CUChar -> IO (Id NSNumber)
numberWithUnsignedChar value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedCharSelector value

-- | @+ numberWithShort:@
numberWithShort :: CShort -> IO (Id NSNumber)
numberWithShort value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithShortSelector value

-- | @+ numberWithUnsignedShort:@
numberWithUnsignedShort :: CUShort -> IO (Id NSNumber)
numberWithUnsignedShort value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedShortSelector value

-- | @+ numberWithInt:@
numberWithInt :: CInt -> IO (Id NSNumber)
numberWithInt value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithIntSelector value

-- | @+ numberWithUnsignedInt:@
numberWithUnsignedInt :: CUInt -> IO (Id NSNumber)
numberWithUnsignedInt value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedIntSelector value

-- | @+ numberWithLong:@
numberWithLong :: CLong -> IO (Id NSNumber)
numberWithLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithLongSelector value

-- | @+ numberWithUnsignedLong:@
numberWithUnsignedLong :: CULong -> IO (Id NSNumber)
numberWithUnsignedLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedLongSelector value

-- | @+ numberWithLongLong:@
numberWithLongLong :: CLong -> IO (Id NSNumber)
numberWithLongLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithLongLongSelector value

-- | @+ numberWithUnsignedLongLong:@
numberWithUnsignedLongLong :: CULong -> IO (Id NSNumber)
numberWithUnsignedLongLong value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedLongLongSelector value

-- | @+ numberWithFloat:@
numberWithFloat :: CFloat -> IO (Id NSNumber)
numberWithFloat value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithFloatSelector value

-- | @+ numberWithDouble:@
numberWithDouble :: CDouble -> IO (Id NSNumber)
numberWithDouble value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithDoubleSelector value

-- | @+ numberWithBool:@
numberWithBool :: Bool -> IO (Id NSNumber)
numberWithBool value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithBoolSelector value

-- | @+ numberWithInteger:@
numberWithInteger :: CLong -> IO (Id NSNumber)
numberWithInteger value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithIntegerSelector value

-- | @+ numberWithUnsignedInteger:@
numberWithUnsignedInteger :: CULong -> IO (Id NSNumber)
numberWithUnsignedInteger value =
  do
    cls' <- getRequiredClass "NSNumber"
    sendClassMessage cls' numberWithUnsignedIntegerSelector value

-- | @- charValue@
charValue :: IsNSNumber nsNumber => nsNumber -> IO CChar
charValue nsNumber =
  sendMessage nsNumber charValueSelector

-- | @- unsignedCharValue@
unsignedCharValue :: IsNSNumber nsNumber => nsNumber -> IO CUChar
unsignedCharValue nsNumber =
  sendMessage nsNumber unsignedCharValueSelector

-- | @- shortValue@
shortValue :: IsNSNumber nsNumber => nsNumber -> IO CShort
shortValue nsNumber =
  sendMessage nsNumber shortValueSelector

-- | @- unsignedShortValue@
unsignedShortValue :: IsNSNumber nsNumber => nsNumber -> IO CUShort
unsignedShortValue nsNumber =
  sendMessage nsNumber unsignedShortValueSelector

-- | @- intValue@
intValue :: IsNSNumber nsNumber => nsNumber -> IO CInt
intValue nsNumber =
  sendMessage nsNumber intValueSelector

-- | @- unsignedIntValue@
unsignedIntValue :: IsNSNumber nsNumber => nsNumber -> IO CUInt
unsignedIntValue nsNumber =
  sendMessage nsNumber unsignedIntValueSelector

-- | @- longValue@
longValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
longValue nsNumber =
  sendMessage nsNumber longValueSelector

-- | @- unsignedLongValue@
unsignedLongValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedLongValue nsNumber =
  sendMessage nsNumber unsignedLongValueSelector

-- | @- longLongValue@
longLongValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
longLongValue nsNumber =
  sendMessage nsNumber longLongValueSelector

-- | @- unsignedLongLongValue@
unsignedLongLongValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedLongLongValue nsNumber =
  sendMessage nsNumber unsignedLongLongValueSelector

-- | @- floatValue@
floatValue :: IsNSNumber nsNumber => nsNumber -> IO CFloat
floatValue nsNumber =
  sendMessage nsNumber floatValueSelector

-- | @- doubleValue@
doubleValue :: IsNSNumber nsNumber => nsNumber -> IO CDouble
doubleValue nsNumber =
  sendMessage nsNumber doubleValueSelector

-- | @- boolValue@
boolValue :: IsNSNumber nsNumber => nsNumber -> IO Bool
boolValue nsNumber =
  sendMessage nsNumber boolValueSelector

-- | @- integerValue@
integerValue :: IsNSNumber nsNumber => nsNumber -> IO CLong
integerValue nsNumber =
  sendMessage nsNumber integerValueSelector

-- | @- unsignedIntegerValue@
unsignedIntegerValue :: IsNSNumber nsNumber => nsNumber -> IO CULong
unsignedIntegerValue nsNumber =
  sendMessage nsNumber unsignedIntegerValueSelector

-- | @- stringValue@
stringValue :: IsNSNumber nsNumber => nsNumber -> IO (Id NSString)
stringValue nsNumber =
  sendMessage nsNumber stringValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSNumber)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithChar:@
initWithCharSelector :: Selector '[CChar] (Id NSNumber)
initWithCharSelector = mkSelector "initWithChar:"

-- | @Selector@ for @initWithUnsignedChar:@
initWithUnsignedCharSelector :: Selector '[CUChar] (Id NSNumber)
initWithUnsignedCharSelector = mkSelector "initWithUnsignedChar:"

-- | @Selector@ for @initWithShort:@
initWithShortSelector :: Selector '[CShort] (Id NSNumber)
initWithShortSelector = mkSelector "initWithShort:"

-- | @Selector@ for @initWithUnsignedShort:@
initWithUnsignedShortSelector :: Selector '[CUShort] (Id NSNumber)
initWithUnsignedShortSelector = mkSelector "initWithUnsignedShort:"

-- | @Selector@ for @initWithInt:@
initWithIntSelector :: Selector '[CInt] (Id NSNumber)
initWithIntSelector = mkSelector "initWithInt:"

-- | @Selector@ for @initWithUnsignedInt:@
initWithUnsignedIntSelector :: Selector '[CUInt] (Id NSNumber)
initWithUnsignedIntSelector = mkSelector "initWithUnsignedInt:"

-- | @Selector@ for @initWithLong:@
initWithLongSelector :: Selector '[CLong] (Id NSNumber)
initWithLongSelector = mkSelector "initWithLong:"

-- | @Selector@ for @initWithUnsignedLong:@
initWithUnsignedLongSelector :: Selector '[CULong] (Id NSNumber)
initWithUnsignedLongSelector = mkSelector "initWithUnsignedLong:"

-- | @Selector@ for @initWithLongLong:@
initWithLongLongSelector :: Selector '[CLong] (Id NSNumber)
initWithLongLongSelector = mkSelector "initWithLongLong:"

-- | @Selector@ for @initWithUnsignedLongLong:@
initWithUnsignedLongLongSelector :: Selector '[CULong] (Id NSNumber)
initWithUnsignedLongLongSelector = mkSelector "initWithUnsignedLongLong:"

-- | @Selector@ for @initWithFloat:@
initWithFloatSelector :: Selector '[CFloat] (Id NSNumber)
initWithFloatSelector = mkSelector "initWithFloat:"

-- | @Selector@ for @initWithDouble:@
initWithDoubleSelector :: Selector '[CDouble] (Id NSNumber)
initWithDoubleSelector = mkSelector "initWithDouble:"

-- | @Selector@ for @initWithBool:@
initWithBoolSelector :: Selector '[Bool] (Id NSNumber)
initWithBoolSelector = mkSelector "initWithBool:"

-- | @Selector@ for @initWithInteger:@
initWithIntegerSelector :: Selector '[CLong] (Id NSNumber)
initWithIntegerSelector = mkSelector "initWithInteger:"

-- | @Selector@ for @initWithUnsignedInteger:@
initWithUnsignedIntegerSelector :: Selector '[CULong] (Id NSNumber)
initWithUnsignedIntegerSelector = mkSelector "initWithUnsignedInteger:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSNumber] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToNumber:@
isEqualToNumberSelector :: Selector '[Id NSNumber] Bool
isEqualToNumberSelector = mkSelector "isEqualToNumber:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @numberWithChar:@
numberWithCharSelector :: Selector '[CChar] (Id NSNumber)
numberWithCharSelector = mkSelector "numberWithChar:"

-- | @Selector@ for @numberWithUnsignedChar:@
numberWithUnsignedCharSelector :: Selector '[CUChar] (Id NSNumber)
numberWithUnsignedCharSelector = mkSelector "numberWithUnsignedChar:"

-- | @Selector@ for @numberWithShort:@
numberWithShortSelector :: Selector '[CShort] (Id NSNumber)
numberWithShortSelector = mkSelector "numberWithShort:"

-- | @Selector@ for @numberWithUnsignedShort:@
numberWithUnsignedShortSelector :: Selector '[CUShort] (Id NSNumber)
numberWithUnsignedShortSelector = mkSelector "numberWithUnsignedShort:"

-- | @Selector@ for @numberWithInt:@
numberWithIntSelector :: Selector '[CInt] (Id NSNumber)
numberWithIntSelector = mkSelector "numberWithInt:"

-- | @Selector@ for @numberWithUnsignedInt:@
numberWithUnsignedIntSelector :: Selector '[CUInt] (Id NSNumber)
numberWithUnsignedIntSelector = mkSelector "numberWithUnsignedInt:"

-- | @Selector@ for @numberWithLong:@
numberWithLongSelector :: Selector '[CLong] (Id NSNumber)
numberWithLongSelector = mkSelector "numberWithLong:"

-- | @Selector@ for @numberWithUnsignedLong:@
numberWithUnsignedLongSelector :: Selector '[CULong] (Id NSNumber)
numberWithUnsignedLongSelector = mkSelector "numberWithUnsignedLong:"

-- | @Selector@ for @numberWithLongLong:@
numberWithLongLongSelector :: Selector '[CLong] (Id NSNumber)
numberWithLongLongSelector = mkSelector "numberWithLongLong:"

-- | @Selector@ for @numberWithUnsignedLongLong:@
numberWithUnsignedLongLongSelector :: Selector '[CULong] (Id NSNumber)
numberWithUnsignedLongLongSelector = mkSelector "numberWithUnsignedLongLong:"

-- | @Selector@ for @numberWithFloat:@
numberWithFloatSelector :: Selector '[CFloat] (Id NSNumber)
numberWithFloatSelector = mkSelector "numberWithFloat:"

-- | @Selector@ for @numberWithDouble:@
numberWithDoubleSelector :: Selector '[CDouble] (Id NSNumber)
numberWithDoubleSelector = mkSelector "numberWithDouble:"

-- | @Selector@ for @numberWithBool:@
numberWithBoolSelector :: Selector '[Bool] (Id NSNumber)
numberWithBoolSelector = mkSelector "numberWithBool:"

-- | @Selector@ for @numberWithInteger:@
numberWithIntegerSelector :: Selector '[CLong] (Id NSNumber)
numberWithIntegerSelector = mkSelector "numberWithInteger:"

-- | @Selector@ for @numberWithUnsignedInteger:@
numberWithUnsignedIntegerSelector :: Selector '[CULong] (Id NSNumber)
numberWithUnsignedIntegerSelector = mkSelector "numberWithUnsignedInteger:"

-- | @Selector@ for @charValue@
charValueSelector :: Selector '[] CChar
charValueSelector = mkSelector "charValue"

-- | @Selector@ for @unsignedCharValue@
unsignedCharValueSelector :: Selector '[] CUChar
unsignedCharValueSelector = mkSelector "unsignedCharValue"

-- | @Selector@ for @shortValue@
shortValueSelector :: Selector '[] CShort
shortValueSelector = mkSelector "shortValue"

-- | @Selector@ for @unsignedShortValue@
unsignedShortValueSelector :: Selector '[] CUShort
unsignedShortValueSelector = mkSelector "unsignedShortValue"

-- | @Selector@ for @intValue@
intValueSelector :: Selector '[] CInt
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @unsignedIntValue@
unsignedIntValueSelector :: Selector '[] CUInt
unsignedIntValueSelector = mkSelector "unsignedIntValue"

-- | @Selector@ for @longValue@
longValueSelector :: Selector '[] CLong
longValueSelector = mkSelector "longValue"

-- | @Selector@ for @unsignedLongValue@
unsignedLongValueSelector :: Selector '[] CULong
unsignedLongValueSelector = mkSelector "unsignedLongValue"

-- | @Selector@ for @longLongValue@
longLongValueSelector :: Selector '[] CLong
longLongValueSelector = mkSelector "longLongValue"

-- | @Selector@ for @unsignedLongLongValue@
unsignedLongLongValueSelector :: Selector '[] CULong
unsignedLongLongValueSelector = mkSelector "unsignedLongLongValue"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @boolValue@
boolValueSelector :: Selector '[] Bool
boolValueSelector = mkSelector "boolValue"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector '[] CLong
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @unsignedIntegerValue@
unsignedIntegerValueSelector :: Selector '[] CULong
unsignedIntegerValueSelector = mkSelector "unsignedIntegerValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

