{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************	NSDecimalNumber: the class		**********
--
-- Generated bindings for @NSDecimalNumber@.
module ObjC.Foundation.NSDecimalNumber
  ( NSDecimalNumber
  , IsNSDecimalNumber(..)
  , initWithMantissa_exponent_isNegative
  , initWithString
  , initWithString_locale
  , descriptionWithLocale
  , decimalNumberWithMantissa_exponent_isNegative
  , decimalNumberWithString
  , decimalNumberWithString_locale
  , decimalNumberByAdding
  , decimalNumberByAdding_withBehavior
  , decimalNumberBySubtracting
  , decimalNumberBySubtracting_withBehavior
  , decimalNumberByMultiplyingBy
  , decimalNumberByMultiplyingBy_withBehavior
  , decimalNumberByDividingBy
  , decimalNumberByDividingBy_withBehavior
  , decimalNumberByRaisingToPower
  , decimalNumberByRaisingToPower_withBehavior
  , decimalNumberByMultiplyingByPowerOf10
  , decimalNumberByMultiplyingByPowerOf10_withBehavior
  , decimalNumberByRoundingAccordingToBehavior
  , compare_
  , zero
  , one
  , minimumDecimalNumber
  , maximumDecimalNumber
  , notANumber
  , defaultBehavior
  , setDefaultBehavior
  , objCType
  , doubleValue
  , compareSelector
  , decimalNumberByAddingSelector
  , decimalNumberByAdding_withBehaviorSelector
  , decimalNumberByDividingBySelector
  , decimalNumberByDividingBy_withBehaviorSelector
  , decimalNumberByMultiplyingByPowerOf10Selector
  , decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector
  , decimalNumberByMultiplyingBySelector
  , decimalNumberByMultiplyingBy_withBehaviorSelector
  , decimalNumberByRaisingToPowerSelector
  , decimalNumberByRaisingToPower_withBehaviorSelector
  , decimalNumberByRoundingAccordingToBehaviorSelector
  , decimalNumberBySubtractingSelector
  , decimalNumberBySubtracting_withBehaviorSelector
  , decimalNumberWithMantissa_exponent_isNegativeSelector
  , decimalNumberWithStringSelector
  , decimalNumberWithString_localeSelector
  , defaultBehaviorSelector
  , descriptionWithLocaleSelector
  , doubleValueSelector
  , initWithMantissa_exponent_isNegativeSelector
  , initWithStringSelector
  , initWithString_localeSelector
  , maximumDecimalNumberSelector
  , minimumDecimalNumberSelector
  , notANumberSelector
  , objCTypeSelector
  , oneSelector
  , setDefaultBehaviorSelector
  , zeroSelector

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

-- | @- initWithMantissa:exponent:isNegative:@
initWithMantissa_exponent_isNegative :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> CShort -> Bool -> IO (Id NSDecimalNumber)
initWithMantissa_exponent_isNegative nsDecimalNumber mantissa exponent flag =
  sendOwnedMessage nsDecimalNumber initWithMantissa_exponent_isNegativeSelector mantissa exponent flag

-- | @- initWithString:@
initWithString :: (IsNSDecimalNumber nsDecimalNumber, IsNSString numberValue) => nsDecimalNumber -> numberValue -> IO (Id NSDecimalNumber)
initWithString nsDecimalNumber numberValue =
  sendOwnedMessage nsDecimalNumber initWithStringSelector (toNSString numberValue)

-- | @- initWithString:locale:@
initWithString_locale :: (IsNSDecimalNumber nsDecimalNumber, IsNSString numberValue) => nsDecimalNumber -> numberValue -> RawId -> IO (Id NSDecimalNumber)
initWithString_locale nsDecimalNumber numberValue locale =
  sendOwnedMessage nsDecimalNumber initWithString_localeSelector (toNSString numberValue) locale

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> RawId -> IO (Id NSString)
descriptionWithLocale nsDecimalNumber locale =
  sendMessage nsDecimalNumber descriptionWithLocaleSelector locale

-- | @+ decimalNumberWithMantissa:exponent:isNegative:@
decimalNumberWithMantissa_exponent_isNegative :: CULong -> CShort -> Bool -> IO (Id NSDecimalNumber)
decimalNumberWithMantissa_exponent_isNegative mantissa exponent flag =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' decimalNumberWithMantissa_exponent_isNegativeSelector mantissa exponent flag

-- | @+ decimalNumberWithString:@
decimalNumberWithString :: IsNSString numberValue => numberValue -> IO (Id NSDecimalNumber)
decimalNumberWithString numberValue =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' decimalNumberWithStringSelector (toNSString numberValue)

-- | @+ decimalNumberWithString:locale:@
decimalNumberWithString_locale :: IsNSString numberValue => numberValue -> RawId -> IO (Id NSDecimalNumber)
decimalNumberWithString_locale numberValue locale =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' decimalNumberWithString_localeSelector (toNSString numberValue) locale

-- | @- decimalNumberByAdding:@
decimalNumberByAdding :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByAdding nsDecimalNumber decimalNumber =
  sendMessage nsDecimalNumber decimalNumberByAddingSelector (toNSDecimalNumber decimalNumber)

-- | @- decimalNumberByAdding:withBehavior:@
decimalNumberByAdding_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByAdding_withBehavior nsDecimalNumber decimalNumber behavior =
  sendMessage nsDecimalNumber decimalNumberByAdding_withBehaviorSelector (toNSDecimalNumber decimalNumber) behavior

-- | @- decimalNumberBySubtracting:@
decimalNumberBySubtracting :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberBySubtracting nsDecimalNumber decimalNumber =
  sendMessage nsDecimalNumber decimalNumberBySubtractingSelector (toNSDecimalNumber decimalNumber)

-- | @- decimalNumberBySubtracting:withBehavior:@
decimalNumberBySubtracting_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberBySubtracting_withBehavior nsDecimalNumber decimalNumber behavior =
  sendMessage nsDecimalNumber decimalNumberBySubtracting_withBehaviorSelector (toNSDecimalNumber decimalNumber) behavior

-- | @- decimalNumberByMultiplyingBy:@
decimalNumberByMultiplyingBy :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingBy nsDecimalNumber decimalNumber =
  sendMessage nsDecimalNumber decimalNumberByMultiplyingBySelector (toNSDecimalNumber decimalNumber)

-- | @- decimalNumberByMultiplyingBy:withBehavior:@
decimalNumberByMultiplyingBy_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingBy_withBehavior nsDecimalNumber decimalNumber behavior =
  sendMessage nsDecimalNumber decimalNumberByMultiplyingBy_withBehaviorSelector (toNSDecimalNumber decimalNumber) behavior

-- | @- decimalNumberByDividingBy:@
decimalNumberByDividingBy :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByDividingBy nsDecimalNumber decimalNumber =
  sendMessage nsDecimalNumber decimalNumberByDividingBySelector (toNSDecimalNumber decimalNumber)

-- | @- decimalNumberByDividingBy:withBehavior:@
decimalNumberByDividingBy_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByDividingBy_withBehavior nsDecimalNumber decimalNumber behavior =
  sendMessage nsDecimalNumber decimalNumberByDividingBy_withBehaviorSelector (toNSDecimalNumber decimalNumber) behavior

-- | @- decimalNumberByRaisingToPower:@
decimalNumberByRaisingToPower :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> IO (Id NSDecimalNumber)
decimalNumberByRaisingToPower nsDecimalNumber power =
  sendMessage nsDecimalNumber decimalNumberByRaisingToPowerSelector power

-- | @- decimalNumberByRaisingToPower:withBehavior:@
decimalNumberByRaisingToPower_withBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByRaisingToPower_withBehavior nsDecimalNumber power behavior =
  sendMessage nsDecimalNumber decimalNumberByRaisingToPower_withBehaviorSelector power behavior

-- | @- decimalNumberByMultiplyingByPowerOf10:@
decimalNumberByMultiplyingByPowerOf10 :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CShort -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10 nsDecimalNumber power =
  sendMessage nsDecimalNumber decimalNumberByMultiplyingByPowerOf10Selector power

-- | @- decimalNumberByMultiplyingByPowerOf10:withBehavior:@
decimalNumberByMultiplyingByPowerOf10_withBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CShort -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10_withBehavior nsDecimalNumber power behavior =
  sendMessage nsDecimalNumber decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector power behavior

-- | @- decimalNumberByRoundingAccordingToBehavior:@
decimalNumberByRoundingAccordingToBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByRoundingAccordingToBehavior nsDecimalNumber behavior =
  sendMessage nsDecimalNumber decimalNumberByRoundingAccordingToBehaviorSelector behavior

-- | @- compare:@
compare_ :: (IsNSDecimalNumber nsDecimalNumber, IsNSNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO NSComparisonResult
compare_ nsDecimalNumber decimalNumber =
  sendMessage nsDecimalNumber compareSelector (toNSNumber decimalNumber)

-- | @+ zero@
zero :: IO (Id NSDecimalNumber)
zero  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' zeroSelector

-- | @+ one@
one :: IO (Id NSDecimalNumber)
one  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' oneSelector

-- | @+ minimumDecimalNumber@
minimumDecimalNumber :: IO (Id NSDecimalNumber)
minimumDecimalNumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' minimumDecimalNumberSelector

-- | @+ maximumDecimalNumber@
maximumDecimalNumber :: IO (Id NSDecimalNumber)
maximumDecimalNumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' maximumDecimalNumberSelector

-- | @+ notANumber@
notANumber :: IO (Id NSDecimalNumber)
notANumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' notANumberSelector

-- | @+ defaultBehavior@
defaultBehavior :: IO RawId
defaultBehavior  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' defaultBehaviorSelector

-- | @+ setDefaultBehavior:@
setDefaultBehavior :: RawId -> IO ()
setDefaultBehavior value =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMessage cls' setDefaultBehaviorSelector value

-- | @- objCType@
objCType :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> IO (Ptr CChar)
objCType nsDecimalNumber =
  sendMessage nsDecimalNumber objCTypeSelector

-- | @- doubleValue@
doubleValue :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> IO CDouble
doubleValue nsDecimalNumber =
  sendMessage nsDecimalNumber doubleValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMantissa:exponent:isNegative:@
initWithMantissa_exponent_isNegativeSelector :: Selector '[CULong, CShort, Bool] (Id NSDecimalNumber)
initWithMantissa_exponent_isNegativeSelector = mkSelector "initWithMantissa:exponent:isNegative:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSDecimalNumber)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:locale:@
initWithString_localeSelector :: Selector '[Id NSString, RawId] (Id NSDecimalNumber)
initWithString_localeSelector = mkSelector "initWithString:locale:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @decimalNumberWithMantissa:exponent:isNegative:@
decimalNumberWithMantissa_exponent_isNegativeSelector :: Selector '[CULong, CShort, Bool] (Id NSDecimalNumber)
decimalNumberWithMantissa_exponent_isNegativeSelector = mkSelector "decimalNumberWithMantissa:exponent:isNegative:"

-- | @Selector@ for @decimalNumberWithString:@
decimalNumberWithStringSelector :: Selector '[Id NSString] (Id NSDecimalNumber)
decimalNumberWithStringSelector = mkSelector "decimalNumberWithString:"

-- | @Selector@ for @decimalNumberWithString:locale:@
decimalNumberWithString_localeSelector :: Selector '[Id NSString, RawId] (Id NSDecimalNumber)
decimalNumberWithString_localeSelector = mkSelector "decimalNumberWithString:locale:"

-- | @Selector@ for @decimalNumberByAdding:@
decimalNumberByAddingSelector :: Selector '[Id NSDecimalNumber] (Id NSDecimalNumber)
decimalNumberByAddingSelector = mkSelector "decimalNumberByAdding:"

-- | @Selector@ for @decimalNumberByAdding:withBehavior:@
decimalNumberByAdding_withBehaviorSelector :: Selector '[Id NSDecimalNumber, RawId] (Id NSDecimalNumber)
decimalNumberByAdding_withBehaviorSelector = mkSelector "decimalNumberByAdding:withBehavior:"

-- | @Selector@ for @decimalNumberBySubtracting:@
decimalNumberBySubtractingSelector :: Selector '[Id NSDecimalNumber] (Id NSDecimalNumber)
decimalNumberBySubtractingSelector = mkSelector "decimalNumberBySubtracting:"

-- | @Selector@ for @decimalNumberBySubtracting:withBehavior:@
decimalNumberBySubtracting_withBehaviorSelector :: Selector '[Id NSDecimalNumber, RawId] (Id NSDecimalNumber)
decimalNumberBySubtracting_withBehaviorSelector = mkSelector "decimalNumberBySubtracting:withBehavior:"

-- | @Selector@ for @decimalNumberByMultiplyingBy:@
decimalNumberByMultiplyingBySelector :: Selector '[Id NSDecimalNumber] (Id NSDecimalNumber)
decimalNumberByMultiplyingBySelector = mkSelector "decimalNumberByMultiplyingBy:"

-- | @Selector@ for @decimalNumberByMultiplyingBy:withBehavior:@
decimalNumberByMultiplyingBy_withBehaviorSelector :: Selector '[Id NSDecimalNumber, RawId] (Id NSDecimalNumber)
decimalNumberByMultiplyingBy_withBehaviorSelector = mkSelector "decimalNumberByMultiplyingBy:withBehavior:"

-- | @Selector@ for @decimalNumberByDividingBy:@
decimalNumberByDividingBySelector :: Selector '[Id NSDecimalNumber] (Id NSDecimalNumber)
decimalNumberByDividingBySelector = mkSelector "decimalNumberByDividingBy:"

-- | @Selector@ for @decimalNumberByDividingBy:withBehavior:@
decimalNumberByDividingBy_withBehaviorSelector :: Selector '[Id NSDecimalNumber, RawId] (Id NSDecimalNumber)
decimalNumberByDividingBy_withBehaviorSelector = mkSelector "decimalNumberByDividingBy:withBehavior:"

-- | @Selector@ for @decimalNumberByRaisingToPower:@
decimalNumberByRaisingToPowerSelector :: Selector '[CULong] (Id NSDecimalNumber)
decimalNumberByRaisingToPowerSelector = mkSelector "decimalNumberByRaisingToPower:"

-- | @Selector@ for @decimalNumberByRaisingToPower:withBehavior:@
decimalNumberByRaisingToPower_withBehaviorSelector :: Selector '[CULong, RawId] (Id NSDecimalNumber)
decimalNumberByRaisingToPower_withBehaviorSelector = mkSelector "decimalNumberByRaisingToPower:withBehavior:"

-- | @Selector@ for @decimalNumberByMultiplyingByPowerOf10:@
decimalNumberByMultiplyingByPowerOf10Selector :: Selector '[CShort] (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10Selector = mkSelector "decimalNumberByMultiplyingByPowerOf10:"

-- | @Selector@ for @decimalNumberByMultiplyingByPowerOf10:withBehavior:@
decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector :: Selector '[CShort, RawId] (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector = mkSelector "decimalNumberByMultiplyingByPowerOf10:withBehavior:"

-- | @Selector@ for @decimalNumberByRoundingAccordingToBehavior:@
decimalNumberByRoundingAccordingToBehaviorSelector :: Selector '[RawId] (Id NSDecimalNumber)
decimalNumberByRoundingAccordingToBehaviorSelector = mkSelector "decimalNumberByRoundingAccordingToBehavior:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSNumber] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @zero@
zeroSelector :: Selector '[] (Id NSDecimalNumber)
zeroSelector = mkSelector "zero"

-- | @Selector@ for @one@
oneSelector :: Selector '[] (Id NSDecimalNumber)
oneSelector = mkSelector "one"

-- | @Selector@ for @minimumDecimalNumber@
minimumDecimalNumberSelector :: Selector '[] (Id NSDecimalNumber)
minimumDecimalNumberSelector = mkSelector "minimumDecimalNumber"

-- | @Selector@ for @maximumDecimalNumber@
maximumDecimalNumberSelector :: Selector '[] (Id NSDecimalNumber)
maximumDecimalNumberSelector = mkSelector "maximumDecimalNumber"

-- | @Selector@ for @notANumber@
notANumberSelector :: Selector '[] (Id NSDecimalNumber)
notANumberSelector = mkSelector "notANumber"

-- | @Selector@ for @defaultBehavior@
defaultBehaviorSelector :: Selector '[] RawId
defaultBehaviorSelector = mkSelector "defaultBehavior"

-- | @Selector@ for @setDefaultBehavior:@
setDefaultBehaviorSelector :: Selector '[RawId] ()
setDefaultBehaviorSelector = mkSelector "setDefaultBehavior:"

-- | @Selector@ for @objCType@
objCTypeSelector :: Selector '[] (Ptr CChar)
objCTypeSelector = mkSelector "objCType"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

