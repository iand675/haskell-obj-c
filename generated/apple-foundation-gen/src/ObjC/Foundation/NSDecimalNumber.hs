{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMantissa_exponent_isNegativeSelector
  , initWithStringSelector
  , initWithString_localeSelector
  , descriptionWithLocaleSelector
  , decimalNumberWithMantissa_exponent_isNegativeSelector
  , decimalNumberWithStringSelector
  , decimalNumberWithString_localeSelector
  , decimalNumberByAddingSelector
  , decimalNumberByAdding_withBehaviorSelector
  , decimalNumberBySubtractingSelector
  , decimalNumberBySubtracting_withBehaviorSelector
  , decimalNumberByMultiplyingBySelector
  , decimalNumberByMultiplyingBy_withBehaviorSelector
  , decimalNumberByDividingBySelector
  , decimalNumberByDividingBy_withBehaviorSelector
  , decimalNumberByRaisingToPowerSelector
  , decimalNumberByRaisingToPower_withBehaviorSelector
  , decimalNumberByMultiplyingByPowerOf10Selector
  , decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector
  , decimalNumberByRoundingAccordingToBehaviorSelector
  , compareSelector
  , zeroSelector
  , oneSelector
  , minimumDecimalNumberSelector
  , maximumDecimalNumberSelector
  , notANumberSelector
  , defaultBehaviorSelector
  , setDefaultBehaviorSelector
  , objCTypeSelector
  , doubleValueSelector

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

-- | @- initWithMantissa:exponent:isNegative:@
initWithMantissa_exponent_isNegative :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> CShort -> Bool -> IO (Id NSDecimalNumber)
initWithMantissa_exponent_isNegative nsDecimalNumber  mantissa exponent flag =
    sendMsg nsDecimalNumber (mkSelector "initWithMantissa:exponent:isNegative:") (retPtr retVoid) [argCULong mantissa, argCInt (fromIntegral exponent), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSDecimalNumber nsDecimalNumber, IsNSString numberValue) => nsDecimalNumber -> numberValue -> IO (Id NSDecimalNumber)
initWithString nsDecimalNumber  numberValue =
  withObjCPtr numberValue $ \raw_numberValue ->
      sendMsg nsDecimalNumber (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_numberValue :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithString:locale:@
initWithString_locale :: (IsNSDecimalNumber nsDecimalNumber, IsNSString numberValue) => nsDecimalNumber -> numberValue -> RawId -> IO (Id NSDecimalNumber)
initWithString_locale nsDecimalNumber  numberValue locale =
  withObjCPtr numberValue $ \raw_numberValue ->
      sendMsg nsDecimalNumber (mkSelector "initWithString:locale:") (retPtr retVoid) [argPtr (castPtr raw_numberValue :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())] >>= ownedObject . castPtr

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> RawId -> IO (Id NSString)
descriptionWithLocale nsDecimalNumber  locale =
    sendMsg nsDecimalNumber (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ decimalNumberWithMantissa:exponent:isNegative:@
decimalNumberWithMantissa_exponent_isNegative :: CULong -> CShort -> Bool -> IO (Id NSDecimalNumber)
decimalNumberWithMantissa_exponent_isNegative mantissa exponent flag =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "decimalNumberWithMantissa:exponent:isNegative:") (retPtr retVoid) [argCULong mantissa, argCInt (fromIntegral exponent), argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @+ decimalNumberWithString:@
decimalNumberWithString :: IsNSString numberValue => numberValue -> IO (Id NSDecimalNumber)
decimalNumberWithString numberValue =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    withObjCPtr numberValue $ \raw_numberValue ->
      sendClassMsg cls' (mkSelector "decimalNumberWithString:") (retPtr retVoid) [argPtr (castPtr raw_numberValue :: Ptr ())] >>= retainedObject . castPtr

-- | @+ decimalNumberWithString:locale:@
decimalNumberWithString_locale :: IsNSString numberValue => numberValue -> RawId -> IO (Id NSDecimalNumber)
decimalNumberWithString_locale numberValue locale =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    withObjCPtr numberValue $ \raw_numberValue ->
      sendClassMsg cls' (mkSelector "decimalNumberWithString:locale:") (retPtr retVoid) [argPtr (castPtr raw_numberValue :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByAdding:@
decimalNumberByAdding :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByAdding nsDecimalNumber  decimalNumber =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByAdding:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByAdding:withBehavior:@
decimalNumberByAdding_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByAdding_withBehavior nsDecimalNumber  decimalNumber behavior =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByAdding:withBehavior:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ()), argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberBySubtracting:@
decimalNumberBySubtracting :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberBySubtracting nsDecimalNumber  decimalNumber =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberBySubtracting:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberBySubtracting:withBehavior:@
decimalNumberBySubtracting_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberBySubtracting_withBehavior nsDecimalNumber  decimalNumber behavior =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberBySubtracting:withBehavior:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ()), argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByMultiplyingBy:@
decimalNumberByMultiplyingBy :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingBy nsDecimalNumber  decimalNumber =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByMultiplyingBy:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByMultiplyingBy:withBehavior:@
decimalNumberByMultiplyingBy_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingBy_withBehavior nsDecimalNumber  decimalNumber behavior =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByMultiplyingBy:withBehavior:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ()), argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByDividingBy:@
decimalNumberByDividingBy :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO (Id NSDecimalNumber)
decimalNumberByDividingBy nsDecimalNumber  decimalNumber =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByDividingBy:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByDividingBy:withBehavior:@
decimalNumberByDividingBy_withBehavior :: (IsNSDecimalNumber nsDecimalNumber, IsNSDecimalNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByDividingBy_withBehavior nsDecimalNumber  decimalNumber behavior =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      sendMsg nsDecimalNumber (mkSelector "decimalNumberByDividingBy:withBehavior:") (retPtr retVoid) [argPtr (castPtr raw_decimalNumber :: Ptr ()), argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByRaisingToPower:@
decimalNumberByRaisingToPower :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> IO (Id NSDecimalNumber)
decimalNumberByRaisingToPower nsDecimalNumber  power =
    sendMsg nsDecimalNumber (mkSelector "decimalNumberByRaisingToPower:") (retPtr retVoid) [argCULong power] >>= retainedObject . castPtr

-- | @- decimalNumberByRaisingToPower:withBehavior:@
decimalNumberByRaisingToPower_withBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CULong -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByRaisingToPower_withBehavior nsDecimalNumber  power behavior =
    sendMsg nsDecimalNumber (mkSelector "decimalNumberByRaisingToPower:withBehavior:") (retPtr retVoid) [argCULong power, argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByMultiplyingByPowerOf10:@
decimalNumberByMultiplyingByPowerOf10 :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CShort -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10 nsDecimalNumber  power =
    sendMsg nsDecimalNumber (mkSelector "decimalNumberByMultiplyingByPowerOf10:") (retPtr retVoid) [argCInt (fromIntegral power)] >>= retainedObject . castPtr

-- | @- decimalNumberByMultiplyingByPowerOf10:withBehavior:@
decimalNumberByMultiplyingByPowerOf10_withBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> CShort -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByMultiplyingByPowerOf10_withBehavior nsDecimalNumber  power behavior =
    sendMsg nsDecimalNumber (mkSelector "decimalNumberByMultiplyingByPowerOf10:withBehavior:") (retPtr retVoid) [argCInt (fromIntegral power), argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- decimalNumberByRoundingAccordingToBehavior:@
decimalNumberByRoundingAccordingToBehavior :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> RawId -> IO (Id NSDecimalNumber)
decimalNumberByRoundingAccordingToBehavior nsDecimalNumber  behavior =
    sendMsg nsDecimalNumber (mkSelector "decimalNumberByRoundingAccordingToBehavior:") (retPtr retVoid) [argPtr (castPtr (unRawId behavior) :: Ptr ())] >>= retainedObject . castPtr

-- | @- compare:@
compare_ :: (IsNSDecimalNumber nsDecimalNumber, IsNSNumber decimalNumber) => nsDecimalNumber -> decimalNumber -> IO NSComparisonResult
compare_ nsDecimalNumber  decimalNumber =
  withObjCPtr decimalNumber $ \raw_decimalNumber ->
      fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsDecimalNumber (mkSelector "compare:") retCLong [argPtr (castPtr raw_decimalNumber :: Ptr ())]

-- | @+ zero@
zero :: IO (Id NSDecimalNumber)
zero  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "zero") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ one@
one :: IO (Id NSDecimalNumber)
one  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "one") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ minimumDecimalNumber@
minimumDecimalNumber :: IO (Id NSDecimalNumber)
minimumDecimalNumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "minimumDecimalNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ maximumDecimalNumber@
maximumDecimalNumber :: IO (Id NSDecimalNumber)
maximumDecimalNumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "maximumDecimalNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ notANumber@
notANumber :: IO (Id NSDecimalNumber)
notANumber  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "notANumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultBehavior@
defaultBehavior :: IO RawId
defaultBehavior  =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultBehavior") (retPtr retVoid) []

-- | @+ setDefaultBehavior:@
setDefaultBehavior :: RawId -> IO ()
setDefaultBehavior value =
  do
    cls' <- getRequiredClass "NSDecimalNumber"
    sendClassMsg cls' (mkSelector "setDefaultBehavior:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- objCType@
objCType :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> IO (Ptr CChar)
objCType nsDecimalNumber  =
    fmap castPtr $ sendMsg nsDecimalNumber (mkSelector "objCType") (retPtr retVoid) []

-- | @- doubleValue@
doubleValue :: IsNSDecimalNumber nsDecimalNumber => nsDecimalNumber -> IO CDouble
doubleValue nsDecimalNumber  =
    sendMsg nsDecimalNumber (mkSelector "doubleValue") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMantissa:exponent:isNegative:@
initWithMantissa_exponent_isNegativeSelector :: Selector
initWithMantissa_exponent_isNegativeSelector = mkSelector "initWithMantissa:exponent:isNegative:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:locale:@
initWithString_localeSelector :: Selector
initWithString_localeSelector = mkSelector "initWithString:locale:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @decimalNumberWithMantissa:exponent:isNegative:@
decimalNumberWithMantissa_exponent_isNegativeSelector :: Selector
decimalNumberWithMantissa_exponent_isNegativeSelector = mkSelector "decimalNumberWithMantissa:exponent:isNegative:"

-- | @Selector@ for @decimalNumberWithString:@
decimalNumberWithStringSelector :: Selector
decimalNumberWithStringSelector = mkSelector "decimalNumberWithString:"

-- | @Selector@ for @decimalNumberWithString:locale:@
decimalNumberWithString_localeSelector :: Selector
decimalNumberWithString_localeSelector = mkSelector "decimalNumberWithString:locale:"

-- | @Selector@ for @decimalNumberByAdding:@
decimalNumberByAddingSelector :: Selector
decimalNumberByAddingSelector = mkSelector "decimalNumberByAdding:"

-- | @Selector@ for @decimalNumberByAdding:withBehavior:@
decimalNumberByAdding_withBehaviorSelector :: Selector
decimalNumberByAdding_withBehaviorSelector = mkSelector "decimalNumberByAdding:withBehavior:"

-- | @Selector@ for @decimalNumberBySubtracting:@
decimalNumberBySubtractingSelector :: Selector
decimalNumberBySubtractingSelector = mkSelector "decimalNumberBySubtracting:"

-- | @Selector@ for @decimalNumberBySubtracting:withBehavior:@
decimalNumberBySubtracting_withBehaviorSelector :: Selector
decimalNumberBySubtracting_withBehaviorSelector = mkSelector "decimalNumberBySubtracting:withBehavior:"

-- | @Selector@ for @decimalNumberByMultiplyingBy:@
decimalNumberByMultiplyingBySelector :: Selector
decimalNumberByMultiplyingBySelector = mkSelector "decimalNumberByMultiplyingBy:"

-- | @Selector@ for @decimalNumberByMultiplyingBy:withBehavior:@
decimalNumberByMultiplyingBy_withBehaviorSelector :: Selector
decimalNumberByMultiplyingBy_withBehaviorSelector = mkSelector "decimalNumberByMultiplyingBy:withBehavior:"

-- | @Selector@ for @decimalNumberByDividingBy:@
decimalNumberByDividingBySelector :: Selector
decimalNumberByDividingBySelector = mkSelector "decimalNumberByDividingBy:"

-- | @Selector@ for @decimalNumberByDividingBy:withBehavior:@
decimalNumberByDividingBy_withBehaviorSelector :: Selector
decimalNumberByDividingBy_withBehaviorSelector = mkSelector "decimalNumberByDividingBy:withBehavior:"

-- | @Selector@ for @decimalNumberByRaisingToPower:@
decimalNumberByRaisingToPowerSelector :: Selector
decimalNumberByRaisingToPowerSelector = mkSelector "decimalNumberByRaisingToPower:"

-- | @Selector@ for @decimalNumberByRaisingToPower:withBehavior:@
decimalNumberByRaisingToPower_withBehaviorSelector :: Selector
decimalNumberByRaisingToPower_withBehaviorSelector = mkSelector "decimalNumberByRaisingToPower:withBehavior:"

-- | @Selector@ for @decimalNumberByMultiplyingByPowerOf10:@
decimalNumberByMultiplyingByPowerOf10Selector :: Selector
decimalNumberByMultiplyingByPowerOf10Selector = mkSelector "decimalNumberByMultiplyingByPowerOf10:"

-- | @Selector@ for @decimalNumberByMultiplyingByPowerOf10:withBehavior:@
decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector :: Selector
decimalNumberByMultiplyingByPowerOf10_withBehaviorSelector = mkSelector "decimalNumberByMultiplyingByPowerOf10:withBehavior:"

-- | @Selector@ for @decimalNumberByRoundingAccordingToBehavior:@
decimalNumberByRoundingAccordingToBehaviorSelector :: Selector
decimalNumberByRoundingAccordingToBehaviorSelector = mkSelector "decimalNumberByRoundingAccordingToBehavior:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @zero@
zeroSelector :: Selector
zeroSelector = mkSelector "zero"

-- | @Selector@ for @one@
oneSelector :: Selector
oneSelector = mkSelector "one"

-- | @Selector@ for @minimumDecimalNumber@
minimumDecimalNumberSelector :: Selector
minimumDecimalNumberSelector = mkSelector "minimumDecimalNumber"

-- | @Selector@ for @maximumDecimalNumber@
maximumDecimalNumberSelector :: Selector
maximumDecimalNumberSelector = mkSelector "maximumDecimalNumber"

-- | @Selector@ for @notANumber@
notANumberSelector :: Selector
notANumberSelector = mkSelector "notANumber"

-- | @Selector@ for @defaultBehavior@
defaultBehaviorSelector :: Selector
defaultBehaviorSelector = mkSelector "defaultBehavior"

-- | @Selector@ for @setDefaultBehavior:@
setDefaultBehaviorSelector :: Selector
setDefaultBehaviorSelector = mkSelector "setDefaultBehavior:"

-- | @Selector@ for @objCType@
objCTypeSelector :: Selector
objCTypeSelector = mkSelector "objCType"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

