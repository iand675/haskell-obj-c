{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNumberFormatter@.
module ObjC.Foundation.NSNumberFormatter
  ( NSNumberFormatter
  , IsNSNumberFormatter(..)
  , getObjectValue_forString_range_error
  , stringFromNumber
  , numberFromString
  , localizedStringFromNumber_numberStyle
  , defaultFormatterBehavior
  , setDefaultFormatterBehavior
  , formattingContext
  , setFormattingContext
  , minimumGroupingDigits
  , setMinimumGroupingDigits
  , numberStyle
  , setNumberStyle
  , locale
  , setLocale
  , generatesDecimalNumbers
  , setGeneratesDecimalNumbers
  , formatterBehavior
  , setFormatterBehavior
  , negativeFormat
  , setNegativeFormat
  , textAttributesForNegativeValues
  , setTextAttributesForNegativeValues
  , positiveFormat
  , setPositiveFormat
  , textAttributesForPositiveValues
  , setTextAttributesForPositiveValues
  , allowsFloats
  , setAllowsFloats
  , decimalSeparator
  , setDecimalSeparator
  , alwaysShowsDecimalSeparator
  , setAlwaysShowsDecimalSeparator
  , currencyDecimalSeparator
  , setCurrencyDecimalSeparator
  , usesGroupingSeparator
  , setUsesGroupingSeparator
  , groupingSeparator
  , setGroupingSeparator
  , zeroSymbol
  , setZeroSymbol
  , textAttributesForZero
  , setTextAttributesForZero
  , nilSymbol
  , setNilSymbol
  , textAttributesForNil
  , setTextAttributesForNil
  , notANumberSymbol
  , setNotANumberSymbol
  , textAttributesForNotANumber
  , setTextAttributesForNotANumber
  , positiveInfinitySymbol
  , setPositiveInfinitySymbol
  , textAttributesForPositiveInfinity
  , setTextAttributesForPositiveInfinity
  , negativeInfinitySymbol
  , setNegativeInfinitySymbol
  , textAttributesForNegativeInfinity
  , setTextAttributesForNegativeInfinity
  , positivePrefix
  , setPositivePrefix
  , positiveSuffix
  , setPositiveSuffix
  , negativePrefix
  , setNegativePrefix
  , negativeSuffix
  , setNegativeSuffix
  , currencyCode
  , setCurrencyCode
  , currencySymbol
  , setCurrencySymbol
  , internationalCurrencySymbol
  , setInternationalCurrencySymbol
  , percentSymbol
  , setPercentSymbol
  , perMillSymbol
  , setPerMillSymbol
  , minusSign
  , setMinusSign
  , plusSign
  , setPlusSign
  , exponentSymbol
  , setExponentSymbol
  , groupingSize
  , setGroupingSize
  , secondaryGroupingSize
  , setSecondaryGroupingSize
  , multiplier
  , setMultiplier
  , formatWidth
  , setFormatWidth
  , paddingCharacter
  , setPaddingCharacter
  , paddingPosition
  , setPaddingPosition
  , roundingMode
  , setRoundingMode
  , roundingIncrement
  , setRoundingIncrement
  , minimumIntegerDigits
  , setMinimumIntegerDigits
  , maximumIntegerDigits
  , setMaximumIntegerDigits
  , minimumFractionDigits
  , setMinimumFractionDigits
  , maximumFractionDigits
  , setMaximumFractionDigits
  , minimum_
  , setMinimum
  , maximum_
  , setMaximum
  , currencyGroupingSeparator
  , setCurrencyGroupingSeparator
  , lenient
  , setLenient
  , usesSignificantDigits
  , setUsesSignificantDigits
  , minimumSignificantDigits
  , setMinimumSignificantDigits
  , maximumSignificantDigits
  , setMaximumSignificantDigits
  , partialStringValidationEnabled
  , setPartialStringValidationEnabled
  , hasThousandSeparators
  , setHasThousandSeparators
  , thousandSeparator
  , setThousandSeparator
  , localizesFormat
  , setLocalizesFormat
  , format
  , setFormat
  , attributedStringForZero
  , setAttributedStringForZero
  , attributedStringForNil
  , setAttributedStringForNil
  , attributedStringForNotANumber
  , setAttributedStringForNotANumber
  , roundingBehavior
  , setRoundingBehavior
  , getObjectValue_forString_range_errorSelector
  , stringFromNumberSelector
  , numberFromStringSelector
  , localizedStringFromNumber_numberStyleSelector
  , defaultFormatterBehaviorSelector
  , setDefaultFormatterBehaviorSelector
  , formattingContextSelector
  , setFormattingContextSelector
  , minimumGroupingDigitsSelector
  , setMinimumGroupingDigitsSelector
  , numberStyleSelector
  , setNumberStyleSelector
  , localeSelector
  , setLocaleSelector
  , generatesDecimalNumbersSelector
  , setGeneratesDecimalNumbersSelector
  , formatterBehaviorSelector
  , setFormatterBehaviorSelector
  , negativeFormatSelector
  , setNegativeFormatSelector
  , textAttributesForNegativeValuesSelector
  , setTextAttributesForNegativeValuesSelector
  , positiveFormatSelector
  , setPositiveFormatSelector
  , textAttributesForPositiveValuesSelector
  , setTextAttributesForPositiveValuesSelector
  , allowsFloatsSelector
  , setAllowsFloatsSelector
  , decimalSeparatorSelector
  , setDecimalSeparatorSelector
  , alwaysShowsDecimalSeparatorSelector
  , setAlwaysShowsDecimalSeparatorSelector
  , currencyDecimalSeparatorSelector
  , setCurrencyDecimalSeparatorSelector
  , usesGroupingSeparatorSelector
  , setUsesGroupingSeparatorSelector
  , groupingSeparatorSelector
  , setGroupingSeparatorSelector
  , zeroSymbolSelector
  , setZeroSymbolSelector
  , textAttributesForZeroSelector
  , setTextAttributesForZeroSelector
  , nilSymbolSelector
  , setNilSymbolSelector
  , textAttributesForNilSelector
  , setTextAttributesForNilSelector
  , notANumberSymbolSelector
  , setNotANumberSymbolSelector
  , textAttributesForNotANumberSelector
  , setTextAttributesForNotANumberSelector
  , positiveInfinitySymbolSelector
  , setPositiveInfinitySymbolSelector
  , textAttributesForPositiveInfinitySelector
  , setTextAttributesForPositiveInfinitySelector
  , negativeInfinitySymbolSelector
  , setNegativeInfinitySymbolSelector
  , textAttributesForNegativeInfinitySelector
  , setTextAttributesForNegativeInfinitySelector
  , positivePrefixSelector
  , setPositivePrefixSelector
  , positiveSuffixSelector
  , setPositiveSuffixSelector
  , negativePrefixSelector
  , setNegativePrefixSelector
  , negativeSuffixSelector
  , setNegativeSuffixSelector
  , currencyCodeSelector
  , setCurrencyCodeSelector
  , currencySymbolSelector
  , setCurrencySymbolSelector
  , internationalCurrencySymbolSelector
  , setInternationalCurrencySymbolSelector
  , percentSymbolSelector
  , setPercentSymbolSelector
  , perMillSymbolSelector
  , setPerMillSymbolSelector
  , minusSignSelector
  , setMinusSignSelector
  , plusSignSelector
  , setPlusSignSelector
  , exponentSymbolSelector
  , setExponentSymbolSelector
  , groupingSizeSelector
  , setGroupingSizeSelector
  , secondaryGroupingSizeSelector
  , setSecondaryGroupingSizeSelector
  , multiplierSelector
  , setMultiplierSelector
  , formatWidthSelector
  , setFormatWidthSelector
  , paddingCharacterSelector
  , setPaddingCharacterSelector
  , paddingPositionSelector
  , setPaddingPositionSelector
  , roundingModeSelector
  , setRoundingModeSelector
  , roundingIncrementSelector
  , setRoundingIncrementSelector
  , minimumIntegerDigitsSelector
  , setMinimumIntegerDigitsSelector
  , maximumIntegerDigitsSelector
  , setMaximumIntegerDigitsSelector
  , minimumFractionDigitsSelector
  , setMinimumFractionDigitsSelector
  , maximumFractionDigitsSelector
  , setMaximumFractionDigitsSelector
  , minimumSelector
  , setMinimumSelector
  , maximumSelector
  , setMaximumSelector
  , currencyGroupingSeparatorSelector
  , setCurrencyGroupingSeparatorSelector
  , lenientSelector
  , setLenientSelector
  , usesSignificantDigitsSelector
  , setUsesSignificantDigitsSelector
  , minimumSignificantDigitsSelector
  , setMinimumSignificantDigitsSelector
  , maximumSignificantDigitsSelector
  , setMaximumSignificantDigitsSelector
  , partialStringValidationEnabledSelector
  , setPartialStringValidationEnabledSelector
  , hasThousandSeparatorsSelector
  , setHasThousandSeparatorsSelector
  , thousandSeparatorSelector
  , setThousandSeparatorSelector
  , localizesFormatSelector
  , setLocalizesFormatSelector
  , formatSelector
  , setFormatSelector
  , attributedStringForZeroSelector
  , setAttributedStringForZeroSelector
  , attributedStringForNilSelector
  , setAttributedStringForNilSelector
  , attributedStringForNotANumberSelector
  , setAttributedStringForNotANumberSelector
  , roundingBehaviorSelector
  , setRoundingBehaviorSelector

  -- * Enum types
  , NSFormattingContext(NSFormattingContext)
  , pattern NSFormattingContextUnknown
  , pattern NSFormattingContextDynamic
  , pattern NSFormattingContextStandalone
  , pattern NSFormattingContextListItem
  , pattern NSFormattingContextBeginningOfSentence
  , pattern NSFormattingContextMiddleOfSentence
  , NSNumberFormatterBehavior(NSNumberFormatterBehavior)
  , pattern NSNumberFormatterBehaviorDefault
  , pattern NSNumberFormatterBehavior10_0
  , pattern NSNumberFormatterBehavior10_4
  , NSNumberFormatterPadPosition(NSNumberFormatterPadPosition)
  , pattern NSNumberFormatterPadBeforePrefix
  , pattern NSNumberFormatterPadAfterPrefix
  , pattern NSNumberFormatterPadBeforeSuffix
  , pattern NSNumberFormatterPadAfterSuffix
  , NSNumberFormatterRoundingMode(NSNumberFormatterRoundingMode)
  , pattern NSNumberFormatterRoundCeiling
  , pattern NSNumberFormatterRoundFloor
  , pattern NSNumberFormatterRoundDown
  , pattern NSNumberFormatterRoundUp
  , pattern NSNumberFormatterRoundHalfEven
  , pattern NSNumberFormatterRoundHalfDown
  , pattern NSNumberFormatterRoundHalfUp
  , NSNumberFormatterStyle(NSNumberFormatterStyle)
  , pattern NSNumberFormatterNoStyle
  , pattern NSNumberFormatterDecimalStyle
  , pattern NSNumberFormatterCurrencyStyle
  , pattern NSNumberFormatterPercentStyle
  , pattern NSNumberFormatterScientificStyle
  , pattern NSNumberFormatterSpellOutStyle
  , pattern NSNumberFormatterOrdinalStyle
  , pattern NSNumberFormatterCurrencyISOCodeStyle
  , pattern NSNumberFormatterCurrencyPluralStyle
  , pattern NSNumberFormatterCurrencyAccountingStyle

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

-- | @- getObjectValue:forString:range:error:@
getObjectValue_forString_range_error :: (IsNSNumberFormatter nsNumberFormatter, IsNSString string, IsNSError error_) => nsNumberFormatter -> Ptr RawId -> string -> Ptr NSRange -> error_ -> IO Bool
getObjectValue_forString_range_error nsNumberFormatter  obj_ string rangep error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "getObjectValue:forString:range:error:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr rangep, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- stringFromNumber:@
stringFromNumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber number) => nsNumberFormatter -> number -> IO (Id NSString)
stringFromNumber nsNumberFormatter  number =
withObjCPtr number $ \raw_number ->
    sendMsg nsNumberFormatter (mkSelector "stringFromNumber:") (retPtr retVoid) [argPtr (castPtr raw_number :: Ptr ())] >>= retainedObject . castPtr

-- | @- numberFromString:@
numberFromString :: (IsNSNumberFormatter nsNumberFormatter, IsNSString string) => nsNumberFormatter -> string -> IO (Id NSNumber)
numberFromString nsNumberFormatter  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsNumberFormatter (mkSelector "numberFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localizedStringFromNumber:numberStyle:@
localizedStringFromNumber_numberStyle :: IsNSNumber num => num -> NSNumberFormatterStyle -> IO (Id NSString)
localizedStringFromNumber_numberStyle num nstyle =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    withObjCPtr num $ \raw_num ->
      sendClassMsg cls' (mkSelector "localizedStringFromNumber:numberStyle:") (retPtr retVoid) [argPtr (castPtr raw_num :: Ptr ()), argCULong (coerce nstyle)] >>= retainedObject . castPtr

-- | @+ defaultFormatterBehavior@
defaultFormatterBehavior :: IO NSNumberFormatterBehavior
defaultFormatterBehavior  =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    fmap (coerce :: CULong -> NSNumberFormatterBehavior) $ sendClassMsg cls' (mkSelector "defaultFormatterBehavior") retCULong []

-- | @+ setDefaultFormatterBehavior:@
setDefaultFormatterBehavior :: NSNumberFormatterBehavior -> IO ()
setDefaultFormatterBehavior behavior =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    sendClassMsg cls' (mkSelector "setDefaultFormatterBehavior:") retVoid [argCULong (coerce behavior)]

-- | @- formattingContext@
formattingContext :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSFormattingContext
formattingContext nsNumberFormatter  =
  fmap (coerce :: CLong -> NSFormattingContext) $ sendMsg nsNumberFormatter (mkSelector "formattingContext") retCLong []

-- | @- setFormattingContext:@
setFormattingContext :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setFormattingContext:") retVoid [argCLong (coerce value)]

-- | @- minimumGroupingDigits@
minimumGroupingDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CLong
minimumGroupingDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minimumGroupingDigits") retCLong []

-- | @- setMinimumGroupingDigits:@
setMinimumGroupingDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CLong -> IO ()
setMinimumGroupingDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMinimumGroupingDigits:") retVoid [argCLong (fromIntegral value)]

-- | @- numberStyle@
numberStyle :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterStyle
numberStyle nsNumberFormatter  =
  fmap (coerce :: CULong -> NSNumberFormatterStyle) $ sendMsg nsNumberFormatter (mkSelector "numberStyle") retCULong []

-- | @- setNumberStyle:@
setNumberStyle :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterStyle -> IO ()
setNumberStyle nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setNumberStyle:") retVoid [argCULong (coerce value)]

-- | @- locale@
locale :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSLocale)
locale nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSNumberFormatter nsNumberFormatter, IsNSLocale value) => nsNumberFormatter -> value -> IO ()
setLocale nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- generatesDecimalNumbers@
generatesDecimalNumbers :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
generatesDecimalNumbers nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "generatesDecimalNumbers") retCULong []

-- | @- setGeneratesDecimalNumbers:@
setGeneratesDecimalNumbers :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setGeneratesDecimalNumbers nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setGeneratesDecimalNumbers:") retVoid [argCULong (if value then 1 else 0)]

-- | @- formatterBehavior@
formatterBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterBehavior
formatterBehavior nsNumberFormatter  =
  fmap (coerce :: CULong -> NSNumberFormatterBehavior) $ sendMsg nsNumberFormatter (mkSelector "formatterBehavior") retCULong []

-- | @- setFormatterBehavior:@
setFormatterBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterBehavior -> IO ()
setFormatterBehavior nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setFormatterBehavior:") retVoid [argCULong (coerce value)]

-- | @- negativeFormat@
negativeFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeFormat nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "negativeFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNegativeFormat:@
setNegativeFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeFormat nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNegativeFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForNegativeValues@
textAttributesForNegativeValues :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNegativeValues nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForNegativeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForNegativeValues:@
setTextAttributesForNegativeValues :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNegativeValues nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForNegativeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- positiveFormat@
positiveFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveFormat nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "positiveFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPositiveFormat:@
setPositiveFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveFormat nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPositiveFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForPositiveValues@
textAttributesForPositiveValues :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForPositiveValues nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForPositiveValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForPositiveValues:@
setTextAttributesForPositiveValues :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForPositiveValues nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForPositiveValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsFloats@
allowsFloats :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
allowsFloats nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "allowsFloats") retCULong []

-- | @- setAllowsFloats:@
setAllowsFloats :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setAllowsFloats nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setAllowsFloats:") retVoid [argCULong (if value then 1 else 0)]

-- | @- decimalSeparator@
decimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
decimalSeparator nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "decimalSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDecimalSeparator:@
setDecimalSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setDecimalSeparator nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setDecimalSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alwaysShowsDecimalSeparator@
alwaysShowsDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
alwaysShowsDecimalSeparator nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "alwaysShowsDecimalSeparator") retCULong []

-- | @- setAlwaysShowsDecimalSeparator:@
setAlwaysShowsDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setAlwaysShowsDecimalSeparator nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setAlwaysShowsDecimalSeparator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- currencyDecimalSeparator@
currencyDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyDecimalSeparator nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "currencyDecimalSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencyDecimalSeparator:@
setCurrencyDecimalSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyDecimalSeparator nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setCurrencyDecimalSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesGroupingSeparator@
usesGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
usesGroupingSeparator nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "usesGroupingSeparator") retCULong []

-- | @- setUsesGroupingSeparator:@
setUsesGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setUsesGroupingSeparator nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setUsesGroupingSeparator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- groupingSeparator@
groupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
groupingSeparator nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "groupingSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupingSeparator:@
setGroupingSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setGroupingSeparator nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setGroupingSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zeroSymbol@
zeroSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
zeroSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "zeroSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZeroSymbol:@
setZeroSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setZeroSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setZeroSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForZero@
textAttributesForZero :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForZero nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForZero") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForZero:@
setTextAttributesForZero :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForZero nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForZero:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nilSymbol@
nilSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
nilSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "nilSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNilSymbol:@
setNilSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNilSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNilSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForNil@
textAttributesForNil :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNil nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForNil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForNil:@
setTextAttributesForNil :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNil nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForNil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- notANumberSymbol@
notANumberSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
notANumberSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "notANumberSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNotANumberSymbol:@
setNotANumberSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNotANumberSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNotANumberSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForNotANumber@
textAttributesForNotANumber :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNotANumber nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForNotANumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForNotANumber:@
setTextAttributesForNotANumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNotANumber nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForNotANumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- positiveInfinitySymbol@
positiveInfinitySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveInfinitySymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "positiveInfinitySymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPositiveInfinitySymbol:@
setPositiveInfinitySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveInfinitySymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPositiveInfinitySymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForPositiveInfinity@
textAttributesForPositiveInfinity :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForPositiveInfinity nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForPositiveInfinity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForPositiveInfinity:@
setTextAttributesForPositiveInfinity :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForPositiveInfinity nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForPositiveInfinity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- negativeInfinitySymbol@
negativeInfinitySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeInfinitySymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "negativeInfinitySymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNegativeInfinitySymbol:@
setNegativeInfinitySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeInfinitySymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNegativeInfinitySymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textAttributesForNegativeInfinity@
textAttributesForNegativeInfinity :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNegativeInfinity nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "textAttributesForNegativeInfinity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextAttributesForNegativeInfinity:@
setTextAttributesForNegativeInfinity :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNegativeInfinity nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setTextAttributesForNegativeInfinity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- positivePrefix@
positivePrefix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positivePrefix nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "positivePrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPositivePrefix:@
setPositivePrefix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositivePrefix nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPositivePrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- positiveSuffix@
positiveSuffix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveSuffix nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "positiveSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPositiveSuffix:@
setPositiveSuffix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveSuffix nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPositiveSuffix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- negativePrefix@
negativePrefix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativePrefix nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "negativePrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNegativePrefix:@
setNegativePrefix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativePrefix nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNegativePrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- negativeSuffix@
negativeSuffix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeSuffix nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "negativeSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNegativeSuffix:@
setNegativeSuffix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeSuffix nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setNegativeSuffix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currencyCode@
currencyCode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyCode nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyCode nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setCurrencyCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currencySymbol@
currencySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencySymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "currencySymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencySymbol:@
setCurrencySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencySymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setCurrencySymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- internationalCurrencySymbol@
internationalCurrencySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
internationalCurrencySymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "internationalCurrencySymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInternationalCurrencySymbol:@
setInternationalCurrencySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setInternationalCurrencySymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setInternationalCurrencySymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentSymbol@
percentSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
percentSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "percentSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentSymbol:@
setPercentSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPercentSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPercentSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- perMillSymbol@
perMillSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
perMillSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "perMillSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPerMillSymbol:@
setPerMillSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPerMillSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPerMillSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minusSign@
minusSign :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
minusSign nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minusSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinusSign:@
setMinusSign :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setMinusSign nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setMinusSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- plusSign@
plusSign :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
plusSign nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "plusSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlusSign:@
setPlusSign :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPlusSign nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPlusSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exponentSymbol@
exponentSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
exponentSymbol nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "exponentSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExponentSymbol:@
setExponentSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setExponentSymbol nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setExponentSymbol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupingSize@
groupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
groupingSize nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "groupingSize") retCULong []

-- | @- setGroupingSize:@
setGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setGroupingSize nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setGroupingSize:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryGroupingSize@
secondaryGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
secondaryGroupingSize nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "secondaryGroupingSize") retCULong []

-- | @- setSecondaryGroupingSize:@
setSecondaryGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setSecondaryGroupingSize nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setSecondaryGroupingSize:") retVoid [argCULong (fromIntegral value)]

-- | @- multiplier@
multiplier :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
multiplier nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "multiplier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMultiplier:@
setMultiplier :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMultiplier nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setMultiplier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- formatWidth@
formatWidth :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
formatWidth nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "formatWidth") retCULong []

-- | @- setFormatWidth:@
setFormatWidth :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setFormatWidth nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setFormatWidth:") retVoid [argCULong (fromIntegral value)]

-- | @- paddingCharacter@
paddingCharacter :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
paddingCharacter nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "paddingCharacter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaddingCharacter:@
setPaddingCharacter :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPaddingCharacter nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setPaddingCharacter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paddingPosition@
paddingPosition :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterPadPosition
paddingPosition nsNumberFormatter  =
  fmap (coerce :: CULong -> NSNumberFormatterPadPosition) $ sendMsg nsNumberFormatter (mkSelector "paddingPosition") retCULong []

-- | @- setPaddingPosition:@
setPaddingPosition :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterPadPosition -> IO ()
setPaddingPosition nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setPaddingPosition:") retVoid [argCULong (coerce value)]

-- | @- roundingMode@
roundingMode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterRoundingMode
roundingMode nsNumberFormatter  =
  fmap (coerce :: CULong -> NSNumberFormatterRoundingMode) $ sendMsg nsNumberFormatter (mkSelector "roundingMode") retCULong []

-- | @- setRoundingMode:@
setRoundingMode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterRoundingMode -> IO ()
setRoundingMode nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setRoundingMode:") retVoid [argCULong (coerce value)]

-- | @- roundingIncrement@
roundingIncrement :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
roundingIncrement nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "roundingIncrement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRoundingIncrement:@
setRoundingIncrement :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setRoundingIncrement nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setRoundingIncrement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumIntegerDigits@
minimumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumIntegerDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minimumIntegerDigits") retCULong []

-- | @- setMinimumIntegerDigits:@
setMinimumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumIntegerDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMinimumIntegerDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumIntegerDigits@
maximumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumIntegerDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "maximumIntegerDigits") retCULong []

-- | @- setMaximumIntegerDigits:@
setMaximumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumIntegerDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMaximumIntegerDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumFractionDigits@
minimumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumFractionDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minimumFractionDigits") retCULong []

-- | @- setMinimumFractionDigits:@
setMinimumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumFractionDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMinimumFractionDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumFractionDigits@
maximumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumFractionDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "maximumFractionDigits") retCULong []

-- | @- setMaximumFractionDigits:@
setMaximumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumFractionDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMaximumFractionDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- minimum@
minimum_ :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
minimum_ nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minimum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinimum:@
setMinimum :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMinimum nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setMinimum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximum@
maximum_ :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
maximum_ nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "maximum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximum:@
setMaximum :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMaximum nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setMaximum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currencyGroupingSeparator@
currencyGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyGroupingSeparator nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "currencyGroupingSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencyGroupingSeparator:@
setCurrencyGroupingSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyGroupingSeparator nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setCurrencyGroupingSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lenient@
lenient :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
lenient nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "lenient") retCULong []

-- | @- setLenient:@
setLenient :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setLenient nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setLenient:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesSignificantDigits@
usesSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
usesSignificantDigits nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "usesSignificantDigits") retCULong []

-- | @- setUsesSignificantDigits:@
setUsesSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setUsesSignificantDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setUsesSignificantDigits:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minimumSignificantDigits@
minimumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumSignificantDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "minimumSignificantDigits") retCULong []

-- | @- setMinimumSignificantDigits:@
setMinimumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumSignificantDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMinimumSignificantDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumSignificantDigits@
maximumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumSignificantDigits nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "maximumSignificantDigits") retCULong []

-- | @- setMaximumSignificantDigits:@
setMaximumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumSignificantDigits nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setMaximumSignificantDigits:") retVoid [argCULong (fromIntegral value)]

-- | @- partialStringValidationEnabled@
partialStringValidationEnabled :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
partialStringValidationEnabled nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "partialStringValidationEnabled") retCULong []

-- | @- setPartialStringValidationEnabled:@
setPartialStringValidationEnabled :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setPartialStringValidationEnabled nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setPartialStringValidationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasThousandSeparators@
hasThousandSeparators :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
hasThousandSeparators nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "hasThousandSeparators") retCULong []

-- | @- setHasThousandSeparators:@
setHasThousandSeparators :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setHasThousandSeparators nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setHasThousandSeparators:") retVoid [argCULong (if value then 1 else 0)]

-- | @- thousandSeparator@
thousandSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
thousandSeparator nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "thousandSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThousandSeparator:@
setThousandSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setThousandSeparator nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setThousandSeparator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizesFormat@
localizesFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
localizesFormat nsNumberFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNumberFormatter (mkSelector "localizesFormat") retCULong []

-- | @- setLocalizesFormat:@
setLocalizesFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setLocalizesFormat nsNumberFormatter  value =
  sendMsg nsNumberFormatter (mkSelector "setLocalizesFormat:") retVoid [argCULong (if value then 1 else 0)]

-- | @- format@
format :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
format nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFormat:@
setFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setFormat nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedStringForZero@
attributedStringForZero :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForZero nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "attributedStringForZero") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringForZero:@
setAttributedStringForZero :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForZero nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setAttributedStringForZero:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedStringForNil@
attributedStringForNil :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForNil nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "attributedStringForNil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringForNil:@
setAttributedStringForNil :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForNil nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setAttributedStringForNil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedStringForNotANumber@
attributedStringForNotANumber :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForNotANumber nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "attributedStringForNotANumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringForNotANumber:@
setAttributedStringForNotANumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForNotANumber nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setAttributedStringForNotANumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- roundingBehavior@
roundingBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDecimalNumberHandler)
roundingBehavior nsNumberFormatter  =
  sendMsg nsNumberFormatter (mkSelector "roundingBehavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRoundingBehavior:@
setRoundingBehavior :: (IsNSNumberFormatter nsNumberFormatter, IsNSDecimalNumberHandler value) => nsNumberFormatter -> value -> IO ()
setRoundingBehavior nsNumberFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNumberFormatter (mkSelector "setRoundingBehavior:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getObjectValue:forString:range:error:@
getObjectValue_forString_range_errorSelector :: Selector
getObjectValue_forString_range_errorSelector = mkSelector "getObjectValue:forString:range:error:"

-- | @Selector@ for @stringFromNumber:@
stringFromNumberSelector :: Selector
stringFromNumberSelector = mkSelector "stringFromNumber:"

-- | @Selector@ for @numberFromString:@
numberFromStringSelector :: Selector
numberFromStringSelector = mkSelector "numberFromString:"

-- | @Selector@ for @localizedStringFromNumber:numberStyle:@
localizedStringFromNumber_numberStyleSelector :: Selector
localizedStringFromNumber_numberStyleSelector = mkSelector "localizedStringFromNumber:numberStyle:"

-- | @Selector@ for @defaultFormatterBehavior@
defaultFormatterBehaviorSelector :: Selector
defaultFormatterBehaviorSelector = mkSelector "defaultFormatterBehavior"

-- | @Selector@ for @setDefaultFormatterBehavior:@
setDefaultFormatterBehaviorSelector :: Selector
setDefaultFormatterBehaviorSelector = mkSelector "setDefaultFormatterBehavior:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @minimumGroupingDigits@
minimumGroupingDigitsSelector :: Selector
minimumGroupingDigitsSelector = mkSelector "minimumGroupingDigits"

-- | @Selector@ for @setMinimumGroupingDigits:@
setMinimumGroupingDigitsSelector :: Selector
setMinimumGroupingDigitsSelector = mkSelector "setMinimumGroupingDigits:"

-- | @Selector@ for @numberStyle@
numberStyleSelector :: Selector
numberStyleSelector = mkSelector "numberStyle"

-- | @Selector@ for @setNumberStyle:@
setNumberStyleSelector :: Selector
setNumberStyleSelector = mkSelector "setNumberStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @generatesDecimalNumbers@
generatesDecimalNumbersSelector :: Selector
generatesDecimalNumbersSelector = mkSelector "generatesDecimalNumbers"

-- | @Selector@ for @setGeneratesDecimalNumbers:@
setGeneratesDecimalNumbersSelector :: Selector
setGeneratesDecimalNumbersSelector = mkSelector "setGeneratesDecimalNumbers:"

-- | @Selector@ for @formatterBehavior@
formatterBehaviorSelector :: Selector
formatterBehaviorSelector = mkSelector "formatterBehavior"

-- | @Selector@ for @setFormatterBehavior:@
setFormatterBehaviorSelector :: Selector
setFormatterBehaviorSelector = mkSelector "setFormatterBehavior:"

-- | @Selector@ for @negativeFormat@
negativeFormatSelector :: Selector
negativeFormatSelector = mkSelector "negativeFormat"

-- | @Selector@ for @setNegativeFormat:@
setNegativeFormatSelector :: Selector
setNegativeFormatSelector = mkSelector "setNegativeFormat:"

-- | @Selector@ for @textAttributesForNegativeValues@
textAttributesForNegativeValuesSelector :: Selector
textAttributesForNegativeValuesSelector = mkSelector "textAttributesForNegativeValues"

-- | @Selector@ for @setTextAttributesForNegativeValues:@
setTextAttributesForNegativeValuesSelector :: Selector
setTextAttributesForNegativeValuesSelector = mkSelector "setTextAttributesForNegativeValues:"

-- | @Selector@ for @positiveFormat@
positiveFormatSelector :: Selector
positiveFormatSelector = mkSelector "positiveFormat"

-- | @Selector@ for @setPositiveFormat:@
setPositiveFormatSelector :: Selector
setPositiveFormatSelector = mkSelector "setPositiveFormat:"

-- | @Selector@ for @textAttributesForPositiveValues@
textAttributesForPositiveValuesSelector :: Selector
textAttributesForPositiveValuesSelector = mkSelector "textAttributesForPositiveValues"

-- | @Selector@ for @setTextAttributesForPositiveValues:@
setTextAttributesForPositiveValuesSelector :: Selector
setTextAttributesForPositiveValuesSelector = mkSelector "setTextAttributesForPositiveValues:"

-- | @Selector@ for @allowsFloats@
allowsFloatsSelector :: Selector
allowsFloatsSelector = mkSelector "allowsFloats"

-- | @Selector@ for @setAllowsFloats:@
setAllowsFloatsSelector :: Selector
setAllowsFloatsSelector = mkSelector "setAllowsFloats:"

-- | @Selector@ for @decimalSeparator@
decimalSeparatorSelector :: Selector
decimalSeparatorSelector = mkSelector "decimalSeparator"

-- | @Selector@ for @setDecimalSeparator:@
setDecimalSeparatorSelector :: Selector
setDecimalSeparatorSelector = mkSelector "setDecimalSeparator:"

-- | @Selector@ for @alwaysShowsDecimalSeparator@
alwaysShowsDecimalSeparatorSelector :: Selector
alwaysShowsDecimalSeparatorSelector = mkSelector "alwaysShowsDecimalSeparator"

-- | @Selector@ for @setAlwaysShowsDecimalSeparator:@
setAlwaysShowsDecimalSeparatorSelector :: Selector
setAlwaysShowsDecimalSeparatorSelector = mkSelector "setAlwaysShowsDecimalSeparator:"

-- | @Selector@ for @currencyDecimalSeparator@
currencyDecimalSeparatorSelector :: Selector
currencyDecimalSeparatorSelector = mkSelector "currencyDecimalSeparator"

-- | @Selector@ for @setCurrencyDecimalSeparator:@
setCurrencyDecimalSeparatorSelector :: Selector
setCurrencyDecimalSeparatorSelector = mkSelector "setCurrencyDecimalSeparator:"

-- | @Selector@ for @usesGroupingSeparator@
usesGroupingSeparatorSelector :: Selector
usesGroupingSeparatorSelector = mkSelector "usesGroupingSeparator"

-- | @Selector@ for @setUsesGroupingSeparator:@
setUsesGroupingSeparatorSelector :: Selector
setUsesGroupingSeparatorSelector = mkSelector "setUsesGroupingSeparator:"

-- | @Selector@ for @groupingSeparator@
groupingSeparatorSelector :: Selector
groupingSeparatorSelector = mkSelector "groupingSeparator"

-- | @Selector@ for @setGroupingSeparator:@
setGroupingSeparatorSelector :: Selector
setGroupingSeparatorSelector = mkSelector "setGroupingSeparator:"

-- | @Selector@ for @zeroSymbol@
zeroSymbolSelector :: Selector
zeroSymbolSelector = mkSelector "zeroSymbol"

-- | @Selector@ for @setZeroSymbol:@
setZeroSymbolSelector :: Selector
setZeroSymbolSelector = mkSelector "setZeroSymbol:"

-- | @Selector@ for @textAttributesForZero@
textAttributesForZeroSelector :: Selector
textAttributesForZeroSelector = mkSelector "textAttributesForZero"

-- | @Selector@ for @setTextAttributesForZero:@
setTextAttributesForZeroSelector :: Selector
setTextAttributesForZeroSelector = mkSelector "setTextAttributesForZero:"

-- | @Selector@ for @nilSymbol@
nilSymbolSelector :: Selector
nilSymbolSelector = mkSelector "nilSymbol"

-- | @Selector@ for @setNilSymbol:@
setNilSymbolSelector :: Selector
setNilSymbolSelector = mkSelector "setNilSymbol:"

-- | @Selector@ for @textAttributesForNil@
textAttributesForNilSelector :: Selector
textAttributesForNilSelector = mkSelector "textAttributesForNil"

-- | @Selector@ for @setTextAttributesForNil:@
setTextAttributesForNilSelector :: Selector
setTextAttributesForNilSelector = mkSelector "setTextAttributesForNil:"

-- | @Selector@ for @notANumberSymbol@
notANumberSymbolSelector :: Selector
notANumberSymbolSelector = mkSelector "notANumberSymbol"

-- | @Selector@ for @setNotANumberSymbol:@
setNotANumberSymbolSelector :: Selector
setNotANumberSymbolSelector = mkSelector "setNotANumberSymbol:"

-- | @Selector@ for @textAttributesForNotANumber@
textAttributesForNotANumberSelector :: Selector
textAttributesForNotANumberSelector = mkSelector "textAttributesForNotANumber"

-- | @Selector@ for @setTextAttributesForNotANumber:@
setTextAttributesForNotANumberSelector :: Selector
setTextAttributesForNotANumberSelector = mkSelector "setTextAttributesForNotANumber:"

-- | @Selector@ for @positiveInfinitySymbol@
positiveInfinitySymbolSelector :: Selector
positiveInfinitySymbolSelector = mkSelector "positiveInfinitySymbol"

-- | @Selector@ for @setPositiveInfinitySymbol:@
setPositiveInfinitySymbolSelector :: Selector
setPositiveInfinitySymbolSelector = mkSelector "setPositiveInfinitySymbol:"

-- | @Selector@ for @textAttributesForPositiveInfinity@
textAttributesForPositiveInfinitySelector :: Selector
textAttributesForPositiveInfinitySelector = mkSelector "textAttributesForPositiveInfinity"

-- | @Selector@ for @setTextAttributesForPositiveInfinity:@
setTextAttributesForPositiveInfinitySelector :: Selector
setTextAttributesForPositiveInfinitySelector = mkSelector "setTextAttributesForPositiveInfinity:"

-- | @Selector@ for @negativeInfinitySymbol@
negativeInfinitySymbolSelector :: Selector
negativeInfinitySymbolSelector = mkSelector "negativeInfinitySymbol"

-- | @Selector@ for @setNegativeInfinitySymbol:@
setNegativeInfinitySymbolSelector :: Selector
setNegativeInfinitySymbolSelector = mkSelector "setNegativeInfinitySymbol:"

-- | @Selector@ for @textAttributesForNegativeInfinity@
textAttributesForNegativeInfinitySelector :: Selector
textAttributesForNegativeInfinitySelector = mkSelector "textAttributesForNegativeInfinity"

-- | @Selector@ for @setTextAttributesForNegativeInfinity:@
setTextAttributesForNegativeInfinitySelector :: Selector
setTextAttributesForNegativeInfinitySelector = mkSelector "setTextAttributesForNegativeInfinity:"

-- | @Selector@ for @positivePrefix@
positivePrefixSelector :: Selector
positivePrefixSelector = mkSelector "positivePrefix"

-- | @Selector@ for @setPositivePrefix:@
setPositivePrefixSelector :: Selector
setPositivePrefixSelector = mkSelector "setPositivePrefix:"

-- | @Selector@ for @positiveSuffix@
positiveSuffixSelector :: Selector
positiveSuffixSelector = mkSelector "positiveSuffix"

-- | @Selector@ for @setPositiveSuffix:@
setPositiveSuffixSelector :: Selector
setPositiveSuffixSelector = mkSelector "setPositiveSuffix:"

-- | @Selector@ for @negativePrefix@
negativePrefixSelector :: Selector
negativePrefixSelector = mkSelector "negativePrefix"

-- | @Selector@ for @setNegativePrefix:@
setNegativePrefixSelector :: Selector
setNegativePrefixSelector = mkSelector "setNegativePrefix:"

-- | @Selector@ for @negativeSuffix@
negativeSuffixSelector :: Selector
negativeSuffixSelector = mkSelector "negativeSuffix"

-- | @Selector@ for @setNegativeSuffix:@
setNegativeSuffixSelector :: Selector
setNegativeSuffixSelector = mkSelector "setNegativeSuffix:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @currencySymbol@
currencySymbolSelector :: Selector
currencySymbolSelector = mkSelector "currencySymbol"

-- | @Selector@ for @setCurrencySymbol:@
setCurrencySymbolSelector :: Selector
setCurrencySymbolSelector = mkSelector "setCurrencySymbol:"

-- | @Selector@ for @internationalCurrencySymbol@
internationalCurrencySymbolSelector :: Selector
internationalCurrencySymbolSelector = mkSelector "internationalCurrencySymbol"

-- | @Selector@ for @setInternationalCurrencySymbol:@
setInternationalCurrencySymbolSelector :: Selector
setInternationalCurrencySymbolSelector = mkSelector "setInternationalCurrencySymbol:"

-- | @Selector@ for @percentSymbol@
percentSymbolSelector :: Selector
percentSymbolSelector = mkSelector "percentSymbol"

-- | @Selector@ for @setPercentSymbol:@
setPercentSymbolSelector :: Selector
setPercentSymbolSelector = mkSelector "setPercentSymbol:"

-- | @Selector@ for @perMillSymbol@
perMillSymbolSelector :: Selector
perMillSymbolSelector = mkSelector "perMillSymbol"

-- | @Selector@ for @setPerMillSymbol:@
setPerMillSymbolSelector :: Selector
setPerMillSymbolSelector = mkSelector "setPerMillSymbol:"

-- | @Selector@ for @minusSign@
minusSignSelector :: Selector
minusSignSelector = mkSelector "minusSign"

-- | @Selector@ for @setMinusSign:@
setMinusSignSelector :: Selector
setMinusSignSelector = mkSelector "setMinusSign:"

-- | @Selector@ for @plusSign@
plusSignSelector :: Selector
plusSignSelector = mkSelector "plusSign"

-- | @Selector@ for @setPlusSign:@
setPlusSignSelector :: Selector
setPlusSignSelector = mkSelector "setPlusSign:"

-- | @Selector@ for @exponentSymbol@
exponentSymbolSelector :: Selector
exponentSymbolSelector = mkSelector "exponentSymbol"

-- | @Selector@ for @setExponentSymbol:@
setExponentSymbolSelector :: Selector
setExponentSymbolSelector = mkSelector "setExponentSymbol:"

-- | @Selector@ for @groupingSize@
groupingSizeSelector :: Selector
groupingSizeSelector = mkSelector "groupingSize"

-- | @Selector@ for @setGroupingSize:@
setGroupingSizeSelector :: Selector
setGroupingSizeSelector = mkSelector "setGroupingSize:"

-- | @Selector@ for @secondaryGroupingSize@
secondaryGroupingSizeSelector :: Selector
secondaryGroupingSizeSelector = mkSelector "secondaryGroupingSize"

-- | @Selector@ for @setSecondaryGroupingSize:@
setSecondaryGroupingSizeSelector :: Selector
setSecondaryGroupingSizeSelector = mkSelector "setSecondaryGroupingSize:"

-- | @Selector@ for @multiplier@
multiplierSelector :: Selector
multiplierSelector = mkSelector "multiplier"

-- | @Selector@ for @setMultiplier:@
setMultiplierSelector :: Selector
setMultiplierSelector = mkSelector "setMultiplier:"

-- | @Selector@ for @formatWidth@
formatWidthSelector :: Selector
formatWidthSelector = mkSelector "formatWidth"

-- | @Selector@ for @setFormatWidth:@
setFormatWidthSelector :: Selector
setFormatWidthSelector = mkSelector "setFormatWidth:"

-- | @Selector@ for @paddingCharacter@
paddingCharacterSelector :: Selector
paddingCharacterSelector = mkSelector "paddingCharacter"

-- | @Selector@ for @setPaddingCharacter:@
setPaddingCharacterSelector :: Selector
setPaddingCharacterSelector = mkSelector "setPaddingCharacter:"

-- | @Selector@ for @paddingPosition@
paddingPositionSelector :: Selector
paddingPositionSelector = mkSelector "paddingPosition"

-- | @Selector@ for @setPaddingPosition:@
setPaddingPositionSelector :: Selector
setPaddingPositionSelector = mkSelector "setPaddingPosition:"

-- | @Selector@ for @roundingMode@
roundingModeSelector :: Selector
roundingModeSelector = mkSelector "roundingMode"

-- | @Selector@ for @setRoundingMode:@
setRoundingModeSelector :: Selector
setRoundingModeSelector = mkSelector "setRoundingMode:"

-- | @Selector@ for @roundingIncrement@
roundingIncrementSelector :: Selector
roundingIncrementSelector = mkSelector "roundingIncrement"

-- | @Selector@ for @setRoundingIncrement:@
setRoundingIncrementSelector :: Selector
setRoundingIncrementSelector = mkSelector "setRoundingIncrement:"

-- | @Selector@ for @minimumIntegerDigits@
minimumIntegerDigitsSelector :: Selector
minimumIntegerDigitsSelector = mkSelector "minimumIntegerDigits"

-- | @Selector@ for @setMinimumIntegerDigits:@
setMinimumIntegerDigitsSelector :: Selector
setMinimumIntegerDigitsSelector = mkSelector "setMinimumIntegerDigits:"

-- | @Selector@ for @maximumIntegerDigits@
maximumIntegerDigitsSelector :: Selector
maximumIntegerDigitsSelector = mkSelector "maximumIntegerDigits"

-- | @Selector@ for @setMaximumIntegerDigits:@
setMaximumIntegerDigitsSelector :: Selector
setMaximumIntegerDigitsSelector = mkSelector "setMaximumIntegerDigits:"

-- | @Selector@ for @minimumFractionDigits@
minimumFractionDigitsSelector :: Selector
minimumFractionDigitsSelector = mkSelector "minimumFractionDigits"

-- | @Selector@ for @setMinimumFractionDigits:@
setMinimumFractionDigitsSelector :: Selector
setMinimumFractionDigitsSelector = mkSelector "setMinimumFractionDigits:"

-- | @Selector@ for @maximumFractionDigits@
maximumFractionDigitsSelector :: Selector
maximumFractionDigitsSelector = mkSelector "maximumFractionDigits"

-- | @Selector@ for @setMaximumFractionDigits:@
setMaximumFractionDigitsSelector :: Selector
setMaximumFractionDigitsSelector = mkSelector "setMaximumFractionDigits:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @setMinimum:@
setMinimumSelector :: Selector
setMinimumSelector = mkSelector "setMinimum:"

-- | @Selector@ for @maximum@
maximumSelector :: Selector
maximumSelector = mkSelector "maximum"

-- | @Selector@ for @setMaximum:@
setMaximumSelector :: Selector
setMaximumSelector = mkSelector "setMaximum:"

-- | @Selector@ for @currencyGroupingSeparator@
currencyGroupingSeparatorSelector :: Selector
currencyGroupingSeparatorSelector = mkSelector "currencyGroupingSeparator"

-- | @Selector@ for @setCurrencyGroupingSeparator:@
setCurrencyGroupingSeparatorSelector :: Selector
setCurrencyGroupingSeparatorSelector = mkSelector "setCurrencyGroupingSeparator:"

-- | @Selector@ for @lenient@
lenientSelector :: Selector
lenientSelector = mkSelector "lenient"

-- | @Selector@ for @setLenient:@
setLenientSelector :: Selector
setLenientSelector = mkSelector "setLenient:"

-- | @Selector@ for @usesSignificantDigits@
usesSignificantDigitsSelector :: Selector
usesSignificantDigitsSelector = mkSelector "usesSignificantDigits"

-- | @Selector@ for @setUsesSignificantDigits:@
setUsesSignificantDigitsSelector :: Selector
setUsesSignificantDigitsSelector = mkSelector "setUsesSignificantDigits:"

-- | @Selector@ for @minimumSignificantDigits@
minimumSignificantDigitsSelector :: Selector
minimumSignificantDigitsSelector = mkSelector "minimumSignificantDigits"

-- | @Selector@ for @setMinimumSignificantDigits:@
setMinimumSignificantDigitsSelector :: Selector
setMinimumSignificantDigitsSelector = mkSelector "setMinimumSignificantDigits:"

-- | @Selector@ for @maximumSignificantDigits@
maximumSignificantDigitsSelector :: Selector
maximumSignificantDigitsSelector = mkSelector "maximumSignificantDigits"

-- | @Selector@ for @setMaximumSignificantDigits:@
setMaximumSignificantDigitsSelector :: Selector
setMaximumSignificantDigitsSelector = mkSelector "setMaximumSignificantDigits:"

-- | @Selector@ for @partialStringValidationEnabled@
partialStringValidationEnabledSelector :: Selector
partialStringValidationEnabledSelector = mkSelector "partialStringValidationEnabled"

-- | @Selector@ for @setPartialStringValidationEnabled:@
setPartialStringValidationEnabledSelector :: Selector
setPartialStringValidationEnabledSelector = mkSelector "setPartialStringValidationEnabled:"

-- | @Selector@ for @hasThousandSeparators@
hasThousandSeparatorsSelector :: Selector
hasThousandSeparatorsSelector = mkSelector "hasThousandSeparators"

-- | @Selector@ for @setHasThousandSeparators:@
setHasThousandSeparatorsSelector :: Selector
setHasThousandSeparatorsSelector = mkSelector "setHasThousandSeparators:"

-- | @Selector@ for @thousandSeparator@
thousandSeparatorSelector :: Selector
thousandSeparatorSelector = mkSelector "thousandSeparator"

-- | @Selector@ for @setThousandSeparator:@
setThousandSeparatorSelector :: Selector
setThousandSeparatorSelector = mkSelector "setThousandSeparator:"

-- | @Selector@ for @localizesFormat@
localizesFormatSelector :: Selector
localizesFormatSelector = mkSelector "localizesFormat"

-- | @Selector@ for @setLocalizesFormat:@
setLocalizesFormatSelector :: Selector
setLocalizesFormatSelector = mkSelector "setLocalizesFormat:"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @attributedStringForZero@
attributedStringForZeroSelector :: Selector
attributedStringForZeroSelector = mkSelector "attributedStringForZero"

-- | @Selector@ for @setAttributedStringForZero:@
setAttributedStringForZeroSelector :: Selector
setAttributedStringForZeroSelector = mkSelector "setAttributedStringForZero:"

-- | @Selector@ for @attributedStringForNil@
attributedStringForNilSelector :: Selector
attributedStringForNilSelector = mkSelector "attributedStringForNil"

-- | @Selector@ for @setAttributedStringForNil:@
setAttributedStringForNilSelector :: Selector
setAttributedStringForNilSelector = mkSelector "setAttributedStringForNil:"

-- | @Selector@ for @attributedStringForNotANumber@
attributedStringForNotANumberSelector :: Selector
attributedStringForNotANumberSelector = mkSelector "attributedStringForNotANumber"

-- | @Selector@ for @setAttributedStringForNotANumber:@
setAttributedStringForNotANumberSelector :: Selector
setAttributedStringForNotANumberSelector = mkSelector "setAttributedStringForNotANumber:"

-- | @Selector@ for @roundingBehavior@
roundingBehaviorSelector :: Selector
roundingBehaviorSelector = mkSelector "roundingBehavior"

-- | @Selector@ for @setRoundingBehavior:@
setRoundingBehaviorSelector :: Selector
setRoundingBehaviorSelector = mkSelector "setRoundingBehavior:"

