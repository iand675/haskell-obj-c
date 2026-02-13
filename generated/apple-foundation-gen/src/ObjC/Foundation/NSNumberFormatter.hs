{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsFloatsSelector
  , alwaysShowsDecimalSeparatorSelector
  , attributedStringForNilSelector
  , attributedStringForNotANumberSelector
  , attributedStringForZeroSelector
  , currencyCodeSelector
  , currencyDecimalSeparatorSelector
  , currencyGroupingSeparatorSelector
  , currencySymbolSelector
  , decimalSeparatorSelector
  , defaultFormatterBehaviorSelector
  , exponentSymbolSelector
  , formatSelector
  , formatWidthSelector
  , formatterBehaviorSelector
  , formattingContextSelector
  , generatesDecimalNumbersSelector
  , getObjectValue_forString_range_errorSelector
  , groupingSeparatorSelector
  , groupingSizeSelector
  , hasThousandSeparatorsSelector
  , internationalCurrencySymbolSelector
  , lenientSelector
  , localeSelector
  , localizedStringFromNumber_numberStyleSelector
  , localizesFormatSelector
  , maximumFractionDigitsSelector
  , maximumIntegerDigitsSelector
  , maximumSelector
  , maximumSignificantDigitsSelector
  , minimumFractionDigitsSelector
  , minimumGroupingDigitsSelector
  , minimumIntegerDigitsSelector
  , minimumSelector
  , minimumSignificantDigitsSelector
  , minusSignSelector
  , multiplierSelector
  , negativeFormatSelector
  , negativeInfinitySymbolSelector
  , negativePrefixSelector
  , negativeSuffixSelector
  , nilSymbolSelector
  , notANumberSymbolSelector
  , numberFromStringSelector
  , numberStyleSelector
  , paddingCharacterSelector
  , paddingPositionSelector
  , partialStringValidationEnabledSelector
  , perMillSymbolSelector
  , percentSymbolSelector
  , plusSignSelector
  , positiveFormatSelector
  , positiveInfinitySymbolSelector
  , positivePrefixSelector
  , positiveSuffixSelector
  , roundingBehaviorSelector
  , roundingIncrementSelector
  , roundingModeSelector
  , secondaryGroupingSizeSelector
  , setAllowsFloatsSelector
  , setAlwaysShowsDecimalSeparatorSelector
  , setAttributedStringForNilSelector
  , setAttributedStringForNotANumberSelector
  , setAttributedStringForZeroSelector
  , setCurrencyCodeSelector
  , setCurrencyDecimalSeparatorSelector
  , setCurrencyGroupingSeparatorSelector
  , setCurrencySymbolSelector
  , setDecimalSeparatorSelector
  , setDefaultFormatterBehaviorSelector
  , setExponentSymbolSelector
  , setFormatSelector
  , setFormatWidthSelector
  , setFormatterBehaviorSelector
  , setFormattingContextSelector
  , setGeneratesDecimalNumbersSelector
  , setGroupingSeparatorSelector
  , setGroupingSizeSelector
  , setHasThousandSeparatorsSelector
  , setInternationalCurrencySymbolSelector
  , setLenientSelector
  , setLocaleSelector
  , setLocalizesFormatSelector
  , setMaximumFractionDigitsSelector
  , setMaximumIntegerDigitsSelector
  , setMaximumSelector
  , setMaximumSignificantDigitsSelector
  , setMinimumFractionDigitsSelector
  , setMinimumGroupingDigitsSelector
  , setMinimumIntegerDigitsSelector
  , setMinimumSelector
  , setMinimumSignificantDigitsSelector
  , setMinusSignSelector
  , setMultiplierSelector
  , setNegativeFormatSelector
  , setNegativeInfinitySymbolSelector
  , setNegativePrefixSelector
  , setNegativeSuffixSelector
  , setNilSymbolSelector
  , setNotANumberSymbolSelector
  , setNumberStyleSelector
  , setPaddingCharacterSelector
  , setPaddingPositionSelector
  , setPartialStringValidationEnabledSelector
  , setPerMillSymbolSelector
  , setPercentSymbolSelector
  , setPlusSignSelector
  , setPositiveFormatSelector
  , setPositiveInfinitySymbolSelector
  , setPositivePrefixSelector
  , setPositiveSuffixSelector
  , setRoundingBehaviorSelector
  , setRoundingIncrementSelector
  , setRoundingModeSelector
  , setSecondaryGroupingSizeSelector
  , setTextAttributesForNegativeInfinitySelector
  , setTextAttributesForNegativeValuesSelector
  , setTextAttributesForNilSelector
  , setTextAttributesForNotANumberSelector
  , setTextAttributesForPositiveInfinitySelector
  , setTextAttributesForPositiveValuesSelector
  , setTextAttributesForZeroSelector
  , setThousandSeparatorSelector
  , setUsesGroupingSeparatorSelector
  , setUsesSignificantDigitsSelector
  , setZeroSymbolSelector
  , stringFromNumberSelector
  , textAttributesForNegativeInfinitySelector
  , textAttributesForNegativeValuesSelector
  , textAttributesForNilSelector
  , textAttributesForNotANumberSelector
  , textAttributesForPositiveInfinitySelector
  , textAttributesForPositiveValuesSelector
  , textAttributesForZeroSelector
  , thousandSeparatorSelector
  , usesGroupingSeparatorSelector
  , usesSignificantDigitsSelector
  , zeroSymbolSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- getObjectValue:forString:range:error:@
getObjectValue_forString_range_error :: (IsNSNumberFormatter nsNumberFormatter, IsNSString string, IsNSError error_) => nsNumberFormatter -> Ptr RawId -> string -> Ptr NSRange -> error_ -> IO Bool
getObjectValue_forString_range_error nsNumberFormatter obj_ string rangep error_ =
  sendMessage nsNumberFormatter getObjectValue_forString_range_errorSelector obj_ (toNSString string) rangep (toNSError error_)

-- | @- stringFromNumber:@
stringFromNumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber number) => nsNumberFormatter -> number -> IO (Id NSString)
stringFromNumber nsNumberFormatter number =
  sendMessage nsNumberFormatter stringFromNumberSelector (toNSNumber number)

-- | @- numberFromString:@
numberFromString :: (IsNSNumberFormatter nsNumberFormatter, IsNSString string) => nsNumberFormatter -> string -> IO (Id NSNumber)
numberFromString nsNumberFormatter string =
  sendMessage nsNumberFormatter numberFromStringSelector (toNSString string)

-- | @+ localizedStringFromNumber:numberStyle:@
localizedStringFromNumber_numberStyle :: IsNSNumber num => num -> NSNumberFormatterStyle -> IO (Id NSString)
localizedStringFromNumber_numberStyle num nstyle =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    sendClassMessage cls' localizedStringFromNumber_numberStyleSelector (toNSNumber num) nstyle

-- | @+ defaultFormatterBehavior@
defaultFormatterBehavior :: IO NSNumberFormatterBehavior
defaultFormatterBehavior  =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    sendClassMessage cls' defaultFormatterBehaviorSelector

-- | @+ setDefaultFormatterBehavior:@
setDefaultFormatterBehavior :: NSNumberFormatterBehavior -> IO ()
setDefaultFormatterBehavior behavior =
  do
    cls' <- getRequiredClass "NSNumberFormatter"
    sendClassMessage cls' setDefaultFormatterBehaviorSelector behavior

-- | @- formattingContext@
formattingContext :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSFormattingContext
formattingContext nsNumberFormatter =
  sendMessage nsNumberFormatter formattingContextSelector

-- | @- setFormattingContext:@
setFormattingContext :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSFormattingContext -> IO ()
setFormattingContext nsNumberFormatter value =
  sendMessage nsNumberFormatter setFormattingContextSelector value

-- | @- minimumGroupingDigits@
minimumGroupingDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CLong
minimumGroupingDigits nsNumberFormatter =
  sendMessage nsNumberFormatter minimumGroupingDigitsSelector

-- | @- setMinimumGroupingDigits:@
setMinimumGroupingDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CLong -> IO ()
setMinimumGroupingDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinimumGroupingDigitsSelector value

-- | @- numberStyle@
numberStyle :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterStyle
numberStyle nsNumberFormatter =
  sendMessage nsNumberFormatter numberStyleSelector

-- | @- setNumberStyle:@
setNumberStyle :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterStyle -> IO ()
setNumberStyle nsNumberFormatter value =
  sendMessage nsNumberFormatter setNumberStyleSelector value

-- | @- locale@
locale :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSLocale)
locale nsNumberFormatter =
  sendMessage nsNumberFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSNumberFormatter nsNumberFormatter, IsNSLocale value) => nsNumberFormatter -> value -> IO ()
setLocale nsNumberFormatter value =
  sendMessage nsNumberFormatter setLocaleSelector (toNSLocale value)

-- | @- generatesDecimalNumbers@
generatesDecimalNumbers :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
generatesDecimalNumbers nsNumberFormatter =
  sendMessage nsNumberFormatter generatesDecimalNumbersSelector

-- | @- setGeneratesDecimalNumbers:@
setGeneratesDecimalNumbers :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setGeneratesDecimalNumbers nsNumberFormatter value =
  sendMessage nsNumberFormatter setGeneratesDecimalNumbersSelector value

-- | @- formatterBehavior@
formatterBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterBehavior
formatterBehavior nsNumberFormatter =
  sendMessage nsNumberFormatter formatterBehaviorSelector

-- | @- setFormatterBehavior:@
setFormatterBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterBehavior -> IO ()
setFormatterBehavior nsNumberFormatter value =
  sendMessage nsNumberFormatter setFormatterBehaviorSelector value

-- | @- negativeFormat@
negativeFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeFormat nsNumberFormatter =
  sendMessage nsNumberFormatter negativeFormatSelector

-- | @- setNegativeFormat:@
setNegativeFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeFormat nsNumberFormatter value =
  sendMessage nsNumberFormatter setNegativeFormatSelector (toNSString value)

-- | @- textAttributesForNegativeValues@
textAttributesForNegativeValues :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNegativeValues nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForNegativeValuesSelector

-- | @- setTextAttributesForNegativeValues:@
setTextAttributesForNegativeValues :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNegativeValues nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForNegativeValuesSelector (toNSDictionary value)

-- | @- positiveFormat@
positiveFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveFormat nsNumberFormatter =
  sendMessage nsNumberFormatter positiveFormatSelector

-- | @- setPositiveFormat:@
setPositiveFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveFormat nsNumberFormatter value =
  sendMessage nsNumberFormatter setPositiveFormatSelector (toNSString value)

-- | @- textAttributesForPositiveValues@
textAttributesForPositiveValues :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForPositiveValues nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForPositiveValuesSelector

-- | @- setTextAttributesForPositiveValues:@
setTextAttributesForPositiveValues :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForPositiveValues nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForPositiveValuesSelector (toNSDictionary value)

-- | @- allowsFloats@
allowsFloats :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
allowsFloats nsNumberFormatter =
  sendMessage nsNumberFormatter allowsFloatsSelector

-- | @- setAllowsFloats:@
setAllowsFloats :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setAllowsFloats nsNumberFormatter value =
  sendMessage nsNumberFormatter setAllowsFloatsSelector value

-- | @- decimalSeparator@
decimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
decimalSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter decimalSeparatorSelector

-- | @- setDecimalSeparator:@
setDecimalSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setDecimalSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setDecimalSeparatorSelector (toNSString value)

-- | @- alwaysShowsDecimalSeparator@
alwaysShowsDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
alwaysShowsDecimalSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter alwaysShowsDecimalSeparatorSelector

-- | @- setAlwaysShowsDecimalSeparator:@
setAlwaysShowsDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setAlwaysShowsDecimalSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setAlwaysShowsDecimalSeparatorSelector value

-- | @- currencyDecimalSeparator@
currencyDecimalSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyDecimalSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter currencyDecimalSeparatorSelector

-- | @- setCurrencyDecimalSeparator:@
setCurrencyDecimalSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyDecimalSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setCurrencyDecimalSeparatorSelector (toNSString value)

-- | @- usesGroupingSeparator@
usesGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
usesGroupingSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter usesGroupingSeparatorSelector

-- | @- setUsesGroupingSeparator:@
setUsesGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setUsesGroupingSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setUsesGroupingSeparatorSelector value

-- | @- groupingSeparator@
groupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
groupingSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter groupingSeparatorSelector

-- | @- setGroupingSeparator:@
setGroupingSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setGroupingSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setGroupingSeparatorSelector (toNSString value)

-- | @- zeroSymbol@
zeroSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
zeroSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter zeroSymbolSelector

-- | @- setZeroSymbol:@
setZeroSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setZeroSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setZeroSymbolSelector (toNSString value)

-- | @- textAttributesForZero@
textAttributesForZero :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForZero nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForZeroSelector

-- | @- setTextAttributesForZero:@
setTextAttributesForZero :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForZero nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForZeroSelector (toNSDictionary value)

-- | @- nilSymbol@
nilSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
nilSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter nilSymbolSelector

-- | @- setNilSymbol:@
setNilSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNilSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setNilSymbolSelector (toNSString value)

-- | @- textAttributesForNil@
textAttributesForNil :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNil nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForNilSelector

-- | @- setTextAttributesForNil:@
setTextAttributesForNil :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNil nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForNilSelector (toNSDictionary value)

-- | @- notANumberSymbol@
notANumberSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
notANumberSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter notANumberSymbolSelector

-- | @- setNotANumberSymbol:@
setNotANumberSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNotANumberSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setNotANumberSymbolSelector (toNSString value)

-- | @- textAttributesForNotANumber@
textAttributesForNotANumber :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNotANumber nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForNotANumberSelector

-- | @- setTextAttributesForNotANumber:@
setTextAttributesForNotANumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNotANumber nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForNotANumberSelector (toNSDictionary value)

-- | @- positiveInfinitySymbol@
positiveInfinitySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveInfinitySymbol nsNumberFormatter =
  sendMessage nsNumberFormatter positiveInfinitySymbolSelector

-- | @- setPositiveInfinitySymbol:@
setPositiveInfinitySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveInfinitySymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setPositiveInfinitySymbolSelector (toNSString value)

-- | @- textAttributesForPositiveInfinity@
textAttributesForPositiveInfinity :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForPositiveInfinity nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForPositiveInfinitySelector

-- | @- setTextAttributesForPositiveInfinity:@
setTextAttributesForPositiveInfinity :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForPositiveInfinity nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForPositiveInfinitySelector (toNSDictionary value)

-- | @- negativeInfinitySymbol@
negativeInfinitySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeInfinitySymbol nsNumberFormatter =
  sendMessage nsNumberFormatter negativeInfinitySymbolSelector

-- | @- setNegativeInfinitySymbol:@
setNegativeInfinitySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeInfinitySymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setNegativeInfinitySymbolSelector (toNSString value)

-- | @- textAttributesForNegativeInfinity@
textAttributesForNegativeInfinity :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDictionary)
textAttributesForNegativeInfinity nsNumberFormatter =
  sendMessage nsNumberFormatter textAttributesForNegativeInfinitySelector

-- | @- setTextAttributesForNegativeInfinity:@
setTextAttributesForNegativeInfinity :: (IsNSNumberFormatter nsNumberFormatter, IsNSDictionary value) => nsNumberFormatter -> value -> IO ()
setTextAttributesForNegativeInfinity nsNumberFormatter value =
  sendMessage nsNumberFormatter setTextAttributesForNegativeInfinitySelector (toNSDictionary value)

-- | @- positivePrefix@
positivePrefix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positivePrefix nsNumberFormatter =
  sendMessage nsNumberFormatter positivePrefixSelector

-- | @- setPositivePrefix:@
setPositivePrefix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositivePrefix nsNumberFormatter value =
  sendMessage nsNumberFormatter setPositivePrefixSelector (toNSString value)

-- | @- positiveSuffix@
positiveSuffix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
positiveSuffix nsNumberFormatter =
  sendMessage nsNumberFormatter positiveSuffixSelector

-- | @- setPositiveSuffix:@
setPositiveSuffix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPositiveSuffix nsNumberFormatter value =
  sendMessage nsNumberFormatter setPositiveSuffixSelector (toNSString value)

-- | @- negativePrefix@
negativePrefix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativePrefix nsNumberFormatter =
  sendMessage nsNumberFormatter negativePrefixSelector

-- | @- setNegativePrefix:@
setNegativePrefix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativePrefix nsNumberFormatter value =
  sendMessage nsNumberFormatter setNegativePrefixSelector (toNSString value)

-- | @- negativeSuffix@
negativeSuffix :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
negativeSuffix nsNumberFormatter =
  sendMessage nsNumberFormatter negativeSuffixSelector

-- | @- setNegativeSuffix:@
setNegativeSuffix :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setNegativeSuffix nsNumberFormatter value =
  sendMessage nsNumberFormatter setNegativeSuffixSelector (toNSString value)

-- | @- currencyCode@
currencyCode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyCode nsNumberFormatter =
  sendMessage nsNumberFormatter currencyCodeSelector

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyCode nsNumberFormatter value =
  sendMessage nsNumberFormatter setCurrencyCodeSelector (toNSString value)

-- | @- currencySymbol@
currencySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencySymbol nsNumberFormatter =
  sendMessage nsNumberFormatter currencySymbolSelector

-- | @- setCurrencySymbol:@
setCurrencySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencySymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setCurrencySymbolSelector (toNSString value)

-- | @- internationalCurrencySymbol@
internationalCurrencySymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
internationalCurrencySymbol nsNumberFormatter =
  sendMessage nsNumberFormatter internationalCurrencySymbolSelector

-- | @- setInternationalCurrencySymbol:@
setInternationalCurrencySymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setInternationalCurrencySymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setInternationalCurrencySymbolSelector (toNSString value)

-- | @- percentSymbol@
percentSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
percentSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter percentSymbolSelector

-- | @- setPercentSymbol:@
setPercentSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPercentSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setPercentSymbolSelector (toNSString value)

-- | @- perMillSymbol@
perMillSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
perMillSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter perMillSymbolSelector

-- | @- setPerMillSymbol:@
setPerMillSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPerMillSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setPerMillSymbolSelector (toNSString value)

-- | @- minusSign@
minusSign :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
minusSign nsNumberFormatter =
  sendMessage nsNumberFormatter minusSignSelector

-- | @- setMinusSign:@
setMinusSign :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setMinusSign nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinusSignSelector (toNSString value)

-- | @- plusSign@
plusSign :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
plusSign nsNumberFormatter =
  sendMessage nsNumberFormatter plusSignSelector

-- | @- setPlusSign:@
setPlusSign :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPlusSign nsNumberFormatter value =
  sendMessage nsNumberFormatter setPlusSignSelector (toNSString value)

-- | @- exponentSymbol@
exponentSymbol :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
exponentSymbol nsNumberFormatter =
  sendMessage nsNumberFormatter exponentSymbolSelector

-- | @- setExponentSymbol:@
setExponentSymbol :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setExponentSymbol nsNumberFormatter value =
  sendMessage nsNumberFormatter setExponentSymbolSelector (toNSString value)

-- | @- groupingSize@
groupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
groupingSize nsNumberFormatter =
  sendMessage nsNumberFormatter groupingSizeSelector

-- | @- setGroupingSize:@
setGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setGroupingSize nsNumberFormatter value =
  sendMessage nsNumberFormatter setGroupingSizeSelector value

-- | @- secondaryGroupingSize@
secondaryGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
secondaryGroupingSize nsNumberFormatter =
  sendMessage nsNumberFormatter secondaryGroupingSizeSelector

-- | @- setSecondaryGroupingSize:@
setSecondaryGroupingSize :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setSecondaryGroupingSize nsNumberFormatter value =
  sendMessage nsNumberFormatter setSecondaryGroupingSizeSelector value

-- | @- multiplier@
multiplier :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
multiplier nsNumberFormatter =
  sendMessage nsNumberFormatter multiplierSelector

-- | @- setMultiplier:@
setMultiplier :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMultiplier nsNumberFormatter value =
  sendMessage nsNumberFormatter setMultiplierSelector (toNSNumber value)

-- | @- formatWidth@
formatWidth :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
formatWidth nsNumberFormatter =
  sendMessage nsNumberFormatter formatWidthSelector

-- | @- setFormatWidth:@
setFormatWidth :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setFormatWidth nsNumberFormatter value =
  sendMessage nsNumberFormatter setFormatWidthSelector value

-- | @- paddingCharacter@
paddingCharacter :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
paddingCharacter nsNumberFormatter =
  sendMessage nsNumberFormatter paddingCharacterSelector

-- | @- setPaddingCharacter:@
setPaddingCharacter :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setPaddingCharacter nsNumberFormatter value =
  sendMessage nsNumberFormatter setPaddingCharacterSelector (toNSString value)

-- | @- paddingPosition@
paddingPosition :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterPadPosition
paddingPosition nsNumberFormatter =
  sendMessage nsNumberFormatter paddingPositionSelector

-- | @- setPaddingPosition:@
setPaddingPosition :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterPadPosition -> IO ()
setPaddingPosition nsNumberFormatter value =
  sendMessage nsNumberFormatter setPaddingPositionSelector value

-- | @- roundingMode@
roundingMode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO NSNumberFormatterRoundingMode
roundingMode nsNumberFormatter =
  sendMessage nsNumberFormatter roundingModeSelector

-- | @- setRoundingMode:@
setRoundingMode :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> NSNumberFormatterRoundingMode -> IO ()
setRoundingMode nsNumberFormatter value =
  sendMessage nsNumberFormatter setRoundingModeSelector value

-- | @- roundingIncrement@
roundingIncrement :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
roundingIncrement nsNumberFormatter =
  sendMessage nsNumberFormatter roundingIncrementSelector

-- | @- setRoundingIncrement:@
setRoundingIncrement :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setRoundingIncrement nsNumberFormatter value =
  sendMessage nsNumberFormatter setRoundingIncrementSelector (toNSNumber value)

-- | @- minimumIntegerDigits@
minimumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumIntegerDigits nsNumberFormatter =
  sendMessage nsNumberFormatter minimumIntegerDigitsSelector

-- | @- setMinimumIntegerDigits:@
setMinimumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumIntegerDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinimumIntegerDigitsSelector value

-- | @- maximumIntegerDigits@
maximumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumIntegerDigits nsNumberFormatter =
  sendMessage nsNumberFormatter maximumIntegerDigitsSelector

-- | @- setMaximumIntegerDigits:@
setMaximumIntegerDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumIntegerDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMaximumIntegerDigitsSelector value

-- | @- minimumFractionDigits@
minimumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumFractionDigits nsNumberFormatter =
  sendMessage nsNumberFormatter minimumFractionDigitsSelector

-- | @- setMinimumFractionDigits:@
setMinimumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumFractionDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinimumFractionDigitsSelector value

-- | @- maximumFractionDigits@
maximumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumFractionDigits nsNumberFormatter =
  sendMessage nsNumberFormatter maximumFractionDigitsSelector

-- | @- setMaximumFractionDigits:@
setMaximumFractionDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumFractionDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMaximumFractionDigitsSelector value

-- | @- minimum@
minimum_ :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
minimum_ nsNumberFormatter =
  sendMessage nsNumberFormatter minimumSelector

-- | @- setMinimum:@
setMinimum :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMinimum nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinimumSelector (toNSNumber value)

-- | @- maximum@
maximum_ :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSNumber)
maximum_ nsNumberFormatter =
  sendMessage nsNumberFormatter maximumSelector

-- | @- setMaximum:@
setMaximum :: (IsNSNumberFormatter nsNumberFormatter, IsNSNumber value) => nsNumberFormatter -> value -> IO ()
setMaximum nsNumberFormatter value =
  sendMessage nsNumberFormatter setMaximumSelector (toNSNumber value)

-- | @- currencyGroupingSeparator@
currencyGroupingSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
currencyGroupingSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter currencyGroupingSeparatorSelector

-- | @- setCurrencyGroupingSeparator:@
setCurrencyGroupingSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setCurrencyGroupingSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setCurrencyGroupingSeparatorSelector (toNSString value)

-- | @- lenient@
lenient :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
lenient nsNumberFormatter =
  sendMessage nsNumberFormatter lenientSelector

-- | @- setLenient:@
setLenient :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setLenient nsNumberFormatter value =
  sendMessage nsNumberFormatter setLenientSelector value

-- | @- usesSignificantDigits@
usesSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
usesSignificantDigits nsNumberFormatter =
  sendMessage nsNumberFormatter usesSignificantDigitsSelector

-- | @- setUsesSignificantDigits:@
setUsesSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setUsesSignificantDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setUsesSignificantDigitsSelector value

-- | @- minimumSignificantDigits@
minimumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
minimumSignificantDigits nsNumberFormatter =
  sendMessage nsNumberFormatter minimumSignificantDigitsSelector

-- | @- setMinimumSignificantDigits:@
setMinimumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMinimumSignificantDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMinimumSignificantDigitsSelector value

-- | @- maximumSignificantDigits@
maximumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO CULong
maximumSignificantDigits nsNumberFormatter =
  sendMessage nsNumberFormatter maximumSignificantDigitsSelector

-- | @- setMaximumSignificantDigits:@
setMaximumSignificantDigits :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> CULong -> IO ()
setMaximumSignificantDigits nsNumberFormatter value =
  sendMessage nsNumberFormatter setMaximumSignificantDigitsSelector value

-- | @- partialStringValidationEnabled@
partialStringValidationEnabled :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
partialStringValidationEnabled nsNumberFormatter =
  sendMessage nsNumberFormatter partialStringValidationEnabledSelector

-- | @- setPartialStringValidationEnabled:@
setPartialStringValidationEnabled :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setPartialStringValidationEnabled nsNumberFormatter value =
  sendMessage nsNumberFormatter setPartialStringValidationEnabledSelector value

-- | @- hasThousandSeparators@
hasThousandSeparators :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
hasThousandSeparators nsNumberFormatter =
  sendMessage nsNumberFormatter hasThousandSeparatorsSelector

-- | @- setHasThousandSeparators:@
setHasThousandSeparators :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setHasThousandSeparators nsNumberFormatter value =
  sendMessage nsNumberFormatter setHasThousandSeparatorsSelector value

-- | @- thousandSeparator@
thousandSeparator :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
thousandSeparator nsNumberFormatter =
  sendMessage nsNumberFormatter thousandSeparatorSelector

-- | @- setThousandSeparator:@
setThousandSeparator :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setThousandSeparator nsNumberFormatter value =
  sendMessage nsNumberFormatter setThousandSeparatorSelector (toNSString value)

-- | @- localizesFormat@
localizesFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO Bool
localizesFormat nsNumberFormatter =
  sendMessage nsNumberFormatter localizesFormatSelector

-- | @- setLocalizesFormat:@
setLocalizesFormat :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> Bool -> IO ()
setLocalizesFormat nsNumberFormatter value =
  sendMessage nsNumberFormatter setLocalizesFormatSelector value

-- | @- format@
format :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSString)
format nsNumberFormatter =
  sendMessage nsNumberFormatter formatSelector

-- | @- setFormat:@
setFormat :: (IsNSNumberFormatter nsNumberFormatter, IsNSString value) => nsNumberFormatter -> value -> IO ()
setFormat nsNumberFormatter value =
  sendMessage nsNumberFormatter setFormatSelector (toNSString value)

-- | @- attributedStringForZero@
attributedStringForZero :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForZero nsNumberFormatter =
  sendMessage nsNumberFormatter attributedStringForZeroSelector

-- | @- setAttributedStringForZero:@
setAttributedStringForZero :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForZero nsNumberFormatter value =
  sendMessage nsNumberFormatter setAttributedStringForZeroSelector (toNSAttributedString value)

-- | @- attributedStringForNil@
attributedStringForNil :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForNil nsNumberFormatter =
  sendMessage nsNumberFormatter attributedStringForNilSelector

-- | @- setAttributedStringForNil:@
setAttributedStringForNil :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForNil nsNumberFormatter value =
  sendMessage nsNumberFormatter setAttributedStringForNilSelector (toNSAttributedString value)

-- | @- attributedStringForNotANumber@
attributedStringForNotANumber :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSAttributedString)
attributedStringForNotANumber nsNumberFormatter =
  sendMessage nsNumberFormatter attributedStringForNotANumberSelector

-- | @- setAttributedStringForNotANumber:@
setAttributedStringForNotANumber :: (IsNSNumberFormatter nsNumberFormatter, IsNSAttributedString value) => nsNumberFormatter -> value -> IO ()
setAttributedStringForNotANumber nsNumberFormatter value =
  sendMessage nsNumberFormatter setAttributedStringForNotANumberSelector (toNSAttributedString value)

-- | @- roundingBehavior@
roundingBehavior :: IsNSNumberFormatter nsNumberFormatter => nsNumberFormatter -> IO (Id NSDecimalNumberHandler)
roundingBehavior nsNumberFormatter =
  sendMessage nsNumberFormatter roundingBehaviorSelector

-- | @- setRoundingBehavior:@
setRoundingBehavior :: (IsNSNumberFormatter nsNumberFormatter, IsNSDecimalNumberHandler value) => nsNumberFormatter -> value -> IO ()
setRoundingBehavior nsNumberFormatter value =
  sendMessage nsNumberFormatter setRoundingBehaviorSelector (toNSDecimalNumberHandler value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getObjectValue:forString:range:error:@
getObjectValue_forString_range_errorSelector :: Selector '[Ptr RawId, Id NSString, Ptr NSRange, Id NSError] Bool
getObjectValue_forString_range_errorSelector = mkSelector "getObjectValue:forString:range:error:"

-- | @Selector@ for @stringFromNumber:@
stringFromNumberSelector :: Selector '[Id NSNumber] (Id NSString)
stringFromNumberSelector = mkSelector "stringFromNumber:"

-- | @Selector@ for @numberFromString:@
numberFromStringSelector :: Selector '[Id NSString] (Id NSNumber)
numberFromStringSelector = mkSelector "numberFromString:"

-- | @Selector@ for @localizedStringFromNumber:numberStyle:@
localizedStringFromNumber_numberStyleSelector :: Selector '[Id NSNumber, NSNumberFormatterStyle] (Id NSString)
localizedStringFromNumber_numberStyleSelector = mkSelector "localizedStringFromNumber:numberStyle:"

-- | @Selector@ for @defaultFormatterBehavior@
defaultFormatterBehaviorSelector :: Selector '[] NSNumberFormatterBehavior
defaultFormatterBehaviorSelector = mkSelector "defaultFormatterBehavior"

-- | @Selector@ for @setDefaultFormatterBehavior:@
setDefaultFormatterBehaviorSelector :: Selector '[NSNumberFormatterBehavior] ()
setDefaultFormatterBehaviorSelector = mkSelector "setDefaultFormatterBehavior:"

-- | @Selector@ for @formattingContext@
formattingContextSelector :: Selector '[] NSFormattingContext
formattingContextSelector = mkSelector "formattingContext"

-- | @Selector@ for @setFormattingContext:@
setFormattingContextSelector :: Selector '[NSFormattingContext] ()
setFormattingContextSelector = mkSelector "setFormattingContext:"

-- | @Selector@ for @minimumGroupingDigits@
minimumGroupingDigitsSelector :: Selector '[] CLong
minimumGroupingDigitsSelector = mkSelector "minimumGroupingDigits"

-- | @Selector@ for @setMinimumGroupingDigits:@
setMinimumGroupingDigitsSelector :: Selector '[CLong] ()
setMinimumGroupingDigitsSelector = mkSelector "setMinimumGroupingDigits:"

-- | @Selector@ for @numberStyle@
numberStyleSelector :: Selector '[] NSNumberFormatterStyle
numberStyleSelector = mkSelector "numberStyle"

-- | @Selector@ for @setNumberStyle:@
setNumberStyleSelector :: Selector '[NSNumberFormatterStyle] ()
setNumberStyleSelector = mkSelector "setNumberStyle:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @generatesDecimalNumbers@
generatesDecimalNumbersSelector :: Selector '[] Bool
generatesDecimalNumbersSelector = mkSelector "generatesDecimalNumbers"

-- | @Selector@ for @setGeneratesDecimalNumbers:@
setGeneratesDecimalNumbersSelector :: Selector '[Bool] ()
setGeneratesDecimalNumbersSelector = mkSelector "setGeneratesDecimalNumbers:"

-- | @Selector@ for @formatterBehavior@
formatterBehaviorSelector :: Selector '[] NSNumberFormatterBehavior
formatterBehaviorSelector = mkSelector "formatterBehavior"

-- | @Selector@ for @setFormatterBehavior:@
setFormatterBehaviorSelector :: Selector '[NSNumberFormatterBehavior] ()
setFormatterBehaviorSelector = mkSelector "setFormatterBehavior:"

-- | @Selector@ for @negativeFormat@
negativeFormatSelector :: Selector '[] (Id NSString)
negativeFormatSelector = mkSelector "negativeFormat"

-- | @Selector@ for @setNegativeFormat:@
setNegativeFormatSelector :: Selector '[Id NSString] ()
setNegativeFormatSelector = mkSelector "setNegativeFormat:"

-- | @Selector@ for @textAttributesForNegativeValues@
textAttributesForNegativeValuesSelector :: Selector '[] (Id NSDictionary)
textAttributesForNegativeValuesSelector = mkSelector "textAttributesForNegativeValues"

-- | @Selector@ for @setTextAttributesForNegativeValues:@
setTextAttributesForNegativeValuesSelector :: Selector '[Id NSDictionary] ()
setTextAttributesForNegativeValuesSelector = mkSelector "setTextAttributesForNegativeValues:"

-- | @Selector@ for @positiveFormat@
positiveFormatSelector :: Selector '[] (Id NSString)
positiveFormatSelector = mkSelector "positiveFormat"

-- | @Selector@ for @setPositiveFormat:@
setPositiveFormatSelector :: Selector '[Id NSString] ()
setPositiveFormatSelector = mkSelector "setPositiveFormat:"

-- | @Selector@ for @textAttributesForPositiveValues@
textAttributesForPositiveValuesSelector :: Selector '[] (Id NSDictionary)
textAttributesForPositiveValuesSelector = mkSelector "textAttributesForPositiveValues"

-- | @Selector@ for @setTextAttributesForPositiveValues:@
setTextAttributesForPositiveValuesSelector :: Selector '[Id NSDictionary] ()
setTextAttributesForPositiveValuesSelector = mkSelector "setTextAttributesForPositiveValues:"

-- | @Selector@ for @allowsFloats@
allowsFloatsSelector :: Selector '[] Bool
allowsFloatsSelector = mkSelector "allowsFloats"

-- | @Selector@ for @setAllowsFloats:@
setAllowsFloatsSelector :: Selector '[Bool] ()
setAllowsFloatsSelector = mkSelector "setAllowsFloats:"

-- | @Selector@ for @decimalSeparator@
decimalSeparatorSelector :: Selector '[] (Id NSString)
decimalSeparatorSelector = mkSelector "decimalSeparator"

-- | @Selector@ for @setDecimalSeparator:@
setDecimalSeparatorSelector :: Selector '[Id NSString] ()
setDecimalSeparatorSelector = mkSelector "setDecimalSeparator:"

-- | @Selector@ for @alwaysShowsDecimalSeparator@
alwaysShowsDecimalSeparatorSelector :: Selector '[] Bool
alwaysShowsDecimalSeparatorSelector = mkSelector "alwaysShowsDecimalSeparator"

-- | @Selector@ for @setAlwaysShowsDecimalSeparator:@
setAlwaysShowsDecimalSeparatorSelector :: Selector '[Bool] ()
setAlwaysShowsDecimalSeparatorSelector = mkSelector "setAlwaysShowsDecimalSeparator:"

-- | @Selector@ for @currencyDecimalSeparator@
currencyDecimalSeparatorSelector :: Selector '[] (Id NSString)
currencyDecimalSeparatorSelector = mkSelector "currencyDecimalSeparator"

-- | @Selector@ for @setCurrencyDecimalSeparator:@
setCurrencyDecimalSeparatorSelector :: Selector '[Id NSString] ()
setCurrencyDecimalSeparatorSelector = mkSelector "setCurrencyDecimalSeparator:"

-- | @Selector@ for @usesGroupingSeparator@
usesGroupingSeparatorSelector :: Selector '[] Bool
usesGroupingSeparatorSelector = mkSelector "usesGroupingSeparator"

-- | @Selector@ for @setUsesGroupingSeparator:@
setUsesGroupingSeparatorSelector :: Selector '[Bool] ()
setUsesGroupingSeparatorSelector = mkSelector "setUsesGroupingSeparator:"

-- | @Selector@ for @groupingSeparator@
groupingSeparatorSelector :: Selector '[] (Id NSString)
groupingSeparatorSelector = mkSelector "groupingSeparator"

-- | @Selector@ for @setGroupingSeparator:@
setGroupingSeparatorSelector :: Selector '[Id NSString] ()
setGroupingSeparatorSelector = mkSelector "setGroupingSeparator:"

-- | @Selector@ for @zeroSymbol@
zeroSymbolSelector :: Selector '[] (Id NSString)
zeroSymbolSelector = mkSelector "zeroSymbol"

-- | @Selector@ for @setZeroSymbol:@
setZeroSymbolSelector :: Selector '[Id NSString] ()
setZeroSymbolSelector = mkSelector "setZeroSymbol:"

-- | @Selector@ for @textAttributesForZero@
textAttributesForZeroSelector :: Selector '[] (Id NSDictionary)
textAttributesForZeroSelector = mkSelector "textAttributesForZero"

-- | @Selector@ for @setTextAttributesForZero:@
setTextAttributesForZeroSelector :: Selector '[Id NSDictionary] ()
setTextAttributesForZeroSelector = mkSelector "setTextAttributesForZero:"

-- | @Selector@ for @nilSymbol@
nilSymbolSelector :: Selector '[] (Id NSString)
nilSymbolSelector = mkSelector "nilSymbol"

-- | @Selector@ for @setNilSymbol:@
setNilSymbolSelector :: Selector '[Id NSString] ()
setNilSymbolSelector = mkSelector "setNilSymbol:"

-- | @Selector@ for @textAttributesForNil@
textAttributesForNilSelector :: Selector '[] (Id NSDictionary)
textAttributesForNilSelector = mkSelector "textAttributesForNil"

-- | @Selector@ for @setTextAttributesForNil:@
setTextAttributesForNilSelector :: Selector '[Id NSDictionary] ()
setTextAttributesForNilSelector = mkSelector "setTextAttributesForNil:"

-- | @Selector@ for @notANumberSymbol@
notANumberSymbolSelector :: Selector '[] (Id NSString)
notANumberSymbolSelector = mkSelector "notANumberSymbol"

-- | @Selector@ for @setNotANumberSymbol:@
setNotANumberSymbolSelector :: Selector '[Id NSString] ()
setNotANumberSymbolSelector = mkSelector "setNotANumberSymbol:"

-- | @Selector@ for @textAttributesForNotANumber@
textAttributesForNotANumberSelector :: Selector '[] (Id NSDictionary)
textAttributesForNotANumberSelector = mkSelector "textAttributesForNotANumber"

-- | @Selector@ for @setTextAttributesForNotANumber:@
setTextAttributesForNotANumberSelector :: Selector '[Id NSDictionary] ()
setTextAttributesForNotANumberSelector = mkSelector "setTextAttributesForNotANumber:"

-- | @Selector@ for @positiveInfinitySymbol@
positiveInfinitySymbolSelector :: Selector '[] (Id NSString)
positiveInfinitySymbolSelector = mkSelector "positiveInfinitySymbol"

-- | @Selector@ for @setPositiveInfinitySymbol:@
setPositiveInfinitySymbolSelector :: Selector '[Id NSString] ()
setPositiveInfinitySymbolSelector = mkSelector "setPositiveInfinitySymbol:"

-- | @Selector@ for @textAttributesForPositiveInfinity@
textAttributesForPositiveInfinitySelector :: Selector '[] (Id NSDictionary)
textAttributesForPositiveInfinitySelector = mkSelector "textAttributesForPositiveInfinity"

-- | @Selector@ for @setTextAttributesForPositiveInfinity:@
setTextAttributesForPositiveInfinitySelector :: Selector '[Id NSDictionary] ()
setTextAttributesForPositiveInfinitySelector = mkSelector "setTextAttributesForPositiveInfinity:"

-- | @Selector@ for @negativeInfinitySymbol@
negativeInfinitySymbolSelector :: Selector '[] (Id NSString)
negativeInfinitySymbolSelector = mkSelector "negativeInfinitySymbol"

-- | @Selector@ for @setNegativeInfinitySymbol:@
setNegativeInfinitySymbolSelector :: Selector '[Id NSString] ()
setNegativeInfinitySymbolSelector = mkSelector "setNegativeInfinitySymbol:"

-- | @Selector@ for @textAttributesForNegativeInfinity@
textAttributesForNegativeInfinitySelector :: Selector '[] (Id NSDictionary)
textAttributesForNegativeInfinitySelector = mkSelector "textAttributesForNegativeInfinity"

-- | @Selector@ for @setTextAttributesForNegativeInfinity:@
setTextAttributesForNegativeInfinitySelector :: Selector '[Id NSDictionary] ()
setTextAttributesForNegativeInfinitySelector = mkSelector "setTextAttributesForNegativeInfinity:"

-- | @Selector@ for @positivePrefix@
positivePrefixSelector :: Selector '[] (Id NSString)
positivePrefixSelector = mkSelector "positivePrefix"

-- | @Selector@ for @setPositivePrefix:@
setPositivePrefixSelector :: Selector '[Id NSString] ()
setPositivePrefixSelector = mkSelector "setPositivePrefix:"

-- | @Selector@ for @positiveSuffix@
positiveSuffixSelector :: Selector '[] (Id NSString)
positiveSuffixSelector = mkSelector "positiveSuffix"

-- | @Selector@ for @setPositiveSuffix:@
setPositiveSuffixSelector :: Selector '[Id NSString] ()
setPositiveSuffixSelector = mkSelector "setPositiveSuffix:"

-- | @Selector@ for @negativePrefix@
negativePrefixSelector :: Selector '[] (Id NSString)
negativePrefixSelector = mkSelector "negativePrefix"

-- | @Selector@ for @setNegativePrefix:@
setNegativePrefixSelector :: Selector '[Id NSString] ()
setNegativePrefixSelector = mkSelector "setNegativePrefix:"

-- | @Selector@ for @negativeSuffix@
negativeSuffixSelector :: Selector '[] (Id NSString)
negativeSuffixSelector = mkSelector "negativeSuffix"

-- | @Selector@ for @setNegativeSuffix:@
setNegativeSuffixSelector :: Selector '[Id NSString] ()
setNegativeSuffixSelector = mkSelector "setNegativeSuffix:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector '[Id NSString] ()
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @currencySymbol@
currencySymbolSelector :: Selector '[] (Id NSString)
currencySymbolSelector = mkSelector "currencySymbol"

-- | @Selector@ for @setCurrencySymbol:@
setCurrencySymbolSelector :: Selector '[Id NSString] ()
setCurrencySymbolSelector = mkSelector "setCurrencySymbol:"

-- | @Selector@ for @internationalCurrencySymbol@
internationalCurrencySymbolSelector :: Selector '[] (Id NSString)
internationalCurrencySymbolSelector = mkSelector "internationalCurrencySymbol"

-- | @Selector@ for @setInternationalCurrencySymbol:@
setInternationalCurrencySymbolSelector :: Selector '[Id NSString] ()
setInternationalCurrencySymbolSelector = mkSelector "setInternationalCurrencySymbol:"

-- | @Selector@ for @percentSymbol@
percentSymbolSelector :: Selector '[] (Id NSString)
percentSymbolSelector = mkSelector "percentSymbol"

-- | @Selector@ for @setPercentSymbol:@
setPercentSymbolSelector :: Selector '[Id NSString] ()
setPercentSymbolSelector = mkSelector "setPercentSymbol:"

-- | @Selector@ for @perMillSymbol@
perMillSymbolSelector :: Selector '[] (Id NSString)
perMillSymbolSelector = mkSelector "perMillSymbol"

-- | @Selector@ for @setPerMillSymbol:@
setPerMillSymbolSelector :: Selector '[Id NSString] ()
setPerMillSymbolSelector = mkSelector "setPerMillSymbol:"

-- | @Selector@ for @minusSign@
minusSignSelector :: Selector '[] (Id NSString)
minusSignSelector = mkSelector "minusSign"

-- | @Selector@ for @setMinusSign:@
setMinusSignSelector :: Selector '[Id NSString] ()
setMinusSignSelector = mkSelector "setMinusSign:"

-- | @Selector@ for @plusSign@
plusSignSelector :: Selector '[] (Id NSString)
plusSignSelector = mkSelector "plusSign"

-- | @Selector@ for @setPlusSign:@
setPlusSignSelector :: Selector '[Id NSString] ()
setPlusSignSelector = mkSelector "setPlusSign:"

-- | @Selector@ for @exponentSymbol@
exponentSymbolSelector :: Selector '[] (Id NSString)
exponentSymbolSelector = mkSelector "exponentSymbol"

-- | @Selector@ for @setExponentSymbol:@
setExponentSymbolSelector :: Selector '[Id NSString] ()
setExponentSymbolSelector = mkSelector "setExponentSymbol:"

-- | @Selector@ for @groupingSize@
groupingSizeSelector :: Selector '[] CULong
groupingSizeSelector = mkSelector "groupingSize"

-- | @Selector@ for @setGroupingSize:@
setGroupingSizeSelector :: Selector '[CULong] ()
setGroupingSizeSelector = mkSelector "setGroupingSize:"

-- | @Selector@ for @secondaryGroupingSize@
secondaryGroupingSizeSelector :: Selector '[] CULong
secondaryGroupingSizeSelector = mkSelector "secondaryGroupingSize"

-- | @Selector@ for @setSecondaryGroupingSize:@
setSecondaryGroupingSizeSelector :: Selector '[CULong] ()
setSecondaryGroupingSizeSelector = mkSelector "setSecondaryGroupingSize:"

-- | @Selector@ for @multiplier@
multiplierSelector :: Selector '[] (Id NSNumber)
multiplierSelector = mkSelector "multiplier"

-- | @Selector@ for @setMultiplier:@
setMultiplierSelector :: Selector '[Id NSNumber] ()
setMultiplierSelector = mkSelector "setMultiplier:"

-- | @Selector@ for @formatWidth@
formatWidthSelector :: Selector '[] CULong
formatWidthSelector = mkSelector "formatWidth"

-- | @Selector@ for @setFormatWidth:@
setFormatWidthSelector :: Selector '[CULong] ()
setFormatWidthSelector = mkSelector "setFormatWidth:"

-- | @Selector@ for @paddingCharacter@
paddingCharacterSelector :: Selector '[] (Id NSString)
paddingCharacterSelector = mkSelector "paddingCharacter"

-- | @Selector@ for @setPaddingCharacter:@
setPaddingCharacterSelector :: Selector '[Id NSString] ()
setPaddingCharacterSelector = mkSelector "setPaddingCharacter:"

-- | @Selector@ for @paddingPosition@
paddingPositionSelector :: Selector '[] NSNumberFormatterPadPosition
paddingPositionSelector = mkSelector "paddingPosition"

-- | @Selector@ for @setPaddingPosition:@
setPaddingPositionSelector :: Selector '[NSNumberFormatterPadPosition] ()
setPaddingPositionSelector = mkSelector "setPaddingPosition:"

-- | @Selector@ for @roundingMode@
roundingModeSelector :: Selector '[] NSNumberFormatterRoundingMode
roundingModeSelector = mkSelector "roundingMode"

-- | @Selector@ for @setRoundingMode:@
setRoundingModeSelector :: Selector '[NSNumberFormatterRoundingMode] ()
setRoundingModeSelector = mkSelector "setRoundingMode:"

-- | @Selector@ for @roundingIncrement@
roundingIncrementSelector :: Selector '[] (Id NSNumber)
roundingIncrementSelector = mkSelector "roundingIncrement"

-- | @Selector@ for @setRoundingIncrement:@
setRoundingIncrementSelector :: Selector '[Id NSNumber] ()
setRoundingIncrementSelector = mkSelector "setRoundingIncrement:"

-- | @Selector@ for @minimumIntegerDigits@
minimumIntegerDigitsSelector :: Selector '[] CULong
minimumIntegerDigitsSelector = mkSelector "minimumIntegerDigits"

-- | @Selector@ for @setMinimumIntegerDigits:@
setMinimumIntegerDigitsSelector :: Selector '[CULong] ()
setMinimumIntegerDigitsSelector = mkSelector "setMinimumIntegerDigits:"

-- | @Selector@ for @maximumIntegerDigits@
maximumIntegerDigitsSelector :: Selector '[] CULong
maximumIntegerDigitsSelector = mkSelector "maximumIntegerDigits"

-- | @Selector@ for @setMaximumIntegerDigits:@
setMaximumIntegerDigitsSelector :: Selector '[CULong] ()
setMaximumIntegerDigitsSelector = mkSelector "setMaximumIntegerDigits:"

-- | @Selector@ for @minimumFractionDigits@
minimumFractionDigitsSelector :: Selector '[] CULong
minimumFractionDigitsSelector = mkSelector "minimumFractionDigits"

-- | @Selector@ for @setMinimumFractionDigits:@
setMinimumFractionDigitsSelector :: Selector '[CULong] ()
setMinimumFractionDigitsSelector = mkSelector "setMinimumFractionDigits:"

-- | @Selector@ for @maximumFractionDigits@
maximumFractionDigitsSelector :: Selector '[] CULong
maximumFractionDigitsSelector = mkSelector "maximumFractionDigits"

-- | @Selector@ for @setMaximumFractionDigits:@
setMaximumFractionDigitsSelector :: Selector '[CULong] ()
setMaximumFractionDigitsSelector = mkSelector "setMaximumFractionDigits:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector '[] (Id NSNumber)
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @setMinimum:@
setMinimumSelector :: Selector '[Id NSNumber] ()
setMinimumSelector = mkSelector "setMinimum:"

-- | @Selector@ for @maximum@
maximumSelector :: Selector '[] (Id NSNumber)
maximumSelector = mkSelector "maximum"

-- | @Selector@ for @setMaximum:@
setMaximumSelector :: Selector '[Id NSNumber] ()
setMaximumSelector = mkSelector "setMaximum:"

-- | @Selector@ for @currencyGroupingSeparator@
currencyGroupingSeparatorSelector :: Selector '[] (Id NSString)
currencyGroupingSeparatorSelector = mkSelector "currencyGroupingSeparator"

-- | @Selector@ for @setCurrencyGroupingSeparator:@
setCurrencyGroupingSeparatorSelector :: Selector '[Id NSString] ()
setCurrencyGroupingSeparatorSelector = mkSelector "setCurrencyGroupingSeparator:"

-- | @Selector@ for @lenient@
lenientSelector :: Selector '[] Bool
lenientSelector = mkSelector "lenient"

-- | @Selector@ for @setLenient:@
setLenientSelector :: Selector '[Bool] ()
setLenientSelector = mkSelector "setLenient:"

-- | @Selector@ for @usesSignificantDigits@
usesSignificantDigitsSelector :: Selector '[] Bool
usesSignificantDigitsSelector = mkSelector "usesSignificantDigits"

-- | @Selector@ for @setUsesSignificantDigits:@
setUsesSignificantDigitsSelector :: Selector '[Bool] ()
setUsesSignificantDigitsSelector = mkSelector "setUsesSignificantDigits:"

-- | @Selector@ for @minimumSignificantDigits@
minimumSignificantDigitsSelector :: Selector '[] CULong
minimumSignificantDigitsSelector = mkSelector "minimumSignificantDigits"

-- | @Selector@ for @setMinimumSignificantDigits:@
setMinimumSignificantDigitsSelector :: Selector '[CULong] ()
setMinimumSignificantDigitsSelector = mkSelector "setMinimumSignificantDigits:"

-- | @Selector@ for @maximumSignificantDigits@
maximumSignificantDigitsSelector :: Selector '[] CULong
maximumSignificantDigitsSelector = mkSelector "maximumSignificantDigits"

-- | @Selector@ for @setMaximumSignificantDigits:@
setMaximumSignificantDigitsSelector :: Selector '[CULong] ()
setMaximumSignificantDigitsSelector = mkSelector "setMaximumSignificantDigits:"

-- | @Selector@ for @partialStringValidationEnabled@
partialStringValidationEnabledSelector :: Selector '[] Bool
partialStringValidationEnabledSelector = mkSelector "partialStringValidationEnabled"

-- | @Selector@ for @setPartialStringValidationEnabled:@
setPartialStringValidationEnabledSelector :: Selector '[Bool] ()
setPartialStringValidationEnabledSelector = mkSelector "setPartialStringValidationEnabled:"

-- | @Selector@ for @hasThousandSeparators@
hasThousandSeparatorsSelector :: Selector '[] Bool
hasThousandSeparatorsSelector = mkSelector "hasThousandSeparators"

-- | @Selector@ for @setHasThousandSeparators:@
setHasThousandSeparatorsSelector :: Selector '[Bool] ()
setHasThousandSeparatorsSelector = mkSelector "setHasThousandSeparators:"

-- | @Selector@ for @thousandSeparator@
thousandSeparatorSelector :: Selector '[] (Id NSString)
thousandSeparatorSelector = mkSelector "thousandSeparator"

-- | @Selector@ for @setThousandSeparator:@
setThousandSeparatorSelector :: Selector '[Id NSString] ()
setThousandSeparatorSelector = mkSelector "setThousandSeparator:"

-- | @Selector@ for @localizesFormat@
localizesFormatSelector :: Selector '[] Bool
localizesFormatSelector = mkSelector "localizesFormat"

-- | @Selector@ for @setLocalizesFormat:@
setLocalizesFormatSelector :: Selector '[Bool] ()
setLocalizesFormatSelector = mkSelector "setLocalizesFormat:"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id NSString)
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[Id NSString] ()
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @attributedStringForZero@
attributedStringForZeroSelector :: Selector '[] (Id NSAttributedString)
attributedStringForZeroSelector = mkSelector "attributedStringForZero"

-- | @Selector@ for @setAttributedStringForZero:@
setAttributedStringForZeroSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringForZeroSelector = mkSelector "setAttributedStringForZero:"

-- | @Selector@ for @attributedStringForNil@
attributedStringForNilSelector :: Selector '[] (Id NSAttributedString)
attributedStringForNilSelector = mkSelector "attributedStringForNil"

-- | @Selector@ for @setAttributedStringForNil:@
setAttributedStringForNilSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringForNilSelector = mkSelector "setAttributedStringForNil:"

-- | @Selector@ for @attributedStringForNotANumber@
attributedStringForNotANumberSelector :: Selector '[] (Id NSAttributedString)
attributedStringForNotANumberSelector = mkSelector "attributedStringForNotANumber"

-- | @Selector@ for @setAttributedStringForNotANumber:@
setAttributedStringForNotANumberSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringForNotANumberSelector = mkSelector "setAttributedStringForNotANumber:"

-- | @Selector@ for @roundingBehavior@
roundingBehaviorSelector :: Selector '[] (Id NSDecimalNumberHandler)
roundingBehaviorSelector = mkSelector "roundingBehavior"

-- | @Selector@ for @setRoundingBehavior:@
setRoundingBehaviorSelector :: Selector '[Id NSDecimalNumberHandler] ()
setRoundingBehaviorSelector = mkSelector "setRoundingBehavior:"

