{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLocale@.
module ObjC.Foundation.NSLocale
  ( NSLocale
  , IsNSLocale(..)
  , objectForKey
  , displayNameForKey_value
  , initWithLocaleIdentifier
  , initWithCoder
  , componentsFromLocaleIdentifier
  , localeIdentifierFromComponents
  , canonicalLocaleIdentifierFromString
  , canonicalLanguageIdentifierFromString
  , localeIdentifierFromWindowsLocaleCode
  , windowsLocaleCodeFromLocaleIdentifier
  , characterDirectionForLanguage
  , lineDirectionForLanguage
  , localeWithLocaleIdentifier
  , init_
  , localizedStringForLocaleIdentifier
  , localizedStringForLanguageCode
  , localizedStringForCountryCode
  , localizedStringForScriptCode
  , localizedStringForVariantCode
  , localizedStringForCalendarIdentifier
  , localizedStringForCollationIdentifier
  , localizedStringForCurrencyCode
  , localizedStringForCollatorIdentifier
  , availableLocaleIdentifiers
  , isoLanguageCodes
  , isoCountryCodes
  , isoCurrencyCodes
  , commonISOCurrencyCodes
  , preferredLanguages
  , autoupdatingCurrentLocale
  , currentLocale
  , systemLocale
  , localeIdentifier
  , languageCode
  , languageIdentifier
  , countryCode
  , regionCode
  , scriptCode
  , variantCode
  , exemplarCharacterSet
  , calendarIdentifier
  , collationIdentifier
  , usesMetricSystem
  , decimalSeparator
  , groupingSeparator
  , currencySymbol
  , currencyCode
  , collatorIdentifier
  , quotationBeginDelimiter
  , quotationEndDelimiter
  , alternateQuotationBeginDelimiter
  , alternateQuotationEndDelimiter
  , alternateQuotationBeginDelimiterSelector
  , alternateQuotationEndDelimiterSelector
  , autoupdatingCurrentLocaleSelector
  , availableLocaleIdentifiersSelector
  , calendarIdentifierSelector
  , canonicalLanguageIdentifierFromStringSelector
  , canonicalLocaleIdentifierFromStringSelector
  , characterDirectionForLanguageSelector
  , collationIdentifierSelector
  , collatorIdentifierSelector
  , commonISOCurrencyCodesSelector
  , componentsFromLocaleIdentifierSelector
  , countryCodeSelector
  , currencyCodeSelector
  , currencySymbolSelector
  , currentLocaleSelector
  , decimalSeparatorSelector
  , displayNameForKey_valueSelector
  , exemplarCharacterSetSelector
  , groupingSeparatorSelector
  , initSelector
  , initWithCoderSelector
  , initWithLocaleIdentifierSelector
  , isoCountryCodesSelector
  , isoCurrencyCodesSelector
  , isoLanguageCodesSelector
  , languageCodeSelector
  , languageIdentifierSelector
  , lineDirectionForLanguageSelector
  , localeIdentifierFromComponentsSelector
  , localeIdentifierFromWindowsLocaleCodeSelector
  , localeIdentifierSelector
  , localeWithLocaleIdentifierSelector
  , localizedStringForCalendarIdentifierSelector
  , localizedStringForCollationIdentifierSelector
  , localizedStringForCollatorIdentifierSelector
  , localizedStringForCountryCodeSelector
  , localizedStringForCurrencyCodeSelector
  , localizedStringForLanguageCodeSelector
  , localizedStringForLocaleIdentifierSelector
  , localizedStringForScriptCodeSelector
  , localizedStringForVariantCodeSelector
  , objectForKeySelector
  , preferredLanguagesSelector
  , quotationBeginDelimiterSelector
  , quotationEndDelimiterSelector
  , regionCodeSelector
  , scriptCodeSelector
  , systemLocaleSelector
  , usesMetricSystemSelector
  , variantCodeSelector
  , windowsLocaleCodeFromLocaleIdentifierSelector

  -- * Enum types
  , NSLocaleLanguageDirection(NSLocaleLanguageDirection)
  , pattern NSLocaleLanguageDirectionUnknown
  , pattern NSLocaleLanguageDirectionLeftToRight
  , pattern NSLocaleLanguageDirectionRightToLeft
  , pattern NSLocaleLanguageDirectionTopToBottom
  , pattern NSLocaleLanguageDirectionBottomToTop

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- objectForKey:@
objectForKey :: (IsNSLocale nsLocale, IsNSString key) => nsLocale -> key -> IO RawId
objectForKey nsLocale key =
  sendMessage nsLocale objectForKeySelector (toNSString key)

-- | @- displayNameForKey:value:@
displayNameForKey_value :: (IsNSLocale nsLocale, IsNSString key) => nsLocale -> key -> RawId -> IO (Id NSString)
displayNameForKey_value nsLocale key value =
  sendMessage nsLocale displayNameForKey_valueSelector (toNSString key) value

-- | @- initWithLocaleIdentifier:@
initWithLocaleIdentifier :: (IsNSLocale nsLocale, IsNSString string) => nsLocale -> string -> IO (Id NSLocale)
initWithLocaleIdentifier nsLocale string =
  sendOwnedMessage nsLocale initWithLocaleIdentifierSelector (toNSString string)

-- | @- initWithCoder:@
initWithCoder :: (IsNSLocale nsLocale, IsNSCoder coder) => nsLocale -> coder -> IO (Id NSLocale)
initWithCoder nsLocale coder =
  sendOwnedMessage nsLocale initWithCoderSelector (toNSCoder coder)

-- | @+ componentsFromLocaleIdentifier:@
componentsFromLocaleIdentifier :: IsNSString string => string -> IO (Id NSDictionary)
componentsFromLocaleIdentifier string =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' componentsFromLocaleIdentifierSelector (toNSString string)

-- | @+ localeIdentifierFromComponents:@
localeIdentifierFromComponents :: IsNSDictionary dict => dict -> IO (Id NSString)
localeIdentifierFromComponents dict =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' localeIdentifierFromComponentsSelector (toNSDictionary dict)

-- | @+ canonicalLocaleIdentifierFromString:@
canonicalLocaleIdentifierFromString :: IsNSString string => string -> IO (Id NSString)
canonicalLocaleIdentifierFromString string =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' canonicalLocaleIdentifierFromStringSelector (toNSString string)

-- | @+ canonicalLanguageIdentifierFromString:@
canonicalLanguageIdentifierFromString :: IsNSString string => string -> IO (Id NSString)
canonicalLanguageIdentifierFromString string =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' canonicalLanguageIdentifierFromStringSelector (toNSString string)

-- | @+ localeIdentifierFromWindowsLocaleCode:@
localeIdentifierFromWindowsLocaleCode :: CUInt -> IO (Id NSString)
localeIdentifierFromWindowsLocaleCode lcid =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' localeIdentifierFromWindowsLocaleCodeSelector lcid

-- | @+ windowsLocaleCodeFromLocaleIdentifier:@
windowsLocaleCodeFromLocaleIdentifier :: IsNSString localeIdentifier => localeIdentifier -> IO CUInt
windowsLocaleCodeFromLocaleIdentifier localeIdentifier =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' windowsLocaleCodeFromLocaleIdentifierSelector (toNSString localeIdentifier)

-- | @+ characterDirectionForLanguage:@
characterDirectionForLanguage :: IsNSString isoLangCode => isoLangCode -> IO NSLocaleLanguageDirection
characterDirectionForLanguage isoLangCode =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' characterDirectionForLanguageSelector (toNSString isoLangCode)

-- | @+ lineDirectionForLanguage:@
lineDirectionForLanguage :: IsNSString isoLangCode => isoLangCode -> IO NSLocaleLanguageDirection
lineDirectionForLanguage isoLangCode =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' lineDirectionForLanguageSelector (toNSString isoLangCode)

-- | @+ localeWithLocaleIdentifier:@
localeWithLocaleIdentifier :: IsNSString ident => ident -> IO (Id NSLocale)
localeWithLocaleIdentifier ident =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' localeWithLocaleIdentifierSelector (toNSString ident)

-- | @- init@
init_ :: IsNSLocale nsLocale => nsLocale -> IO (Id NSLocale)
init_ nsLocale =
  sendOwnedMessage nsLocale initSelector

-- | @- localizedStringForLocaleIdentifier:@
localizedStringForLocaleIdentifier :: (IsNSLocale nsLocale, IsNSString localeIdentifier) => nsLocale -> localeIdentifier -> IO (Id NSString)
localizedStringForLocaleIdentifier nsLocale localeIdentifier =
  sendMessage nsLocale localizedStringForLocaleIdentifierSelector (toNSString localeIdentifier)

-- | @- localizedStringForLanguageCode:@
localizedStringForLanguageCode :: (IsNSLocale nsLocale, IsNSString languageCode) => nsLocale -> languageCode -> IO (Id NSString)
localizedStringForLanguageCode nsLocale languageCode =
  sendMessage nsLocale localizedStringForLanguageCodeSelector (toNSString languageCode)

-- | @- localizedStringForCountryCode:@
localizedStringForCountryCode :: (IsNSLocale nsLocale, IsNSString countryCode) => nsLocale -> countryCode -> IO (Id NSString)
localizedStringForCountryCode nsLocale countryCode =
  sendMessage nsLocale localizedStringForCountryCodeSelector (toNSString countryCode)

-- | @- localizedStringForScriptCode:@
localizedStringForScriptCode :: (IsNSLocale nsLocale, IsNSString scriptCode) => nsLocale -> scriptCode -> IO (Id NSString)
localizedStringForScriptCode nsLocale scriptCode =
  sendMessage nsLocale localizedStringForScriptCodeSelector (toNSString scriptCode)

-- | @- localizedStringForVariantCode:@
localizedStringForVariantCode :: (IsNSLocale nsLocale, IsNSString variantCode) => nsLocale -> variantCode -> IO (Id NSString)
localizedStringForVariantCode nsLocale variantCode =
  sendMessage nsLocale localizedStringForVariantCodeSelector (toNSString variantCode)

-- | @- localizedStringForCalendarIdentifier:@
localizedStringForCalendarIdentifier :: (IsNSLocale nsLocale, IsNSString calendarIdentifier) => nsLocale -> calendarIdentifier -> IO (Id NSString)
localizedStringForCalendarIdentifier nsLocale calendarIdentifier =
  sendMessage nsLocale localizedStringForCalendarIdentifierSelector (toNSString calendarIdentifier)

-- | @- localizedStringForCollationIdentifier:@
localizedStringForCollationIdentifier :: (IsNSLocale nsLocale, IsNSString collationIdentifier) => nsLocale -> collationIdentifier -> IO (Id NSString)
localizedStringForCollationIdentifier nsLocale collationIdentifier =
  sendMessage nsLocale localizedStringForCollationIdentifierSelector (toNSString collationIdentifier)

-- | @- localizedStringForCurrencyCode:@
localizedStringForCurrencyCode :: (IsNSLocale nsLocale, IsNSString currencyCode) => nsLocale -> currencyCode -> IO (Id NSString)
localizedStringForCurrencyCode nsLocale currencyCode =
  sendMessage nsLocale localizedStringForCurrencyCodeSelector (toNSString currencyCode)

-- | @- localizedStringForCollatorIdentifier:@
localizedStringForCollatorIdentifier :: (IsNSLocale nsLocale, IsNSString collatorIdentifier) => nsLocale -> collatorIdentifier -> IO (Id NSString)
localizedStringForCollatorIdentifier nsLocale collatorIdentifier =
  sendMessage nsLocale localizedStringForCollatorIdentifierSelector (toNSString collatorIdentifier)

-- | @+ availableLocaleIdentifiers@
availableLocaleIdentifiers :: IO (Id NSArray)
availableLocaleIdentifiers  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' availableLocaleIdentifiersSelector

-- | @+ ISOLanguageCodes@
isoLanguageCodes :: IO (Id NSArray)
isoLanguageCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' isoLanguageCodesSelector

-- | @+ ISOCountryCodes@
isoCountryCodes :: IO (Id NSArray)
isoCountryCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' isoCountryCodesSelector

-- | @+ ISOCurrencyCodes@
isoCurrencyCodes :: IO (Id NSArray)
isoCurrencyCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' isoCurrencyCodesSelector

-- | @+ commonISOCurrencyCodes@
commonISOCurrencyCodes :: IO (Id NSArray)
commonISOCurrencyCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' commonISOCurrencyCodesSelector

-- | @+ preferredLanguages@
preferredLanguages :: IO (Id NSArray)
preferredLanguages  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' preferredLanguagesSelector

-- | @+ autoupdatingCurrentLocale@
autoupdatingCurrentLocale :: IO (Id NSLocale)
autoupdatingCurrentLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' autoupdatingCurrentLocaleSelector

-- | @+ currentLocale@
currentLocale :: IO (Id NSLocale)
currentLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' currentLocaleSelector

-- | @+ systemLocale@
systemLocale :: IO (Id NSLocale)
systemLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMessage cls' systemLocaleSelector

-- | @- localeIdentifier@
localeIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
localeIdentifier nsLocale =
  sendMessage nsLocale localeIdentifierSelector

-- | @- languageCode@
languageCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
languageCode nsLocale =
  sendMessage nsLocale languageCodeSelector

-- | Returns the identifier for the language part of the locale. For example, returns "en-US" for "en_US\@rg=gbzzzz"  locale.
--
-- ObjC selector: @- languageIdentifier@
languageIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
languageIdentifier nsLocale =
  sendMessage nsLocale languageIdentifierSelector

-- | @- countryCode@
countryCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
countryCode nsLocale =
  sendMessage nsLocale countryCodeSelector

-- | Returns the region code of the locale. If the @rg@ subtag is present, the value of the subtag will be used. For example,  returns "GB" for "en_US\@rg=gbzzzz" locale. If the @localeIdentifier@ doesn’t contain a region, returns @nil@.
--
-- ObjC selector: @- regionCode@
regionCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
regionCode nsLocale =
  sendMessage nsLocale regionCodeSelector

-- | @- scriptCode@
scriptCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
scriptCode nsLocale =
  sendMessage nsLocale scriptCodeSelector

-- | @- variantCode@
variantCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
variantCode nsLocale =
  sendMessage nsLocale variantCodeSelector

-- | @- exemplarCharacterSet@
exemplarCharacterSet :: IsNSLocale nsLocale => nsLocale -> IO (Id NSCharacterSet)
exemplarCharacterSet nsLocale =
  sendMessage nsLocale exemplarCharacterSetSelector

-- | @- calendarIdentifier@
calendarIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
calendarIdentifier nsLocale =
  sendMessage nsLocale calendarIdentifierSelector

-- | @- collationIdentifier@
collationIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
collationIdentifier nsLocale =
  sendMessage nsLocale collationIdentifierSelector

-- | @- usesMetricSystem@
usesMetricSystem :: IsNSLocale nsLocale => nsLocale -> IO Bool
usesMetricSystem nsLocale =
  sendMessage nsLocale usesMetricSystemSelector

-- | @- decimalSeparator@
decimalSeparator :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
decimalSeparator nsLocale =
  sendMessage nsLocale decimalSeparatorSelector

-- | @- groupingSeparator@
groupingSeparator :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
groupingSeparator nsLocale =
  sendMessage nsLocale groupingSeparatorSelector

-- | @- currencySymbol@
currencySymbol :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
currencySymbol nsLocale =
  sendMessage nsLocale currencySymbolSelector

-- | @- currencyCode@
currencyCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
currencyCode nsLocale =
  sendMessage nsLocale currencyCodeSelector

-- | @- collatorIdentifier@
collatorIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
collatorIdentifier nsLocale =
  sendMessage nsLocale collatorIdentifierSelector

-- | @- quotationBeginDelimiter@
quotationBeginDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
quotationBeginDelimiter nsLocale =
  sendMessage nsLocale quotationBeginDelimiterSelector

-- | @- quotationEndDelimiter@
quotationEndDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
quotationEndDelimiter nsLocale =
  sendMessage nsLocale quotationEndDelimiterSelector

-- | @- alternateQuotationBeginDelimiter@
alternateQuotationBeginDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
alternateQuotationBeginDelimiter nsLocale =
  sendMessage nsLocale alternateQuotationBeginDelimiterSelector

-- | @- alternateQuotationEndDelimiter@
alternateQuotationEndDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
alternateQuotationEndDelimiter nsLocale =
  sendMessage nsLocale alternateQuotationEndDelimiterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[Id NSString] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @displayNameForKey:value:@
displayNameForKey_valueSelector :: Selector '[Id NSString, RawId] (Id NSString)
displayNameForKey_valueSelector = mkSelector "displayNameForKey:value:"

-- | @Selector@ for @initWithLocaleIdentifier:@
initWithLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSLocale)
initWithLocaleIdentifierSelector = mkSelector "initWithLocaleIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSLocale)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @componentsFromLocaleIdentifier:@
componentsFromLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSDictionary)
componentsFromLocaleIdentifierSelector = mkSelector "componentsFromLocaleIdentifier:"

-- | @Selector@ for @localeIdentifierFromComponents:@
localeIdentifierFromComponentsSelector :: Selector '[Id NSDictionary] (Id NSString)
localeIdentifierFromComponentsSelector = mkSelector "localeIdentifierFromComponents:"

-- | @Selector@ for @canonicalLocaleIdentifierFromString:@
canonicalLocaleIdentifierFromStringSelector :: Selector '[Id NSString] (Id NSString)
canonicalLocaleIdentifierFromStringSelector = mkSelector "canonicalLocaleIdentifierFromString:"

-- | @Selector@ for @canonicalLanguageIdentifierFromString:@
canonicalLanguageIdentifierFromStringSelector :: Selector '[Id NSString] (Id NSString)
canonicalLanguageIdentifierFromStringSelector = mkSelector "canonicalLanguageIdentifierFromString:"

-- | @Selector@ for @localeIdentifierFromWindowsLocaleCode:@
localeIdentifierFromWindowsLocaleCodeSelector :: Selector '[CUInt] (Id NSString)
localeIdentifierFromWindowsLocaleCodeSelector = mkSelector "localeIdentifierFromWindowsLocaleCode:"

-- | @Selector@ for @windowsLocaleCodeFromLocaleIdentifier:@
windowsLocaleCodeFromLocaleIdentifierSelector :: Selector '[Id NSString] CUInt
windowsLocaleCodeFromLocaleIdentifierSelector = mkSelector "windowsLocaleCodeFromLocaleIdentifier:"

-- | @Selector@ for @characterDirectionForLanguage:@
characterDirectionForLanguageSelector :: Selector '[Id NSString] NSLocaleLanguageDirection
characterDirectionForLanguageSelector = mkSelector "characterDirectionForLanguage:"

-- | @Selector@ for @lineDirectionForLanguage:@
lineDirectionForLanguageSelector :: Selector '[Id NSString] NSLocaleLanguageDirection
lineDirectionForLanguageSelector = mkSelector "lineDirectionForLanguage:"

-- | @Selector@ for @localeWithLocaleIdentifier:@
localeWithLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSLocale)
localeWithLocaleIdentifierSelector = mkSelector "localeWithLocaleIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSLocale)
initSelector = mkSelector "init"

-- | @Selector@ for @localizedStringForLocaleIdentifier:@
localizedStringForLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForLocaleIdentifierSelector = mkSelector "localizedStringForLocaleIdentifier:"

-- | @Selector@ for @localizedStringForLanguageCode:@
localizedStringForLanguageCodeSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForLanguageCodeSelector = mkSelector "localizedStringForLanguageCode:"

-- | @Selector@ for @localizedStringForCountryCode:@
localizedStringForCountryCodeSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForCountryCodeSelector = mkSelector "localizedStringForCountryCode:"

-- | @Selector@ for @localizedStringForScriptCode:@
localizedStringForScriptCodeSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForScriptCodeSelector = mkSelector "localizedStringForScriptCode:"

-- | @Selector@ for @localizedStringForVariantCode:@
localizedStringForVariantCodeSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForVariantCodeSelector = mkSelector "localizedStringForVariantCode:"

-- | @Selector@ for @localizedStringForCalendarIdentifier:@
localizedStringForCalendarIdentifierSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForCalendarIdentifierSelector = mkSelector "localizedStringForCalendarIdentifier:"

-- | @Selector@ for @localizedStringForCollationIdentifier:@
localizedStringForCollationIdentifierSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForCollationIdentifierSelector = mkSelector "localizedStringForCollationIdentifier:"

-- | @Selector@ for @localizedStringForCurrencyCode:@
localizedStringForCurrencyCodeSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForCurrencyCodeSelector = mkSelector "localizedStringForCurrencyCode:"

-- | @Selector@ for @localizedStringForCollatorIdentifier:@
localizedStringForCollatorIdentifierSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForCollatorIdentifierSelector = mkSelector "localizedStringForCollatorIdentifier:"

-- | @Selector@ for @availableLocaleIdentifiers@
availableLocaleIdentifiersSelector :: Selector '[] (Id NSArray)
availableLocaleIdentifiersSelector = mkSelector "availableLocaleIdentifiers"

-- | @Selector@ for @ISOLanguageCodes@
isoLanguageCodesSelector :: Selector '[] (Id NSArray)
isoLanguageCodesSelector = mkSelector "ISOLanguageCodes"

-- | @Selector@ for @ISOCountryCodes@
isoCountryCodesSelector :: Selector '[] (Id NSArray)
isoCountryCodesSelector = mkSelector "ISOCountryCodes"

-- | @Selector@ for @ISOCurrencyCodes@
isoCurrencyCodesSelector :: Selector '[] (Id NSArray)
isoCurrencyCodesSelector = mkSelector "ISOCurrencyCodes"

-- | @Selector@ for @commonISOCurrencyCodes@
commonISOCurrencyCodesSelector :: Selector '[] (Id NSArray)
commonISOCurrencyCodesSelector = mkSelector "commonISOCurrencyCodes"

-- | @Selector@ for @preferredLanguages@
preferredLanguagesSelector :: Selector '[] (Id NSArray)
preferredLanguagesSelector = mkSelector "preferredLanguages"

-- | @Selector@ for @autoupdatingCurrentLocale@
autoupdatingCurrentLocaleSelector :: Selector '[] (Id NSLocale)
autoupdatingCurrentLocaleSelector = mkSelector "autoupdatingCurrentLocale"

-- | @Selector@ for @currentLocale@
currentLocaleSelector :: Selector '[] (Id NSLocale)
currentLocaleSelector = mkSelector "currentLocale"

-- | @Selector@ for @systemLocale@
systemLocaleSelector :: Selector '[] (Id NSLocale)
systemLocaleSelector = mkSelector "systemLocale"

-- | @Selector@ for @localeIdentifier@
localeIdentifierSelector :: Selector '[] (Id NSString)
localeIdentifierSelector = mkSelector "localeIdentifier"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @languageIdentifier@
languageIdentifierSelector :: Selector '[] (Id NSString)
languageIdentifierSelector = mkSelector "languageIdentifier"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector '[] (Id NSString)
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @scriptCode@
scriptCodeSelector :: Selector '[] (Id NSString)
scriptCodeSelector = mkSelector "scriptCode"

-- | @Selector@ for @variantCode@
variantCodeSelector :: Selector '[] (Id NSString)
variantCodeSelector = mkSelector "variantCode"

-- | @Selector@ for @exemplarCharacterSet@
exemplarCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
exemplarCharacterSetSelector = mkSelector "exemplarCharacterSet"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector '[] (Id NSString)
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @collationIdentifier@
collationIdentifierSelector :: Selector '[] (Id NSString)
collationIdentifierSelector = mkSelector "collationIdentifier"

-- | @Selector@ for @usesMetricSystem@
usesMetricSystemSelector :: Selector '[] Bool
usesMetricSystemSelector = mkSelector "usesMetricSystem"

-- | @Selector@ for @decimalSeparator@
decimalSeparatorSelector :: Selector '[] (Id NSString)
decimalSeparatorSelector = mkSelector "decimalSeparator"

-- | @Selector@ for @groupingSeparator@
groupingSeparatorSelector :: Selector '[] (Id NSString)
groupingSeparatorSelector = mkSelector "groupingSeparator"

-- | @Selector@ for @currencySymbol@
currencySymbolSelector :: Selector '[] (Id NSString)
currencySymbolSelector = mkSelector "currencySymbol"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @collatorIdentifier@
collatorIdentifierSelector :: Selector '[] (Id NSString)
collatorIdentifierSelector = mkSelector "collatorIdentifier"

-- | @Selector@ for @quotationBeginDelimiter@
quotationBeginDelimiterSelector :: Selector '[] (Id NSString)
quotationBeginDelimiterSelector = mkSelector "quotationBeginDelimiter"

-- | @Selector@ for @quotationEndDelimiter@
quotationEndDelimiterSelector :: Selector '[] (Id NSString)
quotationEndDelimiterSelector = mkSelector "quotationEndDelimiter"

-- | @Selector@ for @alternateQuotationBeginDelimiter@
alternateQuotationBeginDelimiterSelector :: Selector '[] (Id NSString)
alternateQuotationBeginDelimiterSelector = mkSelector "alternateQuotationBeginDelimiter"

-- | @Selector@ for @alternateQuotationEndDelimiter@
alternateQuotationEndDelimiterSelector :: Selector '[] (Id NSString)
alternateQuotationEndDelimiterSelector = mkSelector "alternateQuotationEndDelimiter"

