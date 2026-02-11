{-# LANGUAGE PatternSynonyms #-}
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
  , objectForKeySelector
  , displayNameForKey_valueSelector
  , initWithLocaleIdentifierSelector
  , initWithCoderSelector
  , componentsFromLocaleIdentifierSelector
  , localeIdentifierFromComponentsSelector
  , canonicalLocaleIdentifierFromStringSelector
  , canonicalLanguageIdentifierFromStringSelector
  , localeIdentifierFromWindowsLocaleCodeSelector
  , windowsLocaleCodeFromLocaleIdentifierSelector
  , characterDirectionForLanguageSelector
  , lineDirectionForLanguageSelector
  , localeWithLocaleIdentifierSelector
  , initSelector
  , localizedStringForLocaleIdentifierSelector
  , localizedStringForLanguageCodeSelector
  , localizedStringForCountryCodeSelector
  , localizedStringForScriptCodeSelector
  , localizedStringForVariantCodeSelector
  , localizedStringForCalendarIdentifierSelector
  , localizedStringForCollationIdentifierSelector
  , localizedStringForCurrencyCodeSelector
  , localizedStringForCollatorIdentifierSelector
  , availableLocaleIdentifiersSelector
  , isoLanguageCodesSelector
  , isoCountryCodesSelector
  , isoCurrencyCodesSelector
  , commonISOCurrencyCodesSelector
  , preferredLanguagesSelector
  , autoupdatingCurrentLocaleSelector
  , currentLocaleSelector
  , systemLocaleSelector
  , localeIdentifierSelector
  , languageCodeSelector
  , languageIdentifierSelector
  , countryCodeSelector
  , regionCodeSelector
  , scriptCodeSelector
  , variantCodeSelector
  , exemplarCharacterSetSelector
  , calendarIdentifierSelector
  , collationIdentifierSelector
  , usesMetricSystemSelector
  , decimalSeparatorSelector
  , groupingSeparatorSelector
  , currencySymbolSelector
  , currencyCodeSelector
  , collatorIdentifierSelector
  , quotationBeginDelimiterSelector
  , quotationEndDelimiterSelector
  , alternateQuotationBeginDelimiterSelector
  , alternateQuotationEndDelimiterSelector

  -- * Enum types
  , NSLocaleLanguageDirection(NSLocaleLanguageDirection)
  , pattern NSLocaleLanguageDirectionUnknown
  , pattern NSLocaleLanguageDirectionLeftToRight
  , pattern NSLocaleLanguageDirectionRightToLeft
  , pattern NSLocaleLanguageDirectionTopToBottom
  , pattern NSLocaleLanguageDirectionBottomToTop

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

-- | @- objectForKey:@
objectForKey :: (IsNSLocale nsLocale, IsNSString key) => nsLocale -> key -> IO RawId
objectForKey nsLocale  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsLocale (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- displayNameForKey:value:@
displayNameForKey_value :: (IsNSLocale nsLocale, IsNSString key) => nsLocale -> key -> RawId -> IO (Id NSString)
displayNameForKey_value nsLocale  key value =
withObjCPtr key $ \raw_key ->
    sendMsg nsLocale (mkSelector "displayNameForKey:value:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithLocaleIdentifier:@
initWithLocaleIdentifier :: (IsNSLocale nsLocale, IsNSString string) => nsLocale -> string -> IO (Id NSLocale)
initWithLocaleIdentifier nsLocale  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsLocale (mkSelector "initWithLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSLocale nsLocale, IsNSCoder coder) => nsLocale -> coder -> IO (Id NSLocale)
initWithCoder nsLocale  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsLocale (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ componentsFromLocaleIdentifier:@
componentsFromLocaleIdentifier :: IsNSString string => string -> IO (Id NSDictionary)
componentsFromLocaleIdentifier string =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "componentsFromLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localeIdentifierFromComponents:@
localeIdentifierFromComponents :: IsNSDictionary dict => dict -> IO (Id NSString)
localeIdentifierFromComponents dict =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr dict $ \raw_dict ->
      sendClassMsg cls' (mkSelector "localeIdentifierFromComponents:") (retPtr retVoid) [argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @+ canonicalLocaleIdentifierFromString:@
canonicalLocaleIdentifierFromString :: IsNSString string => string -> IO (Id NSString)
canonicalLocaleIdentifierFromString string =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "canonicalLocaleIdentifierFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ canonicalLanguageIdentifierFromString:@
canonicalLanguageIdentifierFromString :: IsNSString string => string -> IO (Id NSString)
canonicalLanguageIdentifierFromString string =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "canonicalLanguageIdentifierFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localeIdentifierFromWindowsLocaleCode:@
localeIdentifierFromWindowsLocaleCode :: CUInt -> IO (Id NSString)
localeIdentifierFromWindowsLocaleCode lcid =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "localeIdentifierFromWindowsLocaleCode:") (retPtr retVoid) [argCUInt (fromIntegral lcid)] >>= retainedObject . castPtr

-- | @+ windowsLocaleCodeFromLocaleIdentifier:@
windowsLocaleCodeFromLocaleIdentifier :: IsNSString localeIdentifier => localeIdentifier -> IO CUInt
windowsLocaleCodeFromLocaleIdentifier localeIdentifier =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr localeIdentifier $ \raw_localeIdentifier ->
      sendClassMsg cls' (mkSelector "windowsLocaleCodeFromLocaleIdentifier:") retCUInt [argPtr (castPtr raw_localeIdentifier :: Ptr ())]

-- | @+ characterDirectionForLanguage:@
characterDirectionForLanguage :: IsNSString isoLangCode => isoLangCode -> IO NSLocaleLanguageDirection
characterDirectionForLanguage isoLangCode =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr isoLangCode $ \raw_isoLangCode ->
      fmap (coerce :: CULong -> NSLocaleLanguageDirection) $ sendClassMsg cls' (mkSelector "characterDirectionForLanguage:") retCULong [argPtr (castPtr raw_isoLangCode :: Ptr ())]

-- | @+ lineDirectionForLanguage:@
lineDirectionForLanguage :: IsNSString isoLangCode => isoLangCode -> IO NSLocaleLanguageDirection
lineDirectionForLanguage isoLangCode =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr isoLangCode $ \raw_isoLangCode ->
      fmap (coerce :: CULong -> NSLocaleLanguageDirection) $ sendClassMsg cls' (mkSelector "lineDirectionForLanguage:") retCULong [argPtr (castPtr raw_isoLangCode :: Ptr ())]

-- | @+ localeWithLocaleIdentifier:@
localeWithLocaleIdentifier :: IsNSString ident => ident -> IO (Id NSLocale)
localeWithLocaleIdentifier ident =
  do
    cls' <- getRequiredClass "NSLocale"
    withObjCPtr ident $ \raw_ident ->
      sendClassMsg cls' (mkSelector "localeWithLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_ident :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSLocale nsLocale => nsLocale -> IO (Id NSLocale)
init_ nsLocale  =
  sendMsg nsLocale (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- localizedStringForLocaleIdentifier:@
localizedStringForLocaleIdentifier :: (IsNSLocale nsLocale, IsNSString localeIdentifier) => nsLocale -> localeIdentifier -> IO (Id NSString)
localizedStringForLocaleIdentifier nsLocale  localeIdentifier =
withObjCPtr localeIdentifier $ \raw_localeIdentifier ->
    sendMsg nsLocale (mkSelector "localizedStringForLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_localeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForLanguageCode:@
localizedStringForLanguageCode :: (IsNSLocale nsLocale, IsNSString languageCode) => nsLocale -> languageCode -> IO (Id NSString)
localizedStringForLanguageCode nsLocale  languageCode =
withObjCPtr languageCode $ \raw_languageCode ->
    sendMsg nsLocale (mkSelector "localizedStringForLanguageCode:") (retPtr retVoid) [argPtr (castPtr raw_languageCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForCountryCode:@
localizedStringForCountryCode :: (IsNSLocale nsLocale, IsNSString countryCode) => nsLocale -> countryCode -> IO (Id NSString)
localizedStringForCountryCode nsLocale  countryCode =
withObjCPtr countryCode $ \raw_countryCode ->
    sendMsg nsLocale (mkSelector "localizedStringForCountryCode:") (retPtr retVoid) [argPtr (castPtr raw_countryCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForScriptCode:@
localizedStringForScriptCode :: (IsNSLocale nsLocale, IsNSString scriptCode) => nsLocale -> scriptCode -> IO (Id NSString)
localizedStringForScriptCode nsLocale  scriptCode =
withObjCPtr scriptCode $ \raw_scriptCode ->
    sendMsg nsLocale (mkSelector "localizedStringForScriptCode:") (retPtr retVoid) [argPtr (castPtr raw_scriptCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForVariantCode:@
localizedStringForVariantCode :: (IsNSLocale nsLocale, IsNSString variantCode) => nsLocale -> variantCode -> IO (Id NSString)
localizedStringForVariantCode nsLocale  variantCode =
withObjCPtr variantCode $ \raw_variantCode ->
    sendMsg nsLocale (mkSelector "localizedStringForVariantCode:") (retPtr retVoid) [argPtr (castPtr raw_variantCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForCalendarIdentifier:@
localizedStringForCalendarIdentifier :: (IsNSLocale nsLocale, IsNSString calendarIdentifier) => nsLocale -> calendarIdentifier -> IO (Id NSString)
localizedStringForCalendarIdentifier nsLocale  calendarIdentifier =
withObjCPtr calendarIdentifier $ \raw_calendarIdentifier ->
    sendMsg nsLocale (mkSelector "localizedStringForCalendarIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_calendarIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForCollationIdentifier:@
localizedStringForCollationIdentifier :: (IsNSLocale nsLocale, IsNSString collationIdentifier) => nsLocale -> collationIdentifier -> IO (Id NSString)
localizedStringForCollationIdentifier nsLocale  collationIdentifier =
withObjCPtr collationIdentifier $ \raw_collationIdentifier ->
    sendMsg nsLocale (mkSelector "localizedStringForCollationIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_collationIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForCurrencyCode:@
localizedStringForCurrencyCode :: (IsNSLocale nsLocale, IsNSString currencyCode) => nsLocale -> currencyCode -> IO (Id NSString)
localizedStringForCurrencyCode nsLocale  currencyCode =
withObjCPtr currencyCode $ \raw_currencyCode ->
    sendMsg nsLocale (mkSelector "localizedStringForCurrencyCode:") (retPtr retVoid) [argPtr (castPtr raw_currencyCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForCollatorIdentifier:@
localizedStringForCollatorIdentifier :: (IsNSLocale nsLocale, IsNSString collatorIdentifier) => nsLocale -> collatorIdentifier -> IO (Id NSString)
localizedStringForCollatorIdentifier nsLocale  collatorIdentifier =
withObjCPtr collatorIdentifier $ \raw_collatorIdentifier ->
    sendMsg nsLocale (mkSelector "localizedStringForCollatorIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_collatorIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ availableLocaleIdentifiers@
availableLocaleIdentifiers :: IO (Id NSArray)
availableLocaleIdentifiers  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "availableLocaleIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ISOLanguageCodes@
isoLanguageCodes :: IO (Id NSArray)
isoLanguageCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "ISOLanguageCodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ISOCountryCodes@
isoCountryCodes :: IO (Id NSArray)
isoCountryCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "ISOCountryCodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ISOCurrencyCodes@
isoCurrencyCodes :: IO (Id NSArray)
isoCurrencyCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "ISOCurrencyCodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ commonISOCurrencyCodes@
commonISOCurrencyCodes :: IO (Id NSArray)
commonISOCurrencyCodes  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "commonISOCurrencyCodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ preferredLanguages@
preferredLanguages :: IO (Id NSArray)
preferredLanguages  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "preferredLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ autoupdatingCurrentLocale@
autoupdatingCurrentLocale :: IO (Id NSLocale)
autoupdatingCurrentLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "autoupdatingCurrentLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ currentLocale@
currentLocale :: IO (Id NSLocale)
currentLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "currentLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemLocale@
systemLocale :: IO (Id NSLocale)
systemLocale  =
  do
    cls' <- getRequiredClass "NSLocale"
    sendClassMsg cls' (mkSelector "systemLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localeIdentifier@
localeIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
localeIdentifier nsLocale  =
  sendMsg nsLocale (mkSelector "localeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- languageCode@
languageCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
languageCode nsLocale  =
  sendMsg nsLocale (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the identifier for the language part of the locale. For example, returns "en-US" for "en_US\@rg=gbzzzz"  locale.
--
-- ObjC selector: @- languageIdentifier@
languageIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
languageIdentifier nsLocale  =
  sendMsg nsLocale (mkSelector "languageIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- countryCode@
countryCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
countryCode nsLocale  =
  sendMsg nsLocale (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the region code of the locale. If the @rg@ subtag is present, the value of the subtag will be used. For example,  returns "GB" for "en_US\@rg=gbzzzz" locale. If the @localeIdentifier@ doesn’t contain a region, returns @nil@.
--
-- ObjC selector: @- regionCode@
regionCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
regionCode nsLocale  =
  sendMsg nsLocale (mkSelector "regionCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scriptCode@
scriptCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
scriptCode nsLocale  =
  sendMsg nsLocale (mkSelector "scriptCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- variantCode@
variantCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
variantCode nsLocale  =
  sendMsg nsLocale (mkSelector "variantCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- exemplarCharacterSet@
exemplarCharacterSet :: IsNSLocale nsLocale => nsLocale -> IO (Id NSCharacterSet)
exemplarCharacterSet nsLocale  =
  sendMsg nsLocale (mkSelector "exemplarCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- calendarIdentifier@
calendarIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
calendarIdentifier nsLocale  =
  sendMsg nsLocale (mkSelector "calendarIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collationIdentifier@
collationIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
collationIdentifier nsLocale  =
  sendMsg nsLocale (mkSelector "collationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- usesMetricSystem@
usesMetricSystem :: IsNSLocale nsLocale => nsLocale -> IO Bool
usesMetricSystem nsLocale  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLocale (mkSelector "usesMetricSystem") retCULong []

-- | @- decimalSeparator@
decimalSeparator :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
decimalSeparator nsLocale  =
  sendMsg nsLocale (mkSelector "decimalSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupingSeparator@
groupingSeparator :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
groupingSeparator nsLocale  =
  sendMsg nsLocale (mkSelector "groupingSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencySymbol@
currencySymbol :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
currencySymbol nsLocale  =
  sendMsg nsLocale (mkSelector "currencySymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyCode@
currencyCode :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
currencyCode nsLocale  =
  sendMsg nsLocale (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collatorIdentifier@
collatorIdentifier :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
collatorIdentifier nsLocale  =
  sendMsg nsLocale (mkSelector "collatorIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quotationBeginDelimiter@
quotationBeginDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
quotationBeginDelimiter nsLocale  =
  sendMsg nsLocale (mkSelector "quotationBeginDelimiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- quotationEndDelimiter@
quotationEndDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
quotationEndDelimiter nsLocale  =
  sendMsg nsLocale (mkSelector "quotationEndDelimiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternateQuotationBeginDelimiter@
alternateQuotationBeginDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
alternateQuotationBeginDelimiter nsLocale  =
  sendMsg nsLocale (mkSelector "alternateQuotationBeginDelimiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternateQuotationEndDelimiter@
alternateQuotationEndDelimiter :: IsNSLocale nsLocale => nsLocale -> IO (Id NSString)
alternateQuotationEndDelimiter nsLocale  =
  sendMsg nsLocale (mkSelector "alternateQuotationEndDelimiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @displayNameForKey:value:@
displayNameForKey_valueSelector :: Selector
displayNameForKey_valueSelector = mkSelector "displayNameForKey:value:"

-- | @Selector@ for @initWithLocaleIdentifier:@
initWithLocaleIdentifierSelector :: Selector
initWithLocaleIdentifierSelector = mkSelector "initWithLocaleIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @componentsFromLocaleIdentifier:@
componentsFromLocaleIdentifierSelector :: Selector
componentsFromLocaleIdentifierSelector = mkSelector "componentsFromLocaleIdentifier:"

-- | @Selector@ for @localeIdentifierFromComponents:@
localeIdentifierFromComponentsSelector :: Selector
localeIdentifierFromComponentsSelector = mkSelector "localeIdentifierFromComponents:"

-- | @Selector@ for @canonicalLocaleIdentifierFromString:@
canonicalLocaleIdentifierFromStringSelector :: Selector
canonicalLocaleIdentifierFromStringSelector = mkSelector "canonicalLocaleIdentifierFromString:"

-- | @Selector@ for @canonicalLanguageIdentifierFromString:@
canonicalLanguageIdentifierFromStringSelector :: Selector
canonicalLanguageIdentifierFromStringSelector = mkSelector "canonicalLanguageIdentifierFromString:"

-- | @Selector@ for @localeIdentifierFromWindowsLocaleCode:@
localeIdentifierFromWindowsLocaleCodeSelector :: Selector
localeIdentifierFromWindowsLocaleCodeSelector = mkSelector "localeIdentifierFromWindowsLocaleCode:"

-- | @Selector@ for @windowsLocaleCodeFromLocaleIdentifier:@
windowsLocaleCodeFromLocaleIdentifierSelector :: Selector
windowsLocaleCodeFromLocaleIdentifierSelector = mkSelector "windowsLocaleCodeFromLocaleIdentifier:"

-- | @Selector@ for @characterDirectionForLanguage:@
characterDirectionForLanguageSelector :: Selector
characterDirectionForLanguageSelector = mkSelector "characterDirectionForLanguage:"

-- | @Selector@ for @lineDirectionForLanguage:@
lineDirectionForLanguageSelector :: Selector
lineDirectionForLanguageSelector = mkSelector "lineDirectionForLanguage:"

-- | @Selector@ for @localeWithLocaleIdentifier:@
localeWithLocaleIdentifierSelector :: Selector
localeWithLocaleIdentifierSelector = mkSelector "localeWithLocaleIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @localizedStringForLocaleIdentifier:@
localizedStringForLocaleIdentifierSelector :: Selector
localizedStringForLocaleIdentifierSelector = mkSelector "localizedStringForLocaleIdentifier:"

-- | @Selector@ for @localizedStringForLanguageCode:@
localizedStringForLanguageCodeSelector :: Selector
localizedStringForLanguageCodeSelector = mkSelector "localizedStringForLanguageCode:"

-- | @Selector@ for @localizedStringForCountryCode:@
localizedStringForCountryCodeSelector :: Selector
localizedStringForCountryCodeSelector = mkSelector "localizedStringForCountryCode:"

-- | @Selector@ for @localizedStringForScriptCode:@
localizedStringForScriptCodeSelector :: Selector
localizedStringForScriptCodeSelector = mkSelector "localizedStringForScriptCode:"

-- | @Selector@ for @localizedStringForVariantCode:@
localizedStringForVariantCodeSelector :: Selector
localizedStringForVariantCodeSelector = mkSelector "localizedStringForVariantCode:"

-- | @Selector@ for @localizedStringForCalendarIdentifier:@
localizedStringForCalendarIdentifierSelector :: Selector
localizedStringForCalendarIdentifierSelector = mkSelector "localizedStringForCalendarIdentifier:"

-- | @Selector@ for @localizedStringForCollationIdentifier:@
localizedStringForCollationIdentifierSelector :: Selector
localizedStringForCollationIdentifierSelector = mkSelector "localizedStringForCollationIdentifier:"

-- | @Selector@ for @localizedStringForCurrencyCode:@
localizedStringForCurrencyCodeSelector :: Selector
localizedStringForCurrencyCodeSelector = mkSelector "localizedStringForCurrencyCode:"

-- | @Selector@ for @localizedStringForCollatorIdentifier:@
localizedStringForCollatorIdentifierSelector :: Selector
localizedStringForCollatorIdentifierSelector = mkSelector "localizedStringForCollatorIdentifier:"

-- | @Selector@ for @availableLocaleIdentifiers@
availableLocaleIdentifiersSelector :: Selector
availableLocaleIdentifiersSelector = mkSelector "availableLocaleIdentifiers"

-- | @Selector@ for @ISOLanguageCodes@
isoLanguageCodesSelector :: Selector
isoLanguageCodesSelector = mkSelector "ISOLanguageCodes"

-- | @Selector@ for @ISOCountryCodes@
isoCountryCodesSelector :: Selector
isoCountryCodesSelector = mkSelector "ISOCountryCodes"

-- | @Selector@ for @ISOCurrencyCodes@
isoCurrencyCodesSelector :: Selector
isoCurrencyCodesSelector = mkSelector "ISOCurrencyCodes"

-- | @Selector@ for @commonISOCurrencyCodes@
commonISOCurrencyCodesSelector :: Selector
commonISOCurrencyCodesSelector = mkSelector "commonISOCurrencyCodes"

-- | @Selector@ for @preferredLanguages@
preferredLanguagesSelector :: Selector
preferredLanguagesSelector = mkSelector "preferredLanguages"

-- | @Selector@ for @autoupdatingCurrentLocale@
autoupdatingCurrentLocaleSelector :: Selector
autoupdatingCurrentLocaleSelector = mkSelector "autoupdatingCurrentLocale"

-- | @Selector@ for @currentLocale@
currentLocaleSelector :: Selector
currentLocaleSelector = mkSelector "currentLocale"

-- | @Selector@ for @systemLocale@
systemLocaleSelector :: Selector
systemLocaleSelector = mkSelector "systemLocale"

-- | @Selector@ for @localeIdentifier@
localeIdentifierSelector :: Selector
localeIdentifierSelector = mkSelector "localeIdentifier"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @languageIdentifier@
languageIdentifierSelector :: Selector
languageIdentifierSelector = mkSelector "languageIdentifier"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @scriptCode@
scriptCodeSelector :: Selector
scriptCodeSelector = mkSelector "scriptCode"

-- | @Selector@ for @variantCode@
variantCodeSelector :: Selector
variantCodeSelector = mkSelector "variantCode"

-- | @Selector@ for @exemplarCharacterSet@
exemplarCharacterSetSelector :: Selector
exemplarCharacterSetSelector = mkSelector "exemplarCharacterSet"

-- | @Selector@ for @calendarIdentifier@
calendarIdentifierSelector :: Selector
calendarIdentifierSelector = mkSelector "calendarIdentifier"

-- | @Selector@ for @collationIdentifier@
collationIdentifierSelector :: Selector
collationIdentifierSelector = mkSelector "collationIdentifier"

-- | @Selector@ for @usesMetricSystem@
usesMetricSystemSelector :: Selector
usesMetricSystemSelector = mkSelector "usesMetricSystem"

-- | @Selector@ for @decimalSeparator@
decimalSeparatorSelector :: Selector
decimalSeparatorSelector = mkSelector "decimalSeparator"

-- | @Selector@ for @groupingSeparator@
groupingSeparatorSelector :: Selector
groupingSeparatorSelector = mkSelector "groupingSeparator"

-- | @Selector@ for @currencySymbol@
currencySymbolSelector :: Selector
currencySymbolSelector = mkSelector "currencySymbol"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @collatorIdentifier@
collatorIdentifierSelector :: Selector
collatorIdentifierSelector = mkSelector "collatorIdentifier"

-- | @Selector@ for @quotationBeginDelimiter@
quotationBeginDelimiterSelector :: Selector
quotationBeginDelimiterSelector = mkSelector "quotationBeginDelimiter"

-- | @Selector@ for @quotationEndDelimiter@
quotationEndDelimiterSelector :: Selector
quotationEndDelimiterSelector = mkSelector "quotationEndDelimiter"

-- | @Selector@ for @alternateQuotationBeginDelimiter@
alternateQuotationBeginDelimiterSelector :: Selector
alternateQuotationBeginDelimiterSelector = mkSelector "alternateQuotationBeginDelimiter"

-- | @Selector@ for @alternateQuotationEndDelimiter@
alternateQuotationEndDelimiterSelector :: Selector
alternateQuotationEndDelimiterSelector = mkSelector "alternateQuotationEndDelimiter"

