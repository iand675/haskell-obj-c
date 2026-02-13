{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextCheckingResult@.
module ObjC.Foundation.NSTextCheckingResult
  ( NSTextCheckingResult
  , IsNSTextCheckingResult(..)
  , orthographyCheckingResultWithRange_orthography
  , spellCheckingResultWithRange
  , grammarCheckingResultWithRange_details
  , dateCheckingResultWithRange_date
  , dateCheckingResultWithRange_date_timeZone_duration
  , addressCheckingResultWithRange_components
  , linkCheckingResultWithRange_URL
  , quoteCheckingResultWithRange_replacementString
  , dashCheckingResultWithRange_replacementString
  , replacementCheckingResultWithRange_replacementString
  , correctionCheckingResultWithRange_replacementString
  , correctionCheckingResultWithRange_replacementString_alternativeStrings
  , regularExpressionCheckingResultWithRanges_count_regularExpression
  , phoneNumberCheckingResultWithRange_phoneNumber
  , transitInformationCheckingResultWithRange_components
  , rangeAtIndex
  , rangeWithName
  , resultByAdjustingRangesWithOffset
  , resultType
  , range
  , orthography
  , grammarDetails
  , date
  , timeZone
  , duration
  , components
  , url
  , replacementString
  , alternativeStrings
  , regularExpression
  , phoneNumber
  , numberOfRanges
  , addressComponents
  , addressCheckingResultWithRange_componentsSelector
  , addressComponentsSelector
  , alternativeStringsSelector
  , componentsSelector
  , correctionCheckingResultWithRange_replacementStringSelector
  , correctionCheckingResultWithRange_replacementString_alternativeStringsSelector
  , dashCheckingResultWithRange_replacementStringSelector
  , dateCheckingResultWithRange_dateSelector
  , dateCheckingResultWithRange_date_timeZone_durationSelector
  , dateSelector
  , durationSelector
  , grammarCheckingResultWithRange_detailsSelector
  , grammarDetailsSelector
  , linkCheckingResultWithRange_URLSelector
  , numberOfRangesSelector
  , orthographyCheckingResultWithRange_orthographySelector
  , orthographySelector
  , phoneNumberCheckingResultWithRange_phoneNumberSelector
  , phoneNumberSelector
  , quoteCheckingResultWithRange_replacementStringSelector
  , rangeAtIndexSelector
  , rangeSelector
  , rangeWithNameSelector
  , regularExpressionCheckingResultWithRanges_count_regularExpressionSelector
  , regularExpressionSelector
  , replacementCheckingResultWithRange_replacementStringSelector
  , replacementStringSelector
  , resultByAdjustingRangesWithOffsetSelector
  , resultTypeSelector
  , spellCheckingResultWithRangeSelector
  , timeZoneSelector
  , transitInformationCheckingResultWithRange_componentsSelector
  , urlSelector

  -- * Enum types
  , NSTextCheckingType(NSTextCheckingType)
  , pattern NSTextCheckingTypeOrthography
  , pattern NSTextCheckingTypeSpelling
  , pattern NSTextCheckingTypeGrammar
  , pattern NSTextCheckingTypeDate
  , pattern NSTextCheckingTypeAddress
  , pattern NSTextCheckingTypeLink
  , pattern NSTextCheckingTypeQuote
  , pattern NSTextCheckingTypeDash
  , pattern NSTextCheckingTypeReplacement
  , pattern NSTextCheckingTypeCorrection
  , pattern NSTextCheckingTypeRegularExpression
  , pattern NSTextCheckingTypePhoneNumber
  , pattern NSTextCheckingTypeTransitInformation

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

-- | @+ orthographyCheckingResultWithRange:orthography:@
orthographyCheckingResultWithRange_orthography :: IsNSOrthography orthography => NSRange -> orthography -> IO (Id NSTextCheckingResult)
orthographyCheckingResultWithRange_orthography range orthography =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' orthographyCheckingResultWithRange_orthographySelector range (toNSOrthography orthography)

-- | @+ spellCheckingResultWithRange:@
spellCheckingResultWithRange :: NSRange -> IO (Id NSTextCheckingResult)
spellCheckingResultWithRange range =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' spellCheckingResultWithRangeSelector range

-- | @+ grammarCheckingResultWithRange:details:@
grammarCheckingResultWithRange_details :: IsNSArray details => NSRange -> details -> IO (Id NSTextCheckingResult)
grammarCheckingResultWithRange_details range details =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' grammarCheckingResultWithRange_detailsSelector range (toNSArray details)

-- | @+ dateCheckingResultWithRange:date:@
dateCheckingResultWithRange_date :: IsNSDate date => NSRange -> date -> IO (Id NSTextCheckingResult)
dateCheckingResultWithRange_date range date =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' dateCheckingResultWithRange_dateSelector range (toNSDate date)

-- | @+ dateCheckingResultWithRange:date:timeZone:duration:@
dateCheckingResultWithRange_date_timeZone_duration :: (IsNSDate date, IsNSTimeZone timeZone) => NSRange -> date -> timeZone -> CDouble -> IO (Id NSTextCheckingResult)
dateCheckingResultWithRange_date_timeZone_duration range date timeZone duration =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' dateCheckingResultWithRange_date_timeZone_durationSelector range (toNSDate date) (toNSTimeZone timeZone) duration

-- | @+ addressCheckingResultWithRange:components:@
addressCheckingResultWithRange_components :: IsNSDictionary components => NSRange -> components -> IO (Id NSTextCheckingResult)
addressCheckingResultWithRange_components range components =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' addressCheckingResultWithRange_componentsSelector range (toNSDictionary components)

-- | @+ linkCheckingResultWithRange:URL:@
linkCheckingResultWithRange_URL :: IsNSURL url => NSRange -> url -> IO (Id NSTextCheckingResult)
linkCheckingResultWithRange_URL range url =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' linkCheckingResultWithRange_URLSelector range (toNSURL url)

-- | @+ quoteCheckingResultWithRange:replacementString:@
quoteCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
quoteCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' quoteCheckingResultWithRange_replacementStringSelector range (toNSString replacementString)

-- | @+ dashCheckingResultWithRange:replacementString:@
dashCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
dashCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' dashCheckingResultWithRange_replacementStringSelector range (toNSString replacementString)

-- | @+ replacementCheckingResultWithRange:replacementString:@
replacementCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
replacementCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' replacementCheckingResultWithRange_replacementStringSelector range (toNSString replacementString)

-- | @+ correctionCheckingResultWithRange:replacementString:@
correctionCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' correctionCheckingResultWithRange_replacementStringSelector range (toNSString replacementString)

-- | @+ correctionCheckingResultWithRange:replacementString:alternativeStrings:@
correctionCheckingResultWithRange_replacementString_alternativeStrings :: (IsNSString replacementString, IsNSArray alternativeStrings) => NSRange -> replacementString -> alternativeStrings -> IO (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementString_alternativeStrings range replacementString alternativeStrings =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' correctionCheckingResultWithRange_replacementString_alternativeStringsSelector range (toNSString replacementString) (toNSArray alternativeStrings)

-- | @+ regularExpressionCheckingResultWithRanges:count:regularExpression:@
regularExpressionCheckingResultWithRanges_count_regularExpression :: IsNSRegularExpression regularExpression => Ptr NSRange -> CULong -> regularExpression -> IO (Id NSTextCheckingResult)
regularExpressionCheckingResultWithRanges_count_regularExpression ranges count regularExpression =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' regularExpressionCheckingResultWithRanges_count_regularExpressionSelector ranges count (toNSRegularExpression regularExpression)

-- | @+ phoneNumberCheckingResultWithRange:phoneNumber:@
phoneNumberCheckingResultWithRange_phoneNumber :: IsNSString phoneNumber => NSRange -> phoneNumber -> IO (Id NSTextCheckingResult)
phoneNumberCheckingResultWithRange_phoneNumber range phoneNumber =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' phoneNumberCheckingResultWithRange_phoneNumberSelector range (toNSString phoneNumber)

-- | @+ transitInformationCheckingResultWithRange:components:@
transitInformationCheckingResultWithRange_components :: IsNSDictionary components => NSRange -> components -> IO (Id NSTextCheckingResult)
transitInformationCheckingResultWithRange_components range components =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMessage cls' transitInformationCheckingResultWithRange_componentsSelector range (toNSDictionary components)

-- | @- rangeAtIndex:@
rangeAtIndex :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> CULong -> IO NSRange
rangeAtIndex nsTextCheckingResult idx =
  sendMessage nsTextCheckingResult rangeAtIndexSelector idx

-- | @- rangeWithName:@
rangeWithName :: (IsNSTextCheckingResult nsTextCheckingResult, IsNSString name) => nsTextCheckingResult -> name -> IO NSRange
rangeWithName nsTextCheckingResult name =
  sendMessage nsTextCheckingResult rangeWithNameSelector (toNSString name)

-- | @- resultByAdjustingRangesWithOffset:@
resultByAdjustingRangesWithOffset :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> CLong -> IO (Id NSTextCheckingResult)
resultByAdjustingRangesWithOffset nsTextCheckingResult offset =
  sendMessage nsTextCheckingResult resultByAdjustingRangesWithOffsetSelector offset

-- | @- resultType@
resultType :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO NSTextCheckingType
resultType nsTextCheckingResult =
  sendMessage nsTextCheckingResult resultTypeSelector

-- | @- range@
range :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO NSRange
range nsTextCheckingResult =
  sendMessage nsTextCheckingResult rangeSelector

-- | @- orthography@
orthography :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSOrthography)
orthography nsTextCheckingResult =
  sendMessage nsTextCheckingResult orthographySelector

-- | @- grammarDetails@
grammarDetails :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSArray)
grammarDetails nsTextCheckingResult =
  sendMessage nsTextCheckingResult grammarDetailsSelector

-- | @- date@
date :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDate)
date nsTextCheckingResult =
  sendMessage nsTextCheckingResult dateSelector

-- | @- timeZone@
timeZone :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSTimeZone)
timeZone nsTextCheckingResult =
  sendMessage nsTextCheckingResult timeZoneSelector

-- | @- duration@
duration :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO CDouble
duration nsTextCheckingResult =
  sendMessage nsTextCheckingResult durationSelector

-- | @- components@
components :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDictionary)
components nsTextCheckingResult =
  sendMessage nsTextCheckingResult componentsSelector

-- | @- URL@
url :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSURL)
url nsTextCheckingResult =
  sendMessage nsTextCheckingResult urlSelector

-- | @- replacementString@
replacementString :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSString)
replacementString nsTextCheckingResult =
  sendMessage nsTextCheckingResult replacementStringSelector

-- | @- alternativeStrings@
alternativeStrings :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSArray)
alternativeStrings nsTextCheckingResult =
  sendMessage nsTextCheckingResult alternativeStringsSelector

-- | @- regularExpression@
regularExpression :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSRegularExpression)
regularExpression nsTextCheckingResult =
  sendMessage nsTextCheckingResult regularExpressionSelector

-- | @- phoneNumber@
phoneNumber :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSString)
phoneNumber nsTextCheckingResult =
  sendMessage nsTextCheckingResult phoneNumberSelector

-- | @- numberOfRanges@
numberOfRanges :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO CULong
numberOfRanges nsTextCheckingResult =
  sendMessage nsTextCheckingResult numberOfRangesSelector

-- | @- addressComponents@
addressComponents :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDictionary)
addressComponents nsTextCheckingResult =
  sendMessage nsTextCheckingResult addressComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @orthographyCheckingResultWithRange:orthography:@
orthographyCheckingResultWithRange_orthographySelector :: Selector '[NSRange, Id NSOrthography] (Id NSTextCheckingResult)
orthographyCheckingResultWithRange_orthographySelector = mkSelector "orthographyCheckingResultWithRange:orthography:"

-- | @Selector@ for @spellCheckingResultWithRange:@
spellCheckingResultWithRangeSelector :: Selector '[NSRange] (Id NSTextCheckingResult)
spellCheckingResultWithRangeSelector = mkSelector "spellCheckingResultWithRange:"

-- | @Selector@ for @grammarCheckingResultWithRange:details:@
grammarCheckingResultWithRange_detailsSelector :: Selector '[NSRange, Id NSArray] (Id NSTextCheckingResult)
grammarCheckingResultWithRange_detailsSelector = mkSelector "grammarCheckingResultWithRange:details:"

-- | @Selector@ for @dateCheckingResultWithRange:date:@
dateCheckingResultWithRange_dateSelector :: Selector '[NSRange, Id NSDate] (Id NSTextCheckingResult)
dateCheckingResultWithRange_dateSelector = mkSelector "dateCheckingResultWithRange:date:"

-- | @Selector@ for @dateCheckingResultWithRange:date:timeZone:duration:@
dateCheckingResultWithRange_date_timeZone_durationSelector :: Selector '[NSRange, Id NSDate, Id NSTimeZone, CDouble] (Id NSTextCheckingResult)
dateCheckingResultWithRange_date_timeZone_durationSelector = mkSelector "dateCheckingResultWithRange:date:timeZone:duration:"

-- | @Selector@ for @addressCheckingResultWithRange:components:@
addressCheckingResultWithRange_componentsSelector :: Selector '[NSRange, Id NSDictionary] (Id NSTextCheckingResult)
addressCheckingResultWithRange_componentsSelector = mkSelector "addressCheckingResultWithRange:components:"

-- | @Selector@ for @linkCheckingResultWithRange:URL:@
linkCheckingResultWithRange_URLSelector :: Selector '[NSRange, Id NSURL] (Id NSTextCheckingResult)
linkCheckingResultWithRange_URLSelector = mkSelector "linkCheckingResultWithRange:URL:"

-- | @Selector@ for @quoteCheckingResultWithRange:replacementString:@
quoteCheckingResultWithRange_replacementStringSelector :: Selector '[NSRange, Id NSString] (Id NSTextCheckingResult)
quoteCheckingResultWithRange_replacementStringSelector = mkSelector "quoteCheckingResultWithRange:replacementString:"

-- | @Selector@ for @dashCheckingResultWithRange:replacementString:@
dashCheckingResultWithRange_replacementStringSelector :: Selector '[NSRange, Id NSString] (Id NSTextCheckingResult)
dashCheckingResultWithRange_replacementStringSelector = mkSelector "dashCheckingResultWithRange:replacementString:"

-- | @Selector@ for @replacementCheckingResultWithRange:replacementString:@
replacementCheckingResultWithRange_replacementStringSelector :: Selector '[NSRange, Id NSString] (Id NSTextCheckingResult)
replacementCheckingResultWithRange_replacementStringSelector = mkSelector "replacementCheckingResultWithRange:replacementString:"

-- | @Selector@ for @correctionCheckingResultWithRange:replacementString:@
correctionCheckingResultWithRange_replacementStringSelector :: Selector '[NSRange, Id NSString] (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementStringSelector = mkSelector "correctionCheckingResultWithRange:replacementString:"

-- | @Selector@ for @correctionCheckingResultWithRange:replacementString:alternativeStrings:@
correctionCheckingResultWithRange_replacementString_alternativeStringsSelector :: Selector '[NSRange, Id NSString, Id NSArray] (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementString_alternativeStringsSelector = mkSelector "correctionCheckingResultWithRange:replacementString:alternativeStrings:"

-- | @Selector@ for @regularExpressionCheckingResultWithRanges:count:regularExpression:@
regularExpressionCheckingResultWithRanges_count_regularExpressionSelector :: Selector '[Ptr NSRange, CULong, Id NSRegularExpression] (Id NSTextCheckingResult)
regularExpressionCheckingResultWithRanges_count_regularExpressionSelector = mkSelector "regularExpressionCheckingResultWithRanges:count:regularExpression:"

-- | @Selector@ for @phoneNumberCheckingResultWithRange:phoneNumber:@
phoneNumberCheckingResultWithRange_phoneNumberSelector :: Selector '[NSRange, Id NSString] (Id NSTextCheckingResult)
phoneNumberCheckingResultWithRange_phoneNumberSelector = mkSelector "phoneNumberCheckingResultWithRange:phoneNumber:"

-- | @Selector@ for @transitInformationCheckingResultWithRange:components:@
transitInformationCheckingResultWithRange_componentsSelector :: Selector '[NSRange, Id NSDictionary] (Id NSTextCheckingResult)
transitInformationCheckingResultWithRange_componentsSelector = mkSelector "transitInformationCheckingResultWithRange:components:"

-- | @Selector@ for @rangeAtIndex:@
rangeAtIndexSelector :: Selector '[CULong] NSRange
rangeAtIndexSelector = mkSelector "rangeAtIndex:"

-- | @Selector@ for @rangeWithName:@
rangeWithNameSelector :: Selector '[Id NSString] NSRange
rangeWithNameSelector = mkSelector "rangeWithName:"

-- | @Selector@ for @resultByAdjustingRangesWithOffset:@
resultByAdjustingRangesWithOffsetSelector :: Selector '[CLong] (Id NSTextCheckingResult)
resultByAdjustingRangesWithOffsetSelector = mkSelector "resultByAdjustingRangesWithOffset:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSTextCheckingType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @range@
rangeSelector :: Selector '[] NSRange
rangeSelector = mkSelector "range"

-- | @Selector@ for @orthography@
orthographySelector :: Selector '[] (Id NSOrthography)
orthographySelector = mkSelector "orthography"

-- | @Selector@ for @grammarDetails@
grammarDetailsSelector :: Selector '[] (Id NSArray)
grammarDetailsSelector = mkSelector "grammarDetails"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] (Id NSTimeZone)
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSDictionary)
componentsSelector = mkSelector "components"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @replacementString@
replacementStringSelector :: Selector '[] (Id NSString)
replacementStringSelector = mkSelector "replacementString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector '[] (Id NSArray)
alternativeStringsSelector = mkSelector "alternativeStrings"

-- | @Selector@ for @regularExpression@
regularExpressionSelector :: Selector '[] (Id NSRegularExpression)
regularExpressionSelector = mkSelector "regularExpression"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @numberOfRanges@
numberOfRangesSelector :: Selector '[] CULong
numberOfRangesSelector = mkSelector "numberOfRanges"

-- | @Selector@ for @addressComponents@
addressComponentsSelector :: Selector '[] (Id NSDictionary)
addressComponentsSelector = mkSelector "addressComponents"

