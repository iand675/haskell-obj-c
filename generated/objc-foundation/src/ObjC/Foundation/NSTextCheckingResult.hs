{-# LANGUAGE PatternSynonyms #-}
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
  , orthographyCheckingResultWithRange_orthographySelector
  , spellCheckingResultWithRangeSelector
  , grammarCheckingResultWithRange_detailsSelector
  , dateCheckingResultWithRange_dateSelector
  , dateCheckingResultWithRange_date_timeZone_durationSelector
  , addressCheckingResultWithRange_componentsSelector
  , linkCheckingResultWithRange_URLSelector
  , quoteCheckingResultWithRange_replacementStringSelector
  , dashCheckingResultWithRange_replacementStringSelector
  , replacementCheckingResultWithRange_replacementStringSelector
  , correctionCheckingResultWithRange_replacementStringSelector
  , correctionCheckingResultWithRange_replacementString_alternativeStringsSelector
  , regularExpressionCheckingResultWithRanges_count_regularExpressionSelector
  , phoneNumberCheckingResultWithRange_phoneNumberSelector
  , transitInformationCheckingResultWithRange_componentsSelector
  , rangeAtIndexSelector
  , rangeWithNameSelector
  , resultByAdjustingRangesWithOffsetSelector
  , resultTypeSelector
  , rangeSelector
  , orthographySelector
  , grammarDetailsSelector
  , dateSelector
  , timeZoneSelector
  , durationSelector
  , componentsSelector
  , urlSelector
  , replacementStringSelector
  , alternativeStringsSelector
  , regularExpressionSelector
  , phoneNumberSelector
  , numberOfRangesSelector
  , addressComponentsSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr orthography $ \raw_orthography ->
      sendClassMsg cls' (mkSelector "orthographyCheckingResultWithRange:orthography:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_orthography :: Ptr ())] >>= retainedObject . castPtr

-- | @+ spellCheckingResultWithRange:@
spellCheckingResultWithRange :: NSRange -> IO (Id NSTextCheckingResult)
spellCheckingResultWithRange range =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    sendClassMsg cls' (mkSelector "spellCheckingResultWithRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @+ grammarCheckingResultWithRange:details:@
grammarCheckingResultWithRange_details :: IsNSArray details => NSRange -> details -> IO (Id NSTextCheckingResult)
grammarCheckingResultWithRange_details range details =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr details $ \raw_details ->
      sendClassMsg cls' (mkSelector "grammarCheckingResultWithRange:details:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_details :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dateCheckingResultWithRange:date:@
dateCheckingResultWithRange_date :: IsNSDate date => NSRange -> date -> IO (Id NSTextCheckingResult)
dateCheckingResultWithRange_date range date =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "dateCheckingResultWithRange:date:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dateCheckingResultWithRange:date:timeZone:duration:@
dateCheckingResultWithRange_date_timeZone_duration :: (IsNSDate date, IsNSTimeZone timeZone) => NSRange -> date -> timeZone -> CDouble -> IO (Id NSTextCheckingResult)
dateCheckingResultWithRange_date_timeZone_duration range date timeZone duration =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr date $ \raw_date ->
      withObjCPtr timeZone $ \raw_timeZone ->
        sendClassMsg cls' (mkSelector "dateCheckingResultWithRange:date:timeZone:duration:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_timeZone :: Ptr ()), argCDouble (fromIntegral duration)] >>= retainedObject . castPtr

-- | @+ addressCheckingResultWithRange:components:@
addressCheckingResultWithRange_components :: IsNSDictionary components => NSRange -> components -> IO (Id NSTextCheckingResult)
addressCheckingResultWithRange_components range components =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "addressCheckingResultWithRange:components:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @+ linkCheckingResultWithRange:URL:@
linkCheckingResultWithRange_URL :: IsNSURL url => NSRange -> url -> IO (Id NSTextCheckingResult)
linkCheckingResultWithRange_URL range url =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "linkCheckingResultWithRange:URL:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ quoteCheckingResultWithRange:replacementString:@
quoteCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
quoteCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr replacementString $ \raw_replacementString ->
      sendClassMsg cls' (mkSelector "quoteCheckingResultWithRange:replacementString:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacementString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dashCheckingResultWithRange:replacementString:@
dashCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
dashCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr replacementString $ \raw_replacementString ->
      sendClassMsg cls' (mkSelector "dashCheckingResultWithRange:replacementString:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacementString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ replacementCheckingResultWithRange:replacementString:@
replacementCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
replacementCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr replacementString $ \raw_replacementString ->
      sendClassMsg cls' (mkSelector "replacementCheckingResultWithRange:replacementString:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacementString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ correctionCheckingResultWithRange:replacementString:@
correctionCheckingResultWithRange_replacementString :: IsNSString replacementString => NSRange -> replacementString -> IO (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementString range replacementString =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr replacementString $ \raw_replacementString ->
      sendClassMsg cls' (mkSelector "correctionCheckingResultWithRange:replacementString:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacementString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ correctionCheckingResultWithRange:replacementString:alternativeStrings:@
correctionCheckingResultWithRange_replacementString_alternativeStrings :: (IsNSString replacementString, IsNSArray alternativeStrings) => NSRange -> replacementString -> alternativeStrings -> IO (Id NSTextCheckingResult)
correctionCheckingResultWithRange_replacementString_alternativeStrings range replacementString alternativeStrings =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr replacementString $ \raw_replacementString ->
      withObjCPtr alternativeStrings $ \raw_alternativeStrings ->
        sendClassMsg cls' (mkSelector "correctionCheckingResultWithRange:replacementString:alternativeStrings:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacementString :: Ptr ()), argPtr (castPtr raw_alternativeStrings :: Ptr ())] >>= retainedObject . castPtr

-- | @+ regularExpressionCheckingResultWithRanges:count:regularExpression:@
regularExpressionCheckingResultWithRanges_count_regularExpression :: IsNSRegularExpression regularExpression => Ptr NSRange -> CULong -> regularExpression -> IO (Id NSTextCheckingResult)
regularExpressionCheckingResultWithRanges_count_regularExpression ranges count regularExpression =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr regularExpression $ \raw_regularExpression ->
      sendClassMsg cls' (mkSelector "regularExpressionCheckingResultWithRanges:count:regularExpression:") (retPtr retVoid) [argPtr ranges, argCULong (fromIntegral count), argPtr (castPtr raw_regularExpression :: Ptr ())] >>= retainedObject . castPtr

-- | @+ phoneNumberCheckingResultWithRange:phoneNumber:@
phoneNumberCheckingResultWithRange_phoneNumber :: IsNSString phoneNumber => NSRange -> phoneNumber -> IO (Id NSTextCheckingResult)
phoneNumberCheckingResultWithRange_phoneNumber range phoneNumber =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr phoneNumber $ \raw_phoneNumber ->
      sendClassMsg cls' (mkSelector "phoneNumberCheckingResultWithRange:phoneNumber:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= retainedObject . castPtr

-- | @+ transitInformationCheckingResultWithRange:components:@
transitInformationCheckingResultWithRange_components :: IsNSDictionary components => NSRange -> components -> IO (Id NSTextCheckingResult)
transitInformationCheckingResultWithRange_components range components =
  do
    cls' <- getRequiredClass "NSTextCheckingResult"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "transitInformationCheckingResultWithRange:components:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- rangeAtIndex:@
rangeAtIndex :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> CULong -> IO NSRange
rangeAtIndex nsTextCheckingResult  idx =
  sendMsgStret nsTextCheckingResult (mkSelector "rangeAtIndex:") retNSRange [argCULong (fromIntegral idx)]

-- | @- rangeWithName:@
rangeWithName :: (IsNSTextCheckingResult nsTextCheckingResult, IsNSString name) => nsTextCheckingResult -> name -> IO NSRange
rangeWithName nsTextCheckingResult  name =
withObjCPtr name $ \raw_name ->
    sendMsgStret nsTextCheckingResult (mkSelector "rangeWithName:") retNSRange [argPtr (castPtr raw_name :: Ptr ())]

-- | @- resultByAdjustingRangesWithOffset:@
resultByAdjustingRangesWithOffset :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> CLong -> IO (Id NSTextCheckingResult)
resultByAdjustingRangesWithOffset nsTextCheckingResult  offset =
  sendMsg nsTextCheckingResult (mkSelector "resultByAdjustingRangesWithOffset:") (retPtr retVoid) [argCLong (fromIntegral offset)] >>= retainedObject . castPtr

-- | @- resultType@
resultType :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO NSTextCheckingType
resultType nsTextCheckingResult  =
  fmap (coerce :: CULong -> NSTextCheckingType) $ sendMsg nsTextCheckingResult (mkSelector "resultType") retCULong []

-- | @- range@
range :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO NSRange
range nsTextCheckingResult  =
  sendMsgStret nsTextCheckingResult (mkSelector "range") retNSRange []

-- | @- orthography@
orthography :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSOrthography)
orthography nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "orthography") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- grammarDetails@
grammarDetails :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSArray)
grammarDetails nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "grammarDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- date@
date :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDate)
date nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timeZone@
timeZone :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSTimeZone)
timeZone nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "timeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- duration@
duration :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO CDouble
duration nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "duration") retCDouble []

-- | @- components@
components :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDictionary)
components nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSURL)
url nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- replacementString@
replacementString :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSString)
replacementString nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "replacementString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternativeStrings@
alternativeStrings :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSArray)
alternativeStrings nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "alternativeStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- regularExpression@
regularExpression :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSRegularExpression)
regularExpression nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "regularExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneNumber@
phoneNumber :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSString)
phoneNumber nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfRanges@
numberOfRanges :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO CULong
numberOfRanges nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "numberOfRanges") retCULong []

-- | @- addressComponents@
addressComponents :: IsNSTextCheckingResult nsTextCheckingResult => nsTextCheckingResult -> IO (Id NSDictionary)
addressComponents nsTextCheckingResult  =
  sendMsg nsTextCheckingResult (mkSelector "addressComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @orthographyCheckingResultWithRange:orthography:@
orthographyCheckingResultWithRange_orthographySelector :: Selector
orthographyCheckingResultWithRange_orthographySelector = mkSelector "orthographyCheckingResultWithRange:orthography:"

-- | @Selector@ for @spellCheckingResultWithRange:@
spellCheckingResultWithRangeSelector :: Selector
spellCheckingResultWithRangeSelector = mkSelector "spellCheckingResultWithRange:"

-- | @Selector@ for @grammarCheckingResultWithRange:details:@
grammarCheckingResultWithRange_detailsSelector :: Selector
grammarCheckingResultWithRange_detailsSelector = mkSelector "grammarCheckingResultWithRange:details:"

-- | @Selector@ for @dateCheckingResultWithRange:date:@
dateCheckingResultWithRange_dateSelector :: Selector
dateCheckingResultWithRange_dateSelector = mkSelector "dateCheckingResultWithRange:date:"

-- | @Selector@ for @dateCheckingResultWithRange:date:timeZone:duration:@
dateCheckingResultWithRange_date_timeZone_durationSelector :: Selector
dateCheckingResultWithRange_date_timeZone_durationSelector = mkSelector "dateCheckingResultWithRange:date:timeZone:duration:"

-- | @Selector@ for @addressCheckingResultWithRange:components:@
addressCheckingResultWithRange_componentsSelector :: Selector
addressCheckingResultWithRange_componentsSelector = mkSelector "addressCheckingResultWithRange:components:"

-- | @Selector@ for @linkCheckingResultWithRange:URL:@
linkCheckingResultWithRange_URLSelector :: Selector
linkCheckingResultWithRange_URLSelector = mkSelector "linkCheckingResultWithRange:URL:"

-- | @Selector@ for @quoteCheckingResultWithRange:replacementString:@
quoteCheckingResultWithRange_replacementStringSelector :: Selector
quoteCheckingResultWithRange_replacementStringSelector = mkSelector "quoteCheckingResultWithRange:replacementString:"

-- | @Selector@ for @dashCheckingResultWithRange:replacementString:@
dashCheckingResultWithRange_replacementStringSelector :: Selector
dashCheckingResultWithRange_replacementStringSelector = mkSelector "dashCheckingResultWithRange:replacementString:"

-- | @Selector@ for @replacementCheckingResultWithRange:replacementString:@
replacementCheckingResultWithRange_replacementStringSelector :: Selector
replacementCheckingResultWithRange_replacementStringSelector = mkSelector "replacementCheckingResultWithRange:replacementString:"

-- | @Selector@ for @correctionCheckingResultWithRange:replacementString:@
correctionCheckingResultWithRange_replacementStringSelector :: Selector
correctionCheckingResultWithRange_replacementStringSelector = mkSelector "correctionCheckingResultWithRange:replacementString:"

-- | @Selector@ for @correctionCheckingResultWithRange:replacementString:alternativeStrings:@
correctionCheckingResultWithRange_replacementString_alternativeStringsSelector :: Selector
correctionCheckingResultWithRange_replacementString_alternativeStringsSelector = mkSelector "correctionCheckingResultWithRange:replacementString:alternativeStrings:"

-- | @Selector@ for @regularExpressionCheckingResultWithRanges:count:regularExpression:@
regularExpressionCheckingResultWithRanges_count_regularExpressionSelector :: Selector
regularExpressionCheckingResultWithRanges_count_regularExpressionSelector = mkSelector "regularExpressionCheckingResultWithRanges:count:regularExpression:"

-- | @Selector@ for @phoneNumberCheckingResultWithRange:phoneNumber:@
phoneNumberCheckingResultWithRange_phoneNumberSelector :: Selector
phoneNumberCheckingResultWithRange_phoneNumberSelector = mkSelector "phoneNumberCheckingResultWithRange:phoneNumber:"

-- | @Selector@ for @transitInformationCheckingResultWithRange:components:@
transitInformationCheckingResultWithRange_componentsSelector :: Selector
transitInformationCheckingResultWithRange_componentsSelector = mkSelector "transitInformationCheckingResultWithRange:components:"

-- | @Selector@ for @rangeAtIndex:@
rangeAtIndexSelector :: Selector
rangeAtIndexSelector = mkSelector "rangeAtIndex:"

-- | @Selector@ for @rangeWithName:@
rangeWithNameSelector :: Selector
rangeWithNameSelector = mkSelector "rangeWithName:"

-- | @Selector@ for @resultByAdjustingRangesWithOffset:@
resultByAdjustingRangesWithOffsetSelector :: Selector
resultByAdjustingRangesWithOffsetSelector = mkSelector "resultByAdjustingRangesWithOffset:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @range@
rangeSelector :: Selector
rangeSelector = mkSelector "range"

-- | @Selector@ for @orthography@
orthographySelector :: Selector
orthographySelector = mkSelector "orthography"

-- | @Selector@ for @grammarDetails@
grammarDetailsSelector :: Selector
grammarDetailsSelector = mkSelector "grammarDetails"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @replacementString@
replacementStringSelector :: Selector
replacementStringSelector = mkSelector "replacementString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector
alternativeStringsSelector = mkSelector "alternativeStrings"

-- | @Selector@ for @regularExpression@
regularExpressionSelector :: Selector
regularExpressionSelector = mkSelector "regularExpression"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @numberOfRanges@
numberOfRangesSelector :: Selector
numberOfRangesSelector = mkSelector "numberOfRanges"

-- | @Selector@ for @addressComponents@
addressComponentsSelector :: Selector
addressComponentsSelector = mkSelector "addressComponents"

