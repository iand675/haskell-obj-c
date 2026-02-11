{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDate@.
module ObjC.Foundation.NSDate
  ( NSDate
  , IsNSDate(..)
  , init_
  , initWithTimeIntervalSinceReferenceDate
  , initWithCoder
  , dateWithNaturalLanguageString_locale
  , dateWithNaturalLanguageString
  , dateWithString
  , dateWithCalendarFormat_timeZone
  , descriptionWithCalendarFormat_timeZone_locale
  , initWithString
  , date
  , dateWithTimeIntervalSinceNow
  , dateWithTimeIntervalSinceReferenceDate
  , dateWithTimeIntervalSince1970
  , dateWithTimeInterval_sinceDate
  , initWithTimeIntervalSinceNow
  , initWithTimeIntervalSince1970
  , initWithTimeInterval_sinceDate
  , timeIntervalSinceDate
  , addTimeInterval
  , dateByAddingTimeInterval
  , earlierDate
  , laterDate
  , compare_
  , isEqualToDate
  , descriptionWithLocale
  , timeIntervalSinceReferenceDate
  , distantFuture
  , distantPast
  , now
  , timeIntervalSinceNow
  , timeIntervalSince1970
  , description
  , nsDateTimeIntervalSinceReferenceDate
  , initSelector
  , initWithTimeIntervalSinceReferenceDateSelector
  , initWithCoderSelector
  , dateWithNaturalLanguageString_localeSelector
  , dateWithNaturalLanguageStringSelector
  , dateWithStringSelector
  , dateWithCalendarFormat_timeZoneSelector
  , descriptionWithCalendarFormat_timeZone_localeSelector
  , initWithStringSelector
  , dateSelector
  , dateWithTimeIntervalSinceNowSelector
  , dateWithTimeIntervalSinceReferenceDateSelector
  , dateWithTimeIntervalSince1970Selector
  , dateWithTimeInterval_sinceDateSelector
  , initWithTimeIntervalSinceNowSelector
  , initWithTimeIntervalSince1970Selector
  , initWithTimeInterval_sinceDateSelector
  , timeIntervalSinceDateSelector
  , addTimeIntervalSelector
  , dateByAddingTimeIntervalSelector
  , earlierDateSelector
  , laterDateSelector
  , compareSelector
  , isEqualToDateSelector
  , descriptionWithLocaleSelector
  , timeIntervalSinceReferenceDateSelector
  , distantFutureSelector
  , distantPastSelector
  , nowSelector
  , timeIntervalSinceNowSelector
  , timeIntervalSince1970Selector
  , descriptionSelector

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

-- | @- init@
init_ :: IsNSDate nsDate => nsDate -> IO (Id NSDate)
init_ nsDate  =
  sendMsg nsDate (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTimeIntervalSinceReferenceDate:@
initWithTimeIntervalSinceReferenceDate :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSinceReferenceDate nsDate  ti =
  sendMsg nsDate (mkSelector "initWithTimeIntervalSinceReferenceDate:") (retPtr retVoid) [argCDouble (fromIntegral ti)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDate nsDate, IsNSCoder coder) => nsDate -> coder -> IO (Id NSDate)
initWithCoder nsDate  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDate (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ dateWithNaturalLanguageString:locale:@
dateWithNaturalLanguageString_locale :: IsNSString string => string -> RawId -> IO RawId
dateWithNaturalLanguageString_locale string locale =
  do
    cls' <- getRequiredClass "NSDate"
    withObjCPtr string $ \raw_string ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithNaturalLanguageString:locale:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())]

-- | @+ dateWithNaturalLanguageString:@
dateWithNaturalLanguageString :: IsNSString string => string -> IO RawId
dateWithNaturalLanguageString string =
  do
    cls' <- getRequiredClass "NSDate"
    withObjCPtr string $ \raw_string ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithNaturalLanguageString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())]

-- | @+ dateWithString:@
dateWithString :: IsNSString aString => aString -> IO RawId
dateWithString aString =
  do
    cls' <- getRequiredClass "NSDate"
    withObjCPtr aString $ \raw_aString ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dateWithString:") (retPtr retVoid) [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- dateWithCalendarFormat:timeZone:@
dateWithCalendarFormat_timeZone :: (IsNSDate nsDate, IsNSString format, IsNSTimeZone aTimeZone) => nsDate -> format -> aTimeZone -> IO (Id NSCalendarDate)
dateWithCalendarFormat_timeZone nsDate  format aTimeZone =
withObjCPtr format $ \raw_format ->
  withObjCPtr aTimeZone $ \raw_aTimeZone ->
      sendMsg nsDate (mkSelector "dateWithCalendarFormat:timeZone:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_aTimeZone :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptionWithCalendarFormat:timeZone:locale:@
descriptionWithCalendarFormat_timeZone_locale :: (IsNSDate nsDate, IsNSString format, IsNSTimeZone aTimeZone) => nsDate -> format -> aTimeZone -> RawId -> IO (Id NSString)
descriptionWithCalendarFormat_timeZone_locale nsDate  format aTimeZone locale =
withObjCPtr format $ \raw_format ->
  withObjCPtr aTimeZone $ \raw_aTimeZone ->
      sendMsg nsDate (mkSelector "descriptionWithCalendarFormat:timeZone:locale:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_aTimeZone :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSDate nsDate, IsNSString description) => nsDate -> description -> IO RawId
initWithString nsDate  description =
withObjCPtr description $ \raw_description ->
    fmap (RawId . castPtr) $ sendMsg nsDate (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ())]

-- | @+ date@
date :: IO (Id NSDate)
date  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dateWithTimeIntervalSinceNow:@
dateWithTimeIntervalSinceNow :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSinceNow secs =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "dateWithTimeIntervalSinceNow:") (retPtr retVoid) [argCDouble (fromIntegral secs)] >>= retainedObject . castPtr

-- | @+ dateWithTimeIntervalSinceReferenceDate:@
dateWithTimeIntervalSinceReferenceDate :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSinceReferenceDate ti =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "dateWithTimeIntervalSinceReferenceDate:") (retPtr retVoid) [argCDouble (fromIntegral ti)] >>= retainedObject . castPtr

-- | @+ dateWithTimeIntervalSince1970:@
dateWithTimeIntervalSince1970 :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSince1970 secs =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "dateWithTimeIntervalSince1970:") (retPtr retVoid) [argCDouble (fromIntegral secs)] >>= retainedObject . castPtr

-- | @+ dateWithTimeInterval:sinceDate:@
dateWithTimeInterval_sinceDate :: IsNSDate date => CDouble -> date -> IO (Id NSDate)
dateWithTimeInterval_sinceDate secsToBeAdded date =
  do
    cls' <- getRequiredClass "NSDate"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "dateWithTimeInterval:sinceDate:") (retPtr retVoid) [argCDouble (fromIntegral secsToBeAdded), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithTimeIntervalSinceNow:@
initWithTimeIntervalSinceNow :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSinceNow nsDate  secs =
  sendMsg nsDate (mkSelector "initWithTimeIntervalSinceNow:") (retPtr retVoid) [argCDouble (fromIntegral secs)] >>= ownedObject . castPtr

-- | @- initWithTimeIntervalSince1970:@
initWithTimeIntervalSince1970 :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSince1970 nsDate  secs =
  sendMsg nsDate (mkSelector "initWithTimeIntervalSince1970:") (retPtr retVoid) [argCDouble (fromIntegral secs)] >>= ownedObject . castPtr

-- | @- initWithTimeInterval:sinceDate:@
initWithTimeInterval_sinceDate :: (IsNSDate nsDate, IsNSDate date) => nsDate -> CDouble -> date -> IO (Id NSDate)
initWithTimeInterval_sinceDate nsDate  secsToBeAdded date =
withObjCPtr date $ \raw_date ->
    sendMsg nsDate (mkSelector "initWithTimeInterval:sinceDate:") (retPtr retVoid) [argCDouble (fromIntegral secsToBeAdded), argPtr (castPtr raw_date :: Ptr ())] >>= ownedObject . castPtr

-- | @- timeIntervalSinceDate:@
timeIntervalSinceDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO CDouble
timeIntervalSinceDate nsDate  anotherDate =
withObjCPtr anotherDate $ \raw_anotherDate ->
    sendMsg nsDate (mkSelector "timeIntervalSinceDate:") retCDouble [argPtr (castPtr raw_anotherDate :: Ptr ())]

-- | @- addTimeInterval:@
addTimeInterval :: IsNSDate nsDate => nsDate -> CDouble -> IO RawId
addTimeInterval nsDate  seconds =
  fmap (RawId . castPtr) $ sendMsg nsDate (mkSelector "addTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral seconds)]

-- | @- dateByAddingTimeInterval:@
dateByAddingTimeInterval :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
dateByAddingTimeInterval nsDate  ti =
  sendMsg nsDate (mkSelector "dateByAddingTimeInterval:") (retPtr retVoid) [argCDouble (fromIntegral ti)] >>= retainedObject . castPtr

-- | @- earlierDate:@
earlierDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO (Id NSDate)
earlierDate nsDate  anotherDate =
withObjCPtr anotherDate $ \raw_anotherDate ->
    sendMsg nsDate (mkSelector "earlierDate:") (retPtr retVoid) [argPtr (castPtr raw_anotherDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- laterDate:@
laterDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO (Id NSDate)
laterDate nsDate  anotherDate =
withObjCPtr anotherDate $ \raw_anotherDate ->
    sendMsg nsDate (mkSelector "laterDate:") (retPtr retVoid) [argPtr (castPtr raw_anotherDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- compare:@
compare_ :: (IsNSDate nsDate, IsNSDate other) => nsDate -> other -> IO NSComparisonResult
compare_ nsDate  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsDate (mkSelector "compare:") retCLong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- isEqualToDate:@
isEqualToDate :: (IsNSDate nsDate, IsNSDate otherDate) => nsDate -> otherDate -> IO Bool
isEqualToDate nsDate  otherDate =
withObjCPtr otherDate $ \raw_otherDate ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDate (mkSelector "isEqualToDate:") retCULong [argPtr (castPtr raw_otherDate :: Ptr ())]

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDate nsDate => nsDate -> RawId -> IO (Id NSString)
descriptionWithLocale nsDate  locale =
  sendMsg nsDate (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- timeIntervalSinceReferenceDate@
timeIntervalSinceReferenceDate :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSinceReferenceDate nsDate  =
  sendMsg nsDate (mkSelector "timeIntervalSinceReferenceDate") retCDouble []

-- | @+ distantFuture@
distantFuture :: IO (Id NSDate)
distantFuture  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "distantFuture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ distantPast@
distantPast :: IO (Id NSDate)
distantPast  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "distantPast") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ now@
now :: IO (Id NSDate)
now  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "now") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timeIntervalSinceNow@
timeIntervalSinceNow :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSinceNow nsDate  =
  sendMsg nsDate (mkSelector "timeIntervalSinceNow") retCDouble []

-- | @- timeIntervalSince1970@
timeIntervalSince1970 :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSince1970 nsDate  =
  sendMsg nsDate (mkSelector "timeIntervalSince1970") retCDouble []

-- | @- description@
description :: IsNSDate nsDate => nsDate -> IO (Id NSString)
description nsDate  =
  sendMsg nsDate (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ timeIntervalSinceReferenceDate@
nsDateTimeIntervalSinceReferenceDate :: IO CDouble
nsDateTimeIntervalSinceReferenceDate  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "timeIntervalSinceReferenceDate") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTimeIntervalSinceReferenceDate:@
initWithTimeIntervalSinceReferenceDateSelector :: Selector
initWithTimeIntervalSinceReferenceDateSelector = mkSelector "initWithTimeIntervalSinceReferenceDate:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @dateWithNaturalLanguageString:locale:@
dateWithNaturalLanguageString_localeSelector :: Selector
dateWithNaturalLanguageString_localeSelector = mkSelector "dateWithNaturalLanguageString:locale:"

-- | @Selector@ for @dateWithNaturalLanguageString:@
dateWithNaturalLanguageStringSelector :: Selector
dateWithNaturalLanguageStringSelector = mkSelector "dateWithNaturalLanguageString:"

-- | @Selector@ for @dateWithString:@
dateWithStringSelector :: Selector
dateWithStringSelector = mkSelector "dateWithString:"

-- | @Selector@ for @dateWithCalendarFormat:timeZone:@
dateWithCalendarFormat_timeZoneSelector :: Selector
dateWithCalendarFormat_timeZoneSelector = mkSelector "dateWithCalendarFormat:timeZone:"

-- | @Selector@ for @descriptionWithCalendarFormat:timeZone:locale:@
descriptionWithCalendarFormat_timeZone_localeSelector :: Selector
descriptionWithCalendarFormat_timeZone_localeSelector = mkSelector "descriptionWithCalendarFormat:timeZone:locale:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @dateWithTimeIntervalSinceNow:@
dateWithTimeIntervalSinceNowSelector :: Selector
dateWithTimeIntervalSinceNowSelector = mkSelector "dateWithTimeIntervalSinceNow:"

-- | @Selector@ for @dateWithTimeIntervalSinceReferenceDate:@
dateWithTimeIntervalSinceReferenceDateSelector :: Selector
dateWithTimeIntervalSinceReferenceDateSelector = mkSelector "dateWithTimeIntervalSinceReferenceDate:"

-- | @Selector@ for @dateWithTimeIntervalSince1970:@
dateWithTimeIntervalSince1970Selector :: Selector
dateWithTimeIntervalSince1970Selector = mkSelector "dateWithTimeIntervalSince1970:"

-- | @Selector@ for @dateWithTimeInterval:sinceDate:@
dateWithTimeInterval_sinceDateSelector :: Selector
dateWithTimeInterval_sinceDateSelector = mkSelector "dateWithTimeInterval:sinceDate:"

-- | @Selector@ for @initWithTimeIntervalSinceNow:@
initWithTimeIntervalSinceNowSelector :: Selector
initWithTimeIntervalSinceNowSelector = mkSelector "initWithTimeIntervalSinceNow:"

-- | @Selector@ for @initWithTimeIntervalSince1970:@
initWithTimeIntervalSince1970Selector :: Selector
initWithTimeIntervalSince1970Selector = mkSelector "initWithTimeIntervalSince1970:"

-- | @Selector@ for @initWithTimeInterval:sinceDate:@
initWithTimeInterval_sinceDateSelector :: Selector
initWithTimeInterval_sinceDateSelector = mkSelector "initWithTimeInterval:sinceDate:"

-- | @Selector@ for @timeIntervalSinceDate:@
timeIntervalSinceDateSelector :: Selector
timeIntervalSinceDateSelector = mkSelector "timeIntervalSinceDate:"

-- | @Selector@ for @addTimeInterval:@
addTimeIntervalSelector :: Selector
addTimeIntervalSelector = mkSelector "addTimeInterval:"

-- | @Selector@ for @dateByAddingTimeInterval:@
dateByAddingTimeIntervalSelector :: Selector
dateByAddingTimeIntervalSelector = mkSelector "dateByAddingTimeInterval:"

-- | @Selector@ for @earlierDate:@
earlierDateSelector :: Selector
earlierDateSelector = mkSelector "earlierDate:"

-- | @Selector@ for @laterDate:@
laterDateSelector :: Selector
laterDateSelector = mkSelector "laterDate:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToDate:@
isEqualToDateSelector :: Selector
isEqualToDateSelector = mkSelector "isEqualToDate:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @timeIntervalSinceReferenceDate@
timeIntervalSinceReferenceDateSelector :: Selector
timeIntervalSinceReferenceDateSelector = mkSelector "timeIntervalSinceReferenceDate"

-- | @Selector@ for @distantFuture@
distantFutureSelector :: Selector
distantFutureSelector = mkSelector "distantFuture"

-- | @Selector@ for @distantPast@
distantPastSelector :: Selector
distantPastSelector = mkSelector "distantPast"

-- | @Selector@ for @now@
nowSelector :: Selector
nowSelector = mkSelector "now"

-- | @Selector@ for @timeIntervalSinceNow@
timeIntervalSinceNowSelector :: Selector
timeIntervalSinceNowSelector = mkSelector "timeIntervalSinceNow"

-- | @Selector@ for @timeIntervalSince1970@
timeIntervalSince1970Selector :: Selector
timeIntervalSince1970Selector = mkSelector "timeIntervalSince1970"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

