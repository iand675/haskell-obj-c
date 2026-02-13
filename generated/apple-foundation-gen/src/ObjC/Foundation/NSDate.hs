{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addTimeIntervalSelector
  , compareSelector
  , dateByAddingTimeIntervalSelector
  , dateSelector
  , dateWithCalendarFormat_timeZoneSelector
  , dateWithNaturalLanguageStringSelector
  , dateWithNaturalLanguageString_localeSelector
  , dateWithStringSelector
  , dateWithTimeIntervalSince1970Selector
  , dateWithTimeIntervalSinceNowSelector
  , dateWithTimeIntervalSinceReferenceDateSelector
  , dateWithTimeInterval_sinceDateSelector
  , descriptionSelector
  , descriptionWithCalendarFormat_timeZone_localeSelector
  , descriptionWithLocaleSelector
  , distantFutureSelector
  , distantPastSelector
  , earlierDateSelector
  , initSelector
  , initWithCoderSelector
  , initWithStringSelector
  , initWithTimeIntervalSince1970Selector
  , initWithTimeIntervalSinceNowSelector
  , initWithTimeIntervalSinceReferenceDateSelector
  , initWithTimeInterval_sinceDateSelector
  , isEqualToDateSelector
  , laterDateSelector
  , nowSelector
  , nsDateTimeIntervalSinceReferenceDateSelector
  , timeIntervalSince1970Selector
  , timeIntervalSinceDateSelector
  , timeIntervalSinceNowSelector
  , timeIntervalSinceReferenceDateSelector

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

-- | @- init@
init_ :: IsNSDate nsDate => nsDate -> IO (Id NSDate)
init_ nsDate =
  sendOwnedMessage nsDate initSelector

-- | @- initWithTimeIntervalSinceReferenceDate:@
initWithTimeIntervalSinceReferenceDate :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSinceReferenceDate nsDate ti =
  sendOwnedMessage nsDate initWithTimeIntervalSinceReferenceDateSelector ti

-- | @- initWithCoder:@
initWithCoder :: (IsNSDate nsDate, IsNSCoder coder) => nsDate -> coder -> IO (Id NSDate)
initWithCoder nsDate coder =
  sendOwnedMessage nsDate initWithCoderSelector (toNSCoder coder)

-- | @+ dateWithNaturalLanguageString:locale:@
dateWithNaturalLanguageString_locale :: IsNSString string => string -> RawId -> IO RawId
dateWithNaturalLanguageString_locale string locale =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithNaturalLanguageString_localeSelector (toNSString string) locale

-- | @+ dateWithNaturalLanguageString:@
dateWithNaturalLanguageString :: IsNSString string => string -> IO RawId
dateWithNaturalLanguageString string =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithNaturalLanguageStringSelector (toNSString string)

-- | @+ dateWithString:@
dateWithString :: IsNSString aString => aString -> IO RawId
dateWithString aString =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithStringSelector (toNSString aString)

-- | @- dateWithCalendarFormat:timeZone:@
dateWithCalendarFormat_timeZone :: (IsNSDate nsDate, IsNSString format, IsNSTimeZone aTimeZone) => nsDate -> format -> aTimeZone -> IO (Id NSCalendarDate)
dateWithCalendarFormat_timeZone nsDate format aTimeZone =
  sendMessage nsDate dateWithCalendarFormat_timeZoneSelector (toNSString format) (toNSTimeZone aTimeZone)

-- | @- descriptionWithCalendarFormat:timeZone:locale:@
descriptionWithCalendarFormat_timeZone_locale :: (IsNSDate nsDate, IsNSString format, IsNSTimeZone aTimeZone) => nsDate -> format -> aTimeZone -> RawId -> IO (Id NSString)
descriptionWithCalendarFormat_timeZone_locale nsDate format aTimeZone locale =
  sendMessage nsDate descriptionWithCalendarFormat_timeZone_localeSelector (toNSString format) (toNSTimeZone aTimeZone) locale

-- | @- initWithString:@
initWithString :: (IsNSDate nsDate, IsNSString description) => nsDate -> description -> IO RawId
initWithString nsDate description =
  sendOwnedMessage nsDate initWithStringSelector (toNSString description)

-- | @+ date@
date :: IO (Id NSDate)
date  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateSelector

-- | @+ dateWithTimeIntervalSinceNow:@
dateWithTimeIntervalSinceNow :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSinceNow secs =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithTimeIntervalSinceNowSelector secs

-- | @+ dateWithTimeIntervalSinceReferenceDate:@
dateWithTimeIntervalSinceReferenceDate :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSinceReferenceDate ti =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithTimeIntervalSinceReferenceDateSelector ti

-- | @+ dateWithTimeIntervalSince1970:@
dateWithTimeIntervalSince1970 :: CDouble -> IO (Id NSDate)
dateWithTimeIntervalSince1970 secs =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithTimeIntervalSince1970Selector secs

-- | @+ dateWithTimeInterval:sinceDate:@
dateWithTimeInterval_sinceDate :: IsNSDate date => CDouble -> date -> IO (Id NSDate)
dateWithTimeInterval_sinceDate secsToBeAdded date =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithTimeInterval_sinceDateSelector secsToBeAdded (toNSDate date)

-- | @- initWithTimeIntervalSinceNow:@
initWithTimeIntervalSinceNow :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSinceNow nsDate secs =
  sendOwnedMessage nsDate initWithTimeIntervalSinceNowSelector secs

-- | @- initWithTimeIntervalSince1970:@
initWithTimeIntervalSince1970 :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithTimeIntervalSince1970 nsDate secs =
  sendOwnedMessage nsDate initWithTimeIntervalSince1970Selector secs

-- | @- initWithTimeInterval:sinceDate:@
initWithTimeInterval_sinceDate :: (IsNSDate nsDate, IsNSDate date) => nsDate -> CDouble -> date -> IO (Id NSDate)
initWithTimeInterval_sinceDate nsDate secsToBeAdded date =
  sendOwnedMessage nsDate initWithTimeInterval_sinceDateSelector secsToBeAdded (toNSDate date)

-- | @- timeIntervalSinceDate:@
timeIntervalSinceDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO CDouble
timeIntervalSinceDate nsDate anotherDate =
  sendMessage nsDate timeIntervalSinceDateSelector (toNSDate anotherDate)

-- | @- addTimeInterval:@
addTimeInterval :: IsNSDate nsDate => nsDate -> CDouble -> IO RawId
addTimeInterval nsDate seconds =
  sendMessage nsDate addTimeIntervalSelector seconds

-- | @- dateByAddingTimeInterval:@
dateByAddingTimeInterval :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
dateByAddingTimeInterval nsDate ti =
  sendMessage nsDate dateByAddingTimeIntervalSelector ti

-- | @- earlierDate:@
earlierDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO (Id NSDate)
earlierDate nsDate anotherDate =
  sendMessage nsDate earlierDateSelector (toNSDate anotherDate)

-- | @- laterDate:@
laterDate :: (IsNSDate nsDate, IsNSDate anotherDate) => nsDate -> anotherDate -> IO (Id NSDate)
laterDate nsDate anotherDate =
  sendMessage nsDate laterDateSelector (toNSDate anotherDate)

-- | @- compare:@
compare_ :: (IsNSDate nsDate, IsNSDate other) => nsDate -> other -> IO NSComparisonResult
compare_ nsDate other =
  sendMessage nsDate compareSelector (toNSDate other)

-- | @- isEqualToDate:@
isEqualToDate :: (IsNSDate nsDate, IsNSDate otherDate) => nsDate -> otherDate -> IO Bool
isEqualToDate nsDate otherDate =
  sendMessage nsDate isEqualToDateSelector (toNSDate otherDate)

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSDate nsDate => nsDate -> RawId -> IO (Id NSString)
descriptionWithLocale nsDate locale =
  sendMessage nsDate descriptionWithLocaleSelector locale

-- | @- timeIntervalSinceReferenceDate@
timeIntervalSinceReferenceDate :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSinceReferenceDate nsDate =
  sendMessage nsDate timeIntervalSinceReferenceDateSelector

-- | @+ distantFuture@
distantFuture :: IO (Id NSDate)
distantFuture  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' distantFutureSelector

-- | @+ distantPast@
distantPast :: IO (Id NSDate)
distantPast  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' distantPastSelector

-- | @+ now@
now :: IO (Id NSDate)
now  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' nowSelector

-- | @- timeIntervalSinceNow@
timeIntervalSinceNow :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSinceNow nsDate =
  sendMessage nsDate timeIntervalSinceNowSelector

-- | @- timeIntervalSince1970@
timeIntervalSince1970 :: IsNSDate nsDate => nsDate -> IO CDouble
timeIntervalSince1970 nsDate =
  sendMessage nsDate timeIntervalSince1970Selector

-- | @- description@
description :: IsNSDate nsDate => nsDate -> IO (Id NSString)
description nsDate =
  sendMessage nsDate descriptionSelector

-- | @+ timeIntervalSinceReferenceDate@
nsDateTimeIntervalSinceReferenceDate :: IO CDouble
nsDateTimeIntervalSinceReferenceDate  =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' nsDateTimeIntervalSinceReferenceDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDate)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTimeIntervalSinceReferenceDate:@
initWithTimeIntervalSinceReferenceDateSelector :: Selector '[CDouble] (Id NSDate)
initWithTimeIntervalSinceReferenceDateSelector = mkSelector "initWithTimeIntervalSinceReferenceDate:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDate)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @dateWithNaturalLanguageString:locale:@
dateWithNaturalLanguageString_localeSelector :: Selector '[Id NSString, RawId] RawId
dateWithNaturalLanguageString_localeSelector = mkSelector "dateWithNaturalLanguageString:locale:"

-- | @Selector@ for @dateWithNaturalLanguageString:@
dateWithNaturalLanguageStringSelector :: Selector '[Id NSString] RawId
dateWithNaturalLanguageStringSelector = mkSelector "dateWithNaturalLanguageString:"

-- | @Selector@ for @dateWithString:@
dateWithStringSelector :: Selector '[Id NSString] RawId
dateWithStringSelector = mkSelector "dateWithString:"

-- | @Selector@ for @dateWithCalendarFormat:timeZone:@
dateWithCalendarFormat_timeZoneSelector :: Selector '[Id NSString, Id NSTimeZone] (Id NSCalendarDate)
dateWithCalendarFormat_timeZoneSelector = mkSelector "dateWithCalendarFormat:timeZone:"

-- | @Selector@ for @descriptionWithCalendarFormat:timeZone:locale:@
descriptionWithCalendarFormat_timeZone_localeSelector :: Selector '[Id NSString, Id NSTimeZone, RawId] (Id NSString)
descriptionWithCalendarFormat_timeZone_localeSelector = mkSelector "descriptionWithCalendarFormat:timeZone:locale:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] RawId
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @dateWithTimeIntervalSinceNow:@
dateWithTimeIntervalSinceNowSelector :: Selector '[CDouble] (Id NSDate)
dateWithTimeIntervalSinceNowSelector = mkSelector "dateWithTimeIntervalSinceNow:"

-- | @Selector@ for @dateWithTimeIntervalSinceReferenceDate:@
dateWithTimeIntervalSinceReferenceDateSelector :: Selector '[CDouble] (Id NSDate)
dateWithTimeIntervalSinceReferenceDateSelector = mkSelector "dateWithTimeIntervalSinceReferenceDate:"

-- | @Selector@ for @dateWithTimeIntervalSince1970:@
dateWithTimeIntervalSince1970Selector :: Selector '[CDouble] (Id NSDate)
dateWithTimeIntervalSince1970Selector = mkSelector "dateWithTimeIntervalSince1970:"

-- | @Selector@ for @dateWithTimeInterval:sinceDate:@
dateWithTimeInterval_sinceDateSelector :: Selector '[CDouble, Id NSDate] (Id NSDate)
dateWithTimeInterval_sinceDateSelector = mkSelector "dateWithTimeInterval:sinceDate:"

-- | @Selector@ for @initWithTimeIntervalSinceNow:@
initWithTimeIntervalSinceNowSelector :: Selector '[CDouble] (Id NSDate)
initWithTimeIntervalSinceNowSelector = mkSelector "initWithTimeIntervalSinceNow:"

-- | @Selector@ for @initWithTimeIntervalSince1970:@
initWithTimeIntervalSince1970Selector :: Selector '[CDouble] (Id NSDate)
initWithTimeIntervalSince1970Selector = mkSelector "initWithTimeIntervalSince1970:"

-- | @Selector@ for @initWithTimeInterval:sinceDate:@
initWithTimeInterval_sinceDateSelector :: Selector '[CDouble, Id NSDate] (Id NSDate)
initWithTimeInterval_sinceDateSelector = mkSelector "initWithTimeInterval:sinceDate:"

-- | @Selector@ for @timeIntervalSinceDate:@
timeIntervalSinceDateSelector :: Selector '[Id NSDate] CDouble
timeIntervalSinceDateSelector = mkSelector "timeIntervalSinceDate:"

-- | @Selector@ for @addTimeInterval:@
addTimeIntervalSelector :: Selector '[CDouble] RawId
addTimeIntervalSelector = mkSelector "addTimeInterval:"

-- | @Selector@ for @dateByAddingTimeInterval:@
dateByAddingTimeIntervalSelector :: Selector '[CDouble] (Id NSDate)
dateByAddingTimeIntervalSelector = mkSelector "dateByAddingTimeInterval:"

-- | @Selector@ for @earlierDate:@
earlierDateSelector :: Selector '[Id NSDate] (Id NSDate)
earlierDateSelector = mkSelector "earlierDate:"

-- | @Selector@ for @laterDate:@
laterDateSelector :: Selector '[Id NSDate] (Id NSDate)
laterDateSelector = mkSelector "laterDate:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSDate] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToDate:@
isEqualToDateSelector :: Selector '[Id NSDate] Bool
isEqualToDateSelector = mkSelector "isEqualToDate:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @timeIntervalSinceReferenceDate@
timeIntervalSinceReferenceDateSelector :: Selector '[] CDouble
timeIntervalSinceReferenceDateSelector = mkSelector "timeIntervalSinceReferenceDate"

-- | @Selector@ for @distantFuture@
distantFutureSelector :: Selector '[] (Id NSDate)
distantFutureSelector = mkSelector "distantFuture"

-- | @Selector@ for @distantPast@
distantPastSelector :: Selector '[] (Id NSDate)
distantPastSelector = mkSelector "distantPast"

-- | @Selector@ for @now@
nowSelector :: Selector '[] (Id NSDate)
nowSelector = mkSelector "now"

-- | @Selector@ for @timeIntervalSinceNow@
timeIntervalSinceNowSelector :: Selector '[] CDouble
timeIntervalSinceNowSelector = mkSelector "timeIntervalSinceNow"

-- | @Selector@ for @timeIntervalSince1970@
timeIntervalSince1970Selector :: Selector '[] CDouble
timeIntervalSince1970Selector = mkSelector "timeIntervalSince1970"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @timeIntervalSinceReferenceDate@
nsDateTimeIntervalSinceReferenceDateSelector :: Selector '[] CDouble
nsDateTimeIntervalSinceReferenceDateSelector = mkSelector "timeIntervalSinceReferenceDate"

