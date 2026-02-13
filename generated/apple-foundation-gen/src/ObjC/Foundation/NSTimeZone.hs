{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTimeZone@.
module ObjC.Foundation.NSTimeZone
  ( NSTimeZone
  , IsNSTimeZone(..)
  , secondsFromGMTForDate
  , abbreviationForDate
  , isDaylightSavingTimeForDate
  , daylightSavingTimeOffsetForDate
  , nextDaylightSavingTimeTransitionAfterDate
  , timeZoneWithName
  , timeZoneWithName_data
  , initWithName
  , initWithName_data
  , timeZoneForSecondsFromGMT
  , timeZoneWithAbbreviation
  , resetSystemTimeZone
  , abbreviationDictionary
  , isEqualToTimeZone
  , localizedName_locale
  , name
  , data_
  , systemTimeZone
  , defaultTimeZone
  , setDefaultTimeZone
  , localTimeZone
  , knownTimeZoneNames
  , setAbbreviationDictionary
  , timeZoneDataVersion
  , secondsFromGMT
  , abbreviation
  , daylightSavingTime
  , daylightSavingTimeOffset
  , nextDaylightSavingTimeTransition
  , description
  , abbreviationDictionarySelector
  , abbreviationForDateSelector
  , abbreviationSelector
  , dataSelector
  , daylightSavingTimeOffsetForDateSelector
  , daylightSavingTimeOffsetSelector
  , daylightSavingTimeSelector
  , defaultTimeZoneSelector
  , descriptionSelector
  , initWithNameSelector
  , initWithName_dataSelector
  , isDaylightSavingTimeForDateSelector
  , isEqualToTimeZoneSelector
  , knownTimeZoneNamesSelector
  , localTimeZoneSelector
  , localizedName_localeSelector
  , nameSelector
  , nextDaylightSavingTimeTransitionAfterDateSelector
  , nextDaylightSavingTimeTransitionSelector
  , resetSystemTimeZoneSelector
  , secondsFromGMTForDateSelector
  , secondsFromGMTSelector
  , setAbbreviationDictionarySelector
  , setDefaultTimeZoneSelector
  , systemTimeZoneSelector
  , timeZoneDataVersionSelector
  , timeZoneForSecondsFromGMTSelector
  , timeZoneWithAbbreviationSelector
  , timeZoneWithNameSelector
  , timeZoneWithName_dataSelector

  -- * Enum types
  , NSTimeZoneNameStyle(NSTimeZoneNameStyle)
  , pattern NSTimeZoneNameStyleStandard
  , pattern NSTimeZoneNameStyleShortStandard
  , pattern NSTimeZoneNameStyleDaylightSaving
  , pattern NSTimeZoneNameStyleShortDaylightSaving
  , pattern NSTimeZoneNameStyleGeneric
  , pattern NSTimeZoneNameStyleShortGeneric

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- secondsFromGMTForDate:@
secondsFromGMTForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO CLong
secondsFromGMTForDate nsTimeZone aDate =
  sendMessage nsTimeZone secondsFromGMTForDateSelector (toNSDate aDate)

-- | @- abbreviationForDate:@
abbreviationForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO (Id NSString)
abbreviationForDate nsTimeZone aDate =
  sendMessage nsTimeZone abbreviationForDateSelector (toNSDate aDate)

-- | @- isDaylightSavingTimeForDate:@
isDaylightSavingTimeForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO Bool
isDaylightSavingTimeForDate nsTimeZone aDate =
  sendMessage nsTimeZone isDaylightSavingTimeForDateSelector (toNSDate aDate)

-- | @- daylightSavingTimeOffsetForDate:@
daylightSavingTimeOffsetForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO CDouble
daylightSavingTimeOffsetForDate nsTimeZone aDate =
  sendMessage nsTimeZone daylightSavingTimeOffsetForDateSelector (toNSDate aDate)

-- | @- nextDaylightSavingTimeTransitionAfterDate:@
nextDaylightSavingTimeTransitionAfterDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO (Id NSDate)
nextDaylightSavingTimeTransitionAfterDate nsTimeZone aDate =
  sendMessage nsTimeZone nextDaylightSavingTimeTransitionAfterDateSelector (toNSDate aDate)

-- | @+ timeZoneWithName:@
timeZoneWithName :: IsNSString tzName => tzName -> IO (Id NSTimeZone)
timeZoneWithName tzName =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' timeZoneWithNameSelector (toNSString tzName)

-- | @+ timeZoneWithName:data:@
timeZoneWithName_data :: (IsNSString tzName, IsNSData aData) => tzName -> aData -> IO (Id NSTimeZone)
timeZoneWithName_data tzName aData =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' timeZoneWithName_dataSelector (toNSString tzName) (toNSData aData)

-- | @- initWithName:@
initWithName :: (IsNSTimeZone nsTimeZone, IsNSString tzName) => nsTimeZone -> tzName -> IO (Id NSTimeZone)
initWithName nsTimeZone tzName =
  sendOwnedMessage nsTimeZone initWithNameSelector (toNSString tzName)

-- | @- initWithName:data:@
initWithName_data :: (IsNSTimeZone nsTimeZone, IsNSString tzName, IsNSData aData) => nsTimeZone -> tzName -> aData -> IO (Id NSTimeZone)
initWithName_data nsTimeZone tzName aData =
  sendOwnedMessage nsTimeZone initWithName_dataSelector (toNSString tzName) (toNSData aData)

-- | @+ timeZoneForSecondsFromGMT:@
timeZoneForSecondsFromGMT :: CLong -> IO (Id NSTimeZone)
timeZoneForSecondsFromGMT seconds =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' timeZoneForSecondsFromGMTSelector seconds

-- | @+ timeZoneWithAbbreviation:@
timeZoneWithAbbreviation :: IsNSString abbreviation => abbreviation -> IO (Id NSTimeZone)
timeZoneWithAbbreviation abbreviation =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' timeZoneWithAbbreviationSelector (toNSString abbreviation)

-- | @+ resetSystemTimeZone@
resetSystemTimeZone :: IO ()
resetSystemTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' resetSystemTimeZoneSelector

-- | @+ abbreviationDictionary@
abbreviationDictionary :: IO (Id NSDictionary)
abbreviationDictionary  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' abbreviationDictionarySelector

-- | @- isEqualToTimeZone:@
isEqualToTimeZone :: (IsNSTimeZone nsTimeZone, IsNSTimeZone aTimeZone) => nsTimeZone -> aTimeZone -> IO Bool
isEqualToTimeZone nsTimeZone aTimeZone =
  sendMessage nsTimeZone isEqualToTimeZoneSelector (toNSTimeZone aTimeZone)

-- | @- localizedName:locale:@
localizedName_locale :: (IsNSTimeZone nsTimeZone, IsNSLocale locale) => nsTimeZone -> NSTimeZoneNameStyle -> locale -> IO (Id NSString)
localizedName_locale nsTimeZone style locale =
  sendMessage nsTimeZone localizedName_localeSelector style (toNSLocale locale)

-- | @- name@
name :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
name nsTimeZone =
  sendMessage nsTimeZone nameSelector

-- | @- data@
data_ :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSData)
data_ nsTimeZone =
  sendMessage nsTimeZone dataSelector

-- | @+ systemTimeZone@
systemTimeZone :: IO (Id NSTimeZone)
systemTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' systemTimeZoneSelector

-- | @+ defaultTimeZone@
defaultTimeZone :: IO (Id NSTimeZone)
defaultTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' defaultTimeZoneSelector

-- | @+ setDefaultTimeZone:@
setDefaultTimeZone :: IsNSTimeZone value => value -> IO ()
setDefaultTimeZone value =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' setDefaultTimeZoneSelector (toNSTimeZone value)

-- | @+ localTimeZone@
localTimeZone :: IO (Id NSTimeZone)
localTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' localTimeZoneSelector

-- | @+ knownTimeZoneNames@
knownTimeZoneNames :: IO (Id NSArray)
knownTimeZoneNames  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' knownTimeZoneNamesSelector

-- | @+ setAbbreviationDictionary:@
setAbbreviationDictionary :: IsNSDictionary value => value -> IO ()
setAbbreviationDictionary value =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' setAbbreviationDictionarySelector (toNSDictionary value)

-- | @+ timeZoneDataVersion@
timeZoneDataVersion :: IO (Id NSString)
timeZoneDataVersion  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMessage cls' timeZoneDataVersionSelector

-- | @- secondsFromGMT@
secondsFromGMT :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO CLong
secondsFromGMT nsTimeZone =
  sendMessage nsTimeZone secondsFromGMTSelector

-- | @- abbreviation@
abbreviation :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
abbreviation nsTimeZone =
  sendMessage nsTimeZone abbreviationSelector

-- | @- daylightSavingTime@
daylightSavingTime :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO Bool
daylightSavingTime nsTimeZone =
  sendMessage nsTimeZone daylightSavingTimeSelector

-- | @- daylightSavingTimeOffset@
daylightSavingTimeOffset :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO CDouble
daylightSavingTimeOffset nsTimeZone =
  sendMessage nsTimeZone daylightSavingTimeOffsetSelector

-- | @- nextDaylightSavingTimeTransition@
nextDaylightSavingTimeTransition :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSDate)
nextDaylightSavingTimeTransition nsTimeZone =
  sendMessage nsTimeZone nextDaylightSavingTimeTransitionSelector

-- | @- description@
description :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
description nsTimeZone =
  sendMessage nsTimeZone descriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @secondsFromGMTForDate:@
secondsFromGMTForDateSelector :: Selector '[Id NSDate] CLong
secondsFromGMTForDateSelector = mkSelector "secondsFromGMTForDate:"

-- | @Selector@ for @abbreviationForDate:@
abbreviationForDateSelector :: Selector '[Id NSDate] (Id NSString)
abbreviationForDateSelector = mkSelector "abbreviationForDate:"

-- | @Selector@ for @isDaylightSavingTimeForDate:@
isDaylightSavingTimeForDateSelector :: Selector '[Id NSDate] Bool
isDaylightSavingTimeForDateSelector = mkSelector "isDaylightSavingTimeForDate:"

-- | @Selector@ for @daylightSavingTimeOffsetForDate:@
daylightSavingTimeOffsetForDateSelector :: Selector '[Id NSDate] CDouble
daylightSavingTimeOffsetForDateSelector = mkSelector "daylightSavingTimeOffsetForDate:"

-- | @Selector@ for @nextDaylightSavingTimeTransitionAfterDate:@
nextDaylightSavingTimeTransitionAfterDateSelector :: Selector '[Id NSDate] (Id NSDate)
nextDaylightSavingTimeTransitionAfterDateSelector = mkSelector "nextDaylightSavingTimeTransitionAfterDate:"

-- | @Selector@ for @timeZoneWithName:@
timeZoneWithNameSelector :: Selector '[Id NSString] (Id NSTimeZone)
timeZoneWithNameSelector = mkSelector "timeZoneWithName:"

-- | @Selector@ for @timeZoneWithName:data:@
timeZoneWithName_dataSelector :: Selector '[Id NSString, Id NSData] (Id NSTimeZone)
timeZoneWithName_dataSelector = mkSelector "timeZoneWithName:data:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id NSTimeZone)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:data:@
initWithName_dataSelector :: Selector '[Id NSString, Id NSData] (Id NSTimeZone)
initWithName_dataSelector = mkSelector "initWithName:data:"

-- | @Selector@ for @timeZoneForSecondsFromGMT:@
timeZoneForSecondsFromGMTSelector :: Selector '[CLong] (Id NSTimeZone)
timeZoneForSecondsFromGMTSelector = mkSelector "timeZoneForSecondsFromGMT:"

-- | @Selector@ for @timeZoneWithAbbreviation:@
timeZoneWithAbbreviationSelector :: Selector '[Id NSString] (Id NSTimeZone)
timeZoneWithAbbreviationSelector = mkSelector "timeZoneWithAbbreviation:"

-- | @Selector@ for @resetSystemTimeZone@
resetSystemTimeZoneSelector :: Selector '[] ()
resetSystemTimeZoneSelector = mkSelector "resetSystemTimeZone"

-- | @Selector@ for @abbreviationDictionary@
abbreviationDictionarySelector :: Selector '[] (Id NSDictionary)
abbreviationDictionarySelector = mkSelector "abbreviationDictionary"

-- | @Selector@ for @isEqualToTimeZone:@
isEqualToTimeZoneSelector :: Selector '[Id NSTimeZone] Bool
isEqualToTimeZoneSelector = mkSelector "isEqualToTimeZone:"

-- | @Selector@ for @localizedName:locale:@
localizedName_localeSelector :: Selector '[NSTimeZoneNameStyle, Id NSLocale] (Id NSString)
localizedName_localeSelector = mkSelector "localizedName:locale:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @systemTimeZone@
systemTimeZoneSelector :: Selector '[] (Id NSTimeZone)
systemTimeZoneSelector = mkSelector "systemTimeZone"

-- | @Selector@ for @defaultTimeZone@
defaultTimeZoneSelector :: Selector '[] (Id NSTimeZone)
defaultTimeZoneSelector = mkSelector "defaultTimeZone"

-- | @Selector@ for @setDefaultTimeZone:@
setDefaultTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setDefaultTimeZoneSelector = mkSelector "setDefaultTimeZone:"

-- | @Selector@ for @localTimeZone@
localTimeZoneSelector :: Selector '[] (Id NSTimeZone)
localTimeZoneSelector = mkSelector "localTimeZone"

-- | @Selector@ for @knownTimeZoneNames@
knownTimeZoneNamesSelector :: Selector '[] (Id NSArray)
knownTimeZoneNamesSelector = mkSelector "knownTimeZoneNames"

-- | @Selector@ for @setAbbreviationDictionary:@
setAbbreviationDictionarySelector :: Selector '[Id NSDictionary] ()
setAbbreviationDictionarySelector = mkSelector "setAbbreviationDictionary:"

-- | @Selector@ for @timeZoneDataVersion@
timeZoneDataVersionSelector :: Selector '[] (Id NSString)
timeZoneDataVersionSelector = mkSelector "timeZoneDataVersion"

-- | @Selector@ for @secondsFromGMT@
secondsFromGMTSelector :: Selector '[] CLong
secondsFromGMTSelector = mkSelector "secondsFromGMT"

-- | @Selector@ for @abbreviation@
abbreviationSelector :: Selector '[] (Id NSString)
abbreviationSelector = mkSelector "abbreviation"

-- | @Selector@ for @daylightSavingTime@
daylightSavingTimeSelector :: Selector '[] Bool
daylightSavingTimeSelector = mkSelector "daylightSavingTime"

-- | @Selector@ for @daylightSavingTimeOffset@
daylightSavingTimeOffsetSelector :: Selector '[] CDouble
daylightSavingTimeOffsetSelector = mkSelector "daylightSavingTimeOffset"

-- | @Selector@ for @nextDaylightSavingTimeTransition@
nextDaylightSavingTimeTransitionSelector :: Selector '[] (Id NSDate)
nextDaylightSavingTimeTransitionSelector = mkSelector "nextDaylightSavingTimeTransition"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

