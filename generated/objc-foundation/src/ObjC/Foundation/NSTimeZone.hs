{-# LANGUAGE PatternSynonyms #-}
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
  , secondsFromGMTForDateSelector
  , abbreviationForDateSelector
  , isDaylightSavingTimeForDateSelector
  , daylightSavingTimeOffsetForDateSelector
  , nextDaylightSavingTimeTransitionAfterDateSelector
  , timeZoneWithNameSelector
  , timeZoneWithName_dataSelector
  , initWithNameSelector
  , initWithName_dataSelector
  , timeZoneForSecondsFromGMTSelector
  , timeZoneWithAbbreviationSelector
  , resetSystemTimeZoneSelector
  , abbreviationDictionarySelector
  , isEqualToTimeZoneSelector
  , localizedName_localeSelector
  , nameSelector
  , dataSelector
  , systemTimeZoneSelector
  , defaultTimeZoneSelector
  , setDefaultTimeZoneSelector
  , localTimeZoneSelector
  , knownTimeZoneNamesSelector
  , setAbbreviationDictionarySelector
  , timeZoneDataVersionSelector
  , secondsFromGMTSelector
  , abbreviationSelector
  , daylightSavingTimeSelector
  , daylightSavingTimeOffsetSelector
  , nextDaylightSavingTimeTransitionSelector
  , descriptionSelector

  -- * Enum types
  , NSTimeZoneNameStyle(NSTimeZoneNameStyle)
  , pattern NSTimeZoneNameStyleStandard
  , pattern NSTimeZoneNameStyleShortStandard
  , pattern NSTimeZoneNameStyleDaylightSaving
  , pattern NSTimeZoneNameStyleShortDaylightSaving
  , pattern NSTimeZoneNameStyleGeneric
  , pattern NSTimeZoneNameStyleShortGeneric

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

-- | @- secondsFromGMTForDate:@
secondsFromGMTForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO CLong
secondsFromGMTForDate nsTimeZone  aDate =
withObjCPtr aDate $ \raw_aDate ->
    sendMsg nsTimeZone (mkSelector "secondsFromGMTForDate:") retCLong [argPtr (castPtr raw_aDate :: Ptr ())]

-- | @- abbreviationForDate:@
abbreviationForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO (Id NSString)
abbreviationForDate nsTimeZone  aDate =
withObjCPtr aDate $ \raw_aDate ->
    sendMsg nsTimeZone (mkSelector "abbreviationForDate:") (retPtr retVoid) [argPtr (castPtr raw_aDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- isDaylightSavingTimeForDate:@
isDaylightSavingTimeForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO Bool
isDaylightSavingTimeForDate nsTimeZone  aDate =
withObjCPtr aDate $ \raw_aDate ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTimeZone (mkSelector "isDaylightSavingTimeForDate:") retCULong [argPtr (castPtr raw_aDate :: Ptr ())]

-- | @- daylightSavingTimeOffsetForDate:@
daylightSavingTimeOffsetForDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO CDouble
daylightSavingTimeOffsetForDate nsTimeZone  aDate =
withObjCPtr aDate $ \raw_aDate ->
    sendMsg nsTimeZone (mkSelector "daylightSavingTimeOffsetForDate:") retCDouble [argPtr (castPtr raw_aDate :: Ptr ())]

-- | @- nextDaylightSavingTimeTransitionAfterDate:@
nextDaylightSavingTimeTransitionAfterDate :: (IsNSTimeZone nsTimeZone, IsNSDate aDate) => nsTimeZone -> aDate -> IO (Id NSDate)
nextDaylightSavingTimeTransitionAfterDate nsTimeZone  aDate =
withObjCPtr aDate $ \raw_aDate ->
    sendMsg nsTimeZone (mkSelector "nextDaylightSavingTimeTransitionAfterDate:") (retPtr retVoid) [argPtr (castPtr raw_aDate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ timeZoneWithName:@
timeZoneWithName :: IsNSString tzName => tzName -> IO (Id NSTimeZone)
timeZoneWithName tzName =
  do
    cls' <- getRequiredClass "NSTimeZone"
    withObjCPtr tzName $ \raw_tzName ->
      sendClassMsg cls' (mkSelector "timeZoneWithName:") (retPtr retVoid) [argPtr (castPtr raw_tzName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ timeZoneWithName:data:@
timeZoneWithName_data :: (IsNSString tzName, IsNSData aData) => tzName -> aData -> IO (Id NSTimeZone)
timeZoneWithName_data tzName aData =
  do
    cls' <- getRequiredClass "NSTimeZone"
    withObjCPtr tzName $ \raw_tzName ->
      withObjCPtr aData $ \raw_aData ->
        sendClassMsg cls' (mkSelector "timeZoneWithName:data:") (retPtr retVoid) [argPtr (castPtr raw_tzName :: Ptr ()), argPtr (castPtr raw_aData :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsNSTimeZone nsTimeZone, IsNSString tzName) => nsTimeZone -> tzName -> IO (Id NSTimeZone)
initWithName nsTimeZone  tzName =
withObjCPtr tzName $ \raw_tzName ->
    sendMsg nsTimeZone (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_tzName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:data:@
initWithName_data :: (IsNSTimeZone nsTimeZone, IsNSString tzName, IsNSData aData) => nsTimeZone -> tzName -> aData -> IO (Id NSTimeZone)
initWithName_data nsTimeZone  tzName aData =
withObjCPtr tzName $ \raw_tzName ->
  withObjCPtr aData $ \raw_aData ->
      sendMsg nsTimeZone (mkSelector "initWithName:data:") (retPtr retVoid) [argPtr (castPtr raw_tzName :: Ptr ()), argPtr (castPtr raw_aData :: Ptr ())] >>= ownedObject . castPtr

-- | @+ timeZoneForSecondsFromGMT:@
timeZoneForSecondsFromGMT :: CLong -> IO (Id NSTimeZone)
timeZoneForSecondsFromGMT seconds =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "timeZoneForSecondsFromGMT:") (retPtr retVoid) [argCLong (fromIntegral seconds)] >>= retainedObject . castPtr

-- | @+ timeZoneWithAbbreviation:@
timeZoneWithAbbreviation :: IsNSString abbreviation => abbreviation -> IO (Id NSTimeZone)
timeZoneWithAbbreviation abbreviation =
  do
    cls' <- getRequiredClass "NSTimeZone"
    withObjCPtr abbreviation $ \raw_abbreviation ->
      sendClassMsg cls' (mkSelector "timeZoneWithAbbreviation:") (retPtr retVoid) [argPtr (castPtr raw_abbreviation :: Ptr ())] >>= retainedObject . castPtr

-- | @+ resetSystemTimeZone@
resetSystemTimeZone :: IO ()
resetSystemTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "resetSystemTimeZone") retVoid []

-- | @+ abbreviationDictionary@
abbreviationDictionary :: IO (Id NSDictionary)
abbreviationDictionary  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "abbreviationDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isEqualToTimeZone:@
isEqualToTimeZone :: (IsNSTimeZone nsTimeZone, IsNSTimeZone aTimeZone) => nsTimeZone -> aTimeZone -> IO Bool
isEqualToTimeZone nsTimeZone  aTimeZone =
withObjCPtr aTimeZone $ \raw_aTimeZone ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTimeZone (mkSelector "isEqualToTimeZone:") retCULong [argPtr (castPtr raw_aTimeZone :: Ptr ())]

-- | @- localizedName:locale:@
localizedName_locale :: (IsNSTimeZone nsTimeZone, IsNSLocale locale) => nsTimeZone -> NSTimeZoneNameStyle -> locale -> IO (Id NSString)
localizedName_locale nsTimeZone  style locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsTimeZone (mkSelector "localizedName:locale:") (retPtr retVoid) [argCLong (coerce style), argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
name nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSData)
data_ nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemTimeZone@
systemTimeZone :: IO (Id NSTimeZone)
systemTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "systemTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultTimeZone@
defaultTimeZone :: IO (Id NSTimeZone)
defaultTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "defaultTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setDefaultTimeZone:@
setDefaultTimeZone :: IsNSTimeZone value => value -> IO ()
setDefaultTimeZone value =
  do
    cls' <- getRequiredClass "NSTimeZone"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setDefaultTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ localTimeZone@
localTimeZone :: IO (Id NSTimeZone)
localTimeZone  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "localTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ knownTimeZoneNames@
knownTimeZoneNames :: IO (Id NSArray)
knownTimeZoneNames  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "knownTimeZoneNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setAbbreviationDictionary:@
setAbbreviationDictionary :: IsNSDictionary value => value -> IO ()
setAbbreviationDictionary value =
  do
    cls' <- getRequiredClass "NSTimeZone"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setAbbreviationDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ timeZoneDataVersion@
timeZoneDataVersion :: IO (Id NSString)
timeZoneDataVersion  =
  do
    cls' <- getRequiredClass "NSTimeZone"
    sendClassMsg cls' (mkSelector "timeZoneDataVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secondsFromGMT@
secondsFromGMT :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO CLong
secondsFromGMT nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "secondsFromGMT") retCLong []

-- | @- abbreviation@
abbreviation :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
abbreviation nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "abbreviation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- daylightSavingTime@
daylightSavingTime :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO Bool
daylightSavingTime nsTimeZone  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTimeZone (mkSelector "daylightSavingTime") retCULong []

-- | @- daylightSavingTimeOffset@
daylightSavingTimeOffset :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO CDouble
daylightSavingTimeOffset nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "daylightSavingTimeOffset") retCDouble []

-- | @- nextDaylightSavingTimeTransition@
nextDaylightSavingTimeTransition :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSDate)
nextDaylightSavingTimeTransition nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "nextDaylightSavingTimeTransition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsNSTimeZone nsTimeZone => nsTimeZone -> IO (Id NSString)
description nsTimeZone  =
  sendMsg nsTimeZone (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @secondsFromGMTForDate:@
secondsFromGMTForDateSelector :: Selector
secondsFromGMTForDateSelector = mkSelector "secondsFromGMTForDate:"

-- | @Selector@ for @abbreviationForDate:@
abbreviationForDateSelector :: Selector
abbreviationForDateSelector = mkSelector "abbreviationForDate:"

-- | @Selector@ for @isDaylightSavingTimeForDate:@
isDaylightSavingTimeForDateSelector :: Selector
isDaylightSavingTimeForDateSelector = mkSelector "isDaylightSavingTimeForDate:"

-- | @Selector@ for @daylightSavingTimeOffsetForDate:@
daylightSavingTimeOffsetForDateSelector :: Selector
daylightSavingTimeOffsetForDateSelector = mkSelector "daylightSavingTimeOffsetForDate:"

-- | @Selector@ for @nextDaylightSavingTimeTransitionAfterDate:@
nextDaylightSavingTimeTransitionAfterDateSelector :: Selector
nextDaylightSavingTimeTransitionAfterDateSelector = mkSelector "nextDaylightSavingTimeTransitionAfterDate:"

-- | @Selector@ for @timeZoneWithName:@
timeZoneWithNameSelector :: Selector
timeZoneWithNameSelector = mkSelector "timeZoneWithName:"

-- | @Selector@ for @timeZoneWithName:data:@
timeZoneWithName_dataSelector :: Selector
timeZoneWithName_dataSelector = mkSelector "timeZoneWithName:data:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:data:@
initWithName_dataSelector :: Selector
initWithName_dataSelector = mkSelector "initWithName:data:"

-- | @Selector@ for @timeZoneForSecondsFromGMT:@
timeZoneForSecondsFromGMTSelector :: Selector
timeZoneForSecondsFromGMTSelector = mkSelector "timeZoneForSecondsFromGMT:"

-- | @Selector@ for @timeZoneWithAbbreviation:@
timeZoneWithAbbreviationSelector :: Selector
timeZoneWithAbbreviationSelector = mkSelector "timeZoneWithAbbreviation:"

-- | @Selector@ for @resetSystemTimeZone@
resetSystemTimeZoneSelector :: Selector
resetSystemTimeZoneSelector = mkSelector "resetSystemTimeZone"

-- | @Selector@ for @abbreviationDictionary@
abbreviationDictionarySelector :: Selector
abbreviationDictionarySelector = mkSelector "abbreviationDictionary"

-- | @Selector@ for @isEqualToTimeZone:@
isEqualToTimeZoneSelector :: Selector
isEqualToTimeZoneSelector = mkSelector "isEqualToTimeZone:"

-- | @Selector@ for @localizedName:locale:@
localizedName_localeSelector :: Selector
localizedName_localeSelector = mkSelector "localizedName:locale:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @systemTimeZone@
systemTimeZoneSelector :: Selector
systemTimeZoneSelector = mkSelector "systemTimeZone"

-- | @Selector@ for @defaultTimeZone@
defaultTimeZoneSelector :: Selector
defaultTimeZoneSelector = mkSelector "defaultTimeZone"

-- | @Selector@ for @setDefaultTimeZone:@
setDefaultTimeZoneSelector :: Selector
setDefaultTimeZoneSelector = mkSelector "setDefaultTimeZone:"

-- | @Selector@ for @localTimeZone@
localTimeZoneSelector :: Selector
localTimeZoneSelector = mkSelector "localTimeZone"

-- | @Selector@ for @knownTimeZoneNames@
knownTimeZoneNamesSelector :: Selector
knownTimeZoneNamesSelector = mkSelector "knownTimeZoneNames"

-- | @Selector@ for @setAbbreviationDictionary:@
setAbbreviationDictionarySelector :: Selector
setAbbreviationDictionarySelector = mkSelector "setAbbreviationDictionary:"

-- | @Selector@ for @timeZoneDataVersion@
timeZoneDataVersionSelector :: Selector
timeZoneDataVersionSelector = mkSelector "timeZoneDataVersion"

-- | @Selector@ for @secondsFromGMT@
secondsFromGMTSelector :: Selector
secondsFromGMTSelector = mkSelector "secondsFromGMT"

-- | @Selector@ for @abbreviation@
abbreviationSelector :: Selector
abbreviationSelector = mkSelector "abbreviation"

-- | @Selector@ for @daylightSavingTime@
daylightSavingTimeSelector :: Selector
daylightSavingTimeSelector = mkSelector "daylightSavingTime"

-- | @Selector@ for @daylightSavingTimeOffset@
daylightSavingTimeOffsetSelector :: Selector
daylightSavingTimeOffsetSelector = mkSelector "daylightSavingTimeOffset"

-- | @Selector@ for @nextDaylightSavingTimeTransition@
nextDaylightSavingTimeTransitionSelector :: Selector
nextDaylightSavingTimeTransitionSelector = mkSelector "nextDaylightSavingTimeTransition"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

