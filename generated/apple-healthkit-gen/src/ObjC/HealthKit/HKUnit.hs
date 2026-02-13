{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKUnit@.
module ObjC.HealthKit.HKUnit
  ( HKUnit
  , IsHKUnit(..)
  , init_
  , unitFromString
  , unitFromMassFormatterUnit
  , massFormatterUnitFromUnit
  , unitFromLengthFormatterUnit
  , lengthFormatterUnitFromUnit
  , unitFromEnergyFormatterUnit
  , energyFormatterUnitFromUnit
  , isNull
  , appleEffortScoreUnit
  , luxUnitWithMetricPrefix
  , luxUnit
  , radianAngleUnitWithMetricPrefix
  , radianAngleUnit
  , degreeAngleUnit
  , diopterUnit
  , prismDiopterUnit
  , wattUnitWithMetricPrefix
  , wattUnit
  , voltUnitWithMetricPrefix
  , voltUnit
  , hertzUnitWithMetricPrefix
  , hertzUnit
  , unitMultipliedByUnit
  , unitDividedByUnit
  , unitRaisedToPower
  , reciprocalUnit
  , decibelHearingLevelUnit
  , countUnit
  , percentUnit
  , internationalUnit
  , siemenUnitWithMetricPrefix
  , siemenUnit
  , degreeCelsiusUnit
  , degreeFahrenheitUnit
  , kelvinUnit
  , jouleUnitWithMetricPrefix
  , jouleUnit
  , kilocalorieUnit
  , smallCalorieUnit
  , largeCalorieUnit
  , calorieUnit
  , secondUnitWithMetricPrefix
  , secondUnit
  , minuteUnit
  , hourUnit
  , dayUnit
  , pascalUnitWithMetricPrefix
  , pascalUnit
  , millimeterOfMercuryUnit
  , centimeterOfWaterUnit
  , atmosphereUnit
  , decibelAWeightedSoundPressureLevelUnit
  , inchesOfMercuryUnit
  , literUnitWithMetricPrefix
  , literUnit
  , fluidOunceUSUnit
  , fluidOunceImperialUnit
  , pintUSUnit
  , pintImperialUnit
  , cupUSUnit
  , cupImperialUnit
  , meterUnitWithMetricPrefix
  , meterUnit
  , inchUnit
  , footUnit
  , yardUnit
  , mileUnit
  , gramUnitWithMetricPrefix
  , gramUnit
  , ounceUnit
  , poundUnit
  , stoneUnit
  , moleUnitWithMetricPrefix_molarMass
  , moleUnitWithMolarMass
  , unitString
  , appleEffortScoreUnitSelector
  , atmosphereUnitSelector
  , calorieUnitSelector
  , centimeterOfWaterUnitSelector
  , countUnitSelector
  , cupImperialUnitSelector
  , cupUSUnitSelector
  , dayUnitSelector
  , decibelAWeightedSoundPressureLevelUnitSelector
  , decibelHearingLevelUnitSelector
  , degreeAngleUnitSelector
  , degreeCelsiusUnitSelector
  , degreeFahrenheitUnitSelector
  , diopterUnitSelector
  , energyFormatterUnitFromUnitSelector
  , fluidOunceImperialUnitSelector
  , fluidOunceUSUnitSelector
  , footUnitSelector
  , gramUnitSelector
  , gramUnitWithMetricPrefixSelector
  , hertzUnitSelector
  , hertzUnitWithMetricPrefixSelector
  , hourUnitSelector
  , inchUnitSelector
  , inchesOfMercuryUnitSelector
  , initSelector
  , internationalUnitSelector
  , isNullSelector
  , jouleUnitSelector
  , jouleUnitWithMetricPrefixSelector
  , kelvinUnitSelector
  , kilocalorieUnitSelector
  , largeCalorieUnitSelector
  , lengthFormatterUnitFromUnitSelector
  , literUnitSelector
  , literUnitWithMetricPrefixSelector
  , luxUnitSelector
  , luxUnitWithMetricPrefixSelector
  , massFormatterUnitFromUnitSelector
  , meterUnitSelector
  , meterUnitWithMetricPrefixSelector
  , mileUnitSelector
  , millimeterOfMercuryUnitSelector
  , minuteUnitSelector
  , moleUnitWithMetricPrefix_molarMassSelector
  , moleUnitWithMolarMassSelector
  , ounceUnitSelector
  , pascalUnitSelector
  , pascalUnitWithMetricPrefixSelector
  , percentUnitSelector
  , pintImperialUnitSelector
  , pintUSUnitSelector
  , poundUnitSelector
  , prismDiopterUnitSelector
  , radianAngleUnitSelector
  , radianAngleUnitWithMetricPrefixSelector
  , reciprocalUnitSelector
  , secondUnitSelector
  , secondUnitWithMetricPrefixSelector
  , siemenUnitSelector
  , siemenUnitWithMetricPrefixSelector
  , smallCalorieUnitSelector
  , stoneUnitSelector
  , unitDividedByUnitSelector
  , unitFromEnergyFormatterUnitSelector
  , unitFromLengthFormatterUnitSelector
  , unitFromMassFormatterUnitSelector
  , unitFromStringSelector
  , unitMultipliedByUnitSelector
  , unitRaisedToPowerSelector
  , unitStringSelector
  , voltUnitSelector
  , voltUnitWithMetricPrefixSelector
  , wattUnitSelector
  , wattUnitWithMetricPrefixSelector
  , yardUnitSelector

  -- * Enum types
  , HKMetricPrefix(HKMetricPrefix)
  , pattern HKMetricPrefixNone
  , pattern HKMetricPrefixFemto
  , pattern HKMetricPrefixPico
  , pattern HKMetricPrefixNano
  , pattern HKMetricPrefixMicro
  , pattern HKMetricPrefixMilli
  , pattern HKMetricPrefixCenti
  , pattern HKMetricPrefixDeci
  , pattern HKMetricPrefixDeca
  , pattern HKMetricPrefixHecto
  , pattern HKMetricPrefixKilo
  , pattern HKMetricPrefixMega
  , pattern HKMetricPrefixGiga
  , pattern HKMetricPrefixTera
  , NSEnergyFormatterUnit(NSEnergyFormatterUnit)
  , pattern NSEnergyFormatterUnitJoule
  , pattern NSEnergyFormatterUnitKilojoule
  , pattern NSEnergyFormatterUnitCalorie
  , pattern NSEnergyFormatterUnitKilocalorie
  , NSLengthFormatterUnit(NSLengthFormatterUnit)
  , pattern NSLengthFormatterUnitMillimeter
  , pattern NSLengthFormatterUnitCentimeter
  , pattern NSLengthFormatterUnitMeter
  , pattern NSLengthFormatterUnitKilometer
  , pattern NSLengthFormatterUnitInch
  , pattern NSLengthFormatterUnitFoot
  , pattern NSLengthFormatterUnitYard
  , pattern NSLengthFormatterUnitMile
  , NSMassFormatterUnit(NSMassFormatterUnit)
  , pattern NSMassFormatterUnitGram
  , pattern NSMassFormatterUnitKilogram
  , pattern NSMassFormatterUnitOunce
  , pattern NSMassFormatterUnitPound
  , pattern NSMassFormatterUnitStone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKUnit hkUnit => hkUnit -> IO (Id HKUnit)
init_ hkUnit =
  sendOwnedMessage hkUnit initSelector

-- | @+ unitFromString:@
unitFromString :: IsNSString string => string -> IO (Id HKUnit)
unitFromString string =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' unitFromStringSelector (toNSString string)

-- | @+ unitFromMassFormatterUnit:@
unitFromMassFormatterUnit :: NSMassFormatterUnit -> IO (Id HKUnit)
unitFromMassFormatterUnit massFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' unitFromMassFormatterUnitSelector massFormatterUnit

-- | @+ massFormatterUnitFromUnit:@
massFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSMassFormatterUnit
massFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' massFormatterUnitFromUnitSelector (toHKUnit unit)

-- | @+ unitFromLengthFormatterUnit:@
unitFromLengthFormatterUnit :: NSLengthFormatterUnit -> IO (Id HKUnit)
unitFromLengthFormatterUnit lengthFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' unitFromLengthFormatterUnitSelector lengthFormatterUnit

-- | @+ lengthFormatterUnitFromUnit:@
lengthFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSLengthFormatterUnit
lengthFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' lengthFormatterUnitFromUnitSelector (toHKUnit unit)

-- | @+ unitFromEnergyFormatterUnit:@
unitFromEnergyFormatterUnit :: NSEnergyFormatterUnit -> IO (Id HKUnit)
unitFromEnergyFormatterUnit energyFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' unitFromEnergyFormatterUnitSelector energyFormatterUnit

-- | @+ energyFormatterUnitFromUnit:@
energyFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSEnergyFormatterUnit
energyFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' energyFormatterUnitFromUnitSelector (toHKUnit unit)

-- | @- isNull@
isNull :: IsHKUnit hkUnit => hkUnit -> IO Bool
isNull hkUnit =
  sendMessage hkUnit isNullSelector

-- | @+ appleEffortScoreUnit@
appleEffortScoreUnit :: IO (Id HKUnit)
appleEffortScoreUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' appleEffortScoreUnitSelector

-- | @+ luxUnitWithMetricPrefix:@
luxUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
luxUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' luxUnitWithMetricPrefixSelector prefix

-- | @+ luxUnit@
luxUnit :: IO (Id HKUnit)
luxUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' luxUnitSelector

-- | @+ radianAngleUnitWithMetricPrefix:@
radianAngleUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
radianAngleUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' radianAngleUnitWithMetricPrefixSelector prefix

-- | @+ radianAngleUnit@
radianAngleUnit :: IO (Id HKUnit)
radianAngleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' radianAngleUnitSelector

-- | @+ degreeAngleUnit@
degreeAngleUnit :: IO (Id HKUnit)
degreeAngleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' degreeAngleUnitSelector

-- | @+ diopterUnit@
diopterUnit :: IO (Id HKUnit)
diopterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' diopterUnitSelector

-- | @+ prismDiopterUnit@
prismDiopterUnit :: IO (Id HKUnit)
prismDiopterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' prismDiopterUnitSelector

-- | @+ wattUnitWithMetricPrefix:@
wattUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
wattUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' wattUnitWithMetricPrefixSelector prefix

-- | @+ wattUnit@
wattUnit :: IO (Id HKUnit)
wattUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' wattUnitSelector

-- | @+ voltUnitWithMetricPrefix:@
voltUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
voltUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' voltUnitWithMetricPrefixSelector prefix

-- | @+ voltUnit@
voltUnit :: IO (Id HKUnit)
voltUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' voltUnitSelector

-- | @+ hertzUnitWithMetricPrefix:@
hertzUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
hertzUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' hertzUnitWithMetricPrefixSelector prefix

-- | @+ hertzUnit@
hertzUnit :: IO (Id HKUnit)
hertzUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' hertzUnitSelector

-- | @- unitMultipliedByUnit:@
unitMultipliedByUnit :: (IsHKUnit hkUnit, IsHKUnit unit) => hkUnit -> unit -> IO (Id HKUnit)
unitMultipliedByUnit hkUnit unit =
  sendMessage hkUnit unitMultipliedByUnitSelector (toHKUnit unit)

-- | @- unitDividedByUnit:@
unitDividedByUnit :: (IsHKUnit hkUnit, IsHKUnit unit) => hkUnit -> unit -> IO (Id HKUnit)
unitDividedByUnit hkUnit unit =
  sendMessage hkUnit unitDividedByUnitSelector (toHKUnit unit)

-- | @- unitRaisedToPower:@
unitRaisedToPower :: IsHKUnit hkUnit => hkUnit -> CLong -> IO (Id HKUnit)
unitRaisedToPower hkUnit power =
  sendMessage hkUnit unitRaisedToPowerSelector power

-- | @- reciprocalUnit@
reciprocalUnit :: IsHKUnit hkUnit => hkUnit -> IO (Id HKUnit)
reciprocalUnit hkUnit =
  sendMessage hkUnit reciprocalUnitSelector

-- | @+ decibelHearingLevelUnit@
decibelHearingLevelUnit :: IO (Id HKUnit)
decibelHearingLevelUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' decibelHearingLevelUnitSelector

-- | @+ countUnit@
countUnit :: IO (Id HKUnit)
countUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' countUnitSelector

-- | @+ percentUnit@
percentUnit :: IO (Id HKUnit)
percentUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' percentUnitSelector

-- | @+ internationalUnit@
internationalUnit :: IO (Id HKUnit)
internationalUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' internationalUnitSelector

-- | @+ siemenUnitWithMetricPrefix:@
siemenUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
siemenUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' siemenUnitWithMetricPrefixSelector prefix

-- | @+ siemenUnit@
siemenUnit :: IO (Id HKUnit)
siemenUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' siemenUnitSelector

-- | @+ degreeCelsiusUnit@
degreeCelsiusUnit :: IO (Id HKUnit)
degreeCelsiusUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' degreeCelsiusUnitSelector

-- | @+ degreeFahrenheitUnit@
degreeFahrenheitUnit :: IO (Id HKUnit)
degreeFahrenheitUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' degreeFahrenheitUnitSelector

-- | @+ kelvinUnit@
kelvinUnit :: IO (Id HKUnit)
kelvinUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' kelvinUnitSelector

-- | @+ jouleUnitWithMetricPrefix:@
jouleUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
jouleUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' jouleUnitWithMetricPrefixSelector prefix

-- | @+ jouleUnit@
jouleUnit :: IO (Id HKUnit)
jouleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' jouleUnitSelector

-- | @+ kilocalorieUnit@
kilocalorieUnit :: IO (Id HKUnit)
kilocalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' kilocalorieUnitSelector

-- | @+ smallCalorieUnit@
smallCalorieUnit :: IO (Id HKUnit)
smallCalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' smallCalorieUnitSelector

-- | @+ largeCalorieUnit@
largeCalorieUnit :: IO (Id HKUnit)
largeCalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' largeCalorieUnitSelector

-- | @+ calorieUnit@
calorieUnit :: IO (Id HKUnit)
calorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' calorieUnitSelector

-- | @+ secondUnitWithMetricPrefix:@
secondUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
secondUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' secondUnitWithMetricPrefixSelector prefix

-- | @+ secondUnit@
secondUnit :: IO (Id HKUnit)
secondUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' secondUnitSelector

-- | @+ minuteUnit@
minuteUnit :: IO (Id HKUnit)
minuteUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' minuteUnitSelector

-- | @+ hourUnit@
hourUnit :: IO (Id HKUnit)
hourUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' hourUnitSelector

-- | @+ dayUnit@
dayUnit :: IO (Id HKUnit)
dayUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' dayUnitSelector

-- | @+ pascalUnitWithMetricPrefix:@
pascalUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
pascalUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' pascalUnitWithMetricPrefixSelector prefix

-- | @+ pascalUnit@
pascalUnit :: IO (Id HKUnit)
pascalUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' pascalUnitSelector

-- | @+ millimeterOfMercuryUnit@
millimeterOfMercuryUnit :: IO (Id HKUnit)
millimeterOfMercuryUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' millimeterOfMercuryUnitSelector

-- | @+ centimeterOfWaterUnit@
centimeterOfWaterUnit :: IO (Id HKUnit)
centimeterOfWaterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' centimeterOfWaterUnitSelector

-- | @+ atmosphereUnit@
atmosphereUnit :: IO (Id HKUnit)
atmosphereUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' atmosphereUnitSelector

-- | @+ decibelAWeightedSoundPressureLevelUnit@
decibelAWeightedSoundPressureLevelUnit :: IO (Id HKUnit)
decibelAWeightedSoundPressureLevelUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' decibelAWeightedSoundPressureLevelUnitSelector

-- | @+ inchesOfMercuryUnit@
inchesOfMercuryUnit :: IO (Id HKUnit)
inchesOfMercuryUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' inchesOfMercuryUnitSelector

-- | @+ literUnitWithMetricPrefix:@
literUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
literUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' literUnitWithMetricPrefixSelector prefix

-- | @+ literUnit@
literUnit :: IO (Id HKUnit)
literUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' literUnitSelector

-- | @+ fluidOunceUSUnit@
fluidOunceUSUnit :: IO (Id HKUnit)
fluidOunceUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' fluidOunceUSUnitSelector

-- | @+ fluidOunceImperialUnit@
fluidOunceImperialUnit :: IO (Id HKUnit)
fluidOunceImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' fluidOunceImperialUnitSelector

-- | @+ pintUSUnit@
pintUSUnit :: IO (Id HKUnit)
pintUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' pintUSUnitSelector

-- | @+ pintImperialUnit@
pintImperialUnit :: IO (Id HKUnit)
pintImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' pintImperialUnitSelector

-- | @+ cupUSUnit@
cupUSUnit :: IO (Id HKUnit)
cupUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' cupUSUnitSelector

-- | @+ cupImperialUnit@
cupImperialUnit :: IO (Id HKUnit)
cupImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' cupImperialUnitSelector

-- | @+ meterUnitWithMetricPrefix:@
meterUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
meterUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' meterUnitWithMetricPrefixSelector prefix

-- | @+ meterUnit@
meterUnit :: IO (Id HKUnit)
meterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' meterUnitSelector

-- | @+ inchUnit@
inchUnit :: IO (Id HKUnit)
inchUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' inchUnitSelector

-- | @+ footUnit@
footUnit :: IO (Id HKUnit)
footUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' footUnitSelector

-- | @+ yardUnit@
yardUnit :: IO (Id HKUnit)
yardUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' yardUnitSelector

-- | @+ mileUnit@
mileUnit :: IO (Id HKUnit)
mileUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' mileUnitSelector

-- | @+ gramUnitWithMetricPrefix:@
gramUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
gramUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' gramUnitWithMetricPrefixSelector prefix

-- | @+ gramUnit@
gramUnit :: IO (Id HKUnit)
gramUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' gramUnitSelector

-- | @+ ounceUnit@
ounceUnit :: IO (Id HKUnit)
ounceUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' ounceUnitSelector

-- | @+ poundUnit@
poundUnit :: IO (Id HKUnit)
poundUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' poundUnitSelector

-- | @+ stoneUnit@
stoneUnit :: IO (Id HKUnit)
stoneUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' stoneUnitSelector

-- | @+ moleUnitWithMetricPrefix:molarMass:@
moleUnitWithMetricPrefix_molarMass :: HKMetricPrefix -> CDouble -> IO (Id HKUnit)
moleUnitWithMetricPrefix_molarMass prefix gramsPerMole =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' moleUnitWithMetricPrefix_molarMassSelector prefix gramsPerMole

-- | @+ moleUnitWithMolarMass:@
moleUnitWithMolarMass :: CDouble -> IO (Id HKUnit)
moleUnitWithMolarMass gramsPerMole =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMessage cls' moleUnitWithMolarMassSelector gramsPerMole

-- | Returns a unique string representation for the unit that could be used with +unitFromString:
--
-- ObjC selector: @- unitString@
unitString :: IsHKUnit hkUnit => hkUnit -> IO (Id NSString)
unitString hkUnit =
  sendMessage hkUnit unitStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKUnit)
initSelector = mkSelector "init"

-- | @Selector@ for @unitFromString:@
unitFromStringSelector :: Selector '[Id NSString] (Id HKUnit)
unitFromStringSelector = mkSelector "unitFromString:"

-- | @Selector@ for @unitFromMassFormatterUnit:@
unitFromMassFormatterUnitSelector :: Selector '[NSMassFormatterUnit] (Id HKUnit)
unitFromMassFormatterUnitSelector = mkSelector "unitFromMassFormatterUnit:"

-- | @Selector@ for @massFormatterUnitFromUnit:@
massFormatterUnitFromUnitSelector :: Selector '[Id HKUnit] NSMassFormatterUnit
massFormatterUnitFromUnitSelector = mkSelector "massFormatterUnitFromUnit:"

-- | @Selector@ for @unitFromLengthFormatterUnit:@
unitFromLengthFormatterUnitSelector :: Selector '[NSLengthFormatterUnit] (Id HKUnit)
unitFromLengthFormatterUnitSelector = mkSelector "unitFromLengthFormatterUnit:"

-- | @Selector@ for @lengthFormatterUnitFromUnit:@
lengthFormatterUnitFromUnitSelector :: Selector '[Id HKUnit] NSLengthFormatterUnit
lengthFormatterUnitFromUnitSelector = mkSelector "lengthFormatterUnitFromUnit:"

-- | @Selector@ for @unitFromEnergyFormatterUnit:@
unitFromEnergyFormatterUnitSelector :: Selector '[NSEnergyFormatterUnit] (Id HKUnit)
unitFromEnergyFormatterUnitSelector = mkSelector "unitFromEnergyFormatterUnit:"

-- | @Selector@ for @energyFormatterUnitFromUnit:@
energyFormatterUnitFromUnitSelector :: Selector '[Id HKUnit] NSEnergyFormatterUnit
energyFormatterUnitFromUnitSelector = mkSelector "energyFormatterUnitFromUnit:"

-- | @Selector@ for @isNull@
isNullSelector :: Selector '[] Bool
isNullSelector = mkSelector "isNull"

-- | @Selector@ for @appleEffortScoreUnit@
appleEffortScoreUnitSelector :: Selector '[] (Id HKUnit)
appleEffortScoreUnitSelector = mkSelector "appleEffortScoreUnit"

-- | @Selector@ for @luxUnitWithMetricPrefix:@
luxUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
luxUnitWithMetricPrefixSelector = mkSelector "luxUnitWithMetricPrefix:"

-- | @Selector@ for @luxUnit@
luxUnitSelector :: Selector '[] (Id HKUnit)
luxUnitSelector = mkSelector "luxUnit"

-- | @Selector@ for @radianAngleUnitWithMetricPrefix:@
radianAngleUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
radianAngleUnitWithMetricPrefixSelector = mkSelector "radianAngleUnitWithMetricPrefix:"

-- | @Selector@ for @radianAngleUnit@
radianAngleUnitSelector :: Selector '[] (Id HKUnit)
radianAngleUnitSelector = mkSelector "radianAngleUnit"

-- | @Selector@ for @degreeAngleUnit@
degreeAngleUnitSelector :: Selector '[] (Id HKUnit)
degreeAngleUnitSelector = mkSelector "degreeAngleUnit"

-- | @Selector@ for @diopterUnit@
diopterUnitSelector :: Selector '[] (Id HKUnit)
diopterUnitSelector = mkSelector "diopterUnit"

-- | @Selector@ for @prismDiopterUnit@
prismDiopterUnitSelector :: Selector '[] (Id HKUnit)
prismDiopterUnitSelector = mkSelector "prismDiopterUnit"

-- | @Selector@ for @wattUnitWithMetricPrefix:@
wattUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
wattUnitWithMetricPrefixSelector = mkSelector "wattUnitWithMetricPrefix:"

-- | @Selector@ for @wattUnit@
wattUnitSelector :: Selector '[] (Id HKUnit)
wattUnitSelector = mkSelector "wattUnit"

-- | @Selector@ for @voltUnitWithMetricPrefix:@
voltUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
voltUnitWithMetricPrefixSelector = mkSelector "voltUnitWithMetricPrefix:"

-- | @Selector@ for @voltUnit@
voltUnitSelector :: Selector '[] (Id HKUnit)
voltUnitSelector = mkSelector "voltUnit"

-- | @Selector@ for @hertzUnitWithMetricPrefix:@
hertzUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
hertzUnitWithMetricPrefixSelector = mkSelector "hertzUnitWithMetricPrefix:"

-- | @Selector@ for @hertzUnit@
hertzUnitSelector :: Selector '[] (Id HKUnit)
hertzUnitSelector = mkSelector "hertzUnit"

-- | @Selector@ for @unitMultipliedByUnit:@
unitMultipliedByUnitSelector :: Selector '[Id HKUnit] (Id HKUnit)
unitMultipliedByUnitSelector = mkSelector "unitMultipliedByUnit:"

-- | @Selector@ for @unitDividedByUnit:@
unitDividedByUnitSelector :: Selector '[Id HKUnit] (Id HKUnit)
unitDividedByUnitSelector = mkSelector "unitDividedByUnit:"

-- | @Selector@ for @unitRaisedToPower:@
unitRaisedToPowerSelector :: Selector '[CLong] (Id HKUnit)
unitRaisedToPowerSelector = mkSelector "unitRaisedToPower:"

-- | @Selector@ for @reciprocalUnit@
reciprocalUnitSelector :: Selector '[] (Id HKUnit)
reciprocalUnitSelector = mkSelector "reciprocalUnit"

-- | @Selector@ for @decibelHearingLevelUnit@
decibelHearingLevelUnitSelector :: Selector '[] (Id HKUnit)
decibelHearingLevelUnitSelector = mkSelector "decibelHearingLevelUnit"

-- | @Selector@ for @countUnit@
countUnitSelector :: Selector '[] (Id HKUnit)
countUnitSelector = mkSelector "countUnit"

-- | @Selector@ for @percentUnit@
percentUnitSelector :: Selector '[] (Id HKUnit)
percentUnitSelector = mkSelector "percentUnit"

-- | @Selector@ for @internationalUnit@
internationalUnitSelector :: Selector '[] (Id HKUnit)
internationalUnitSelector = mkSelector "internationalUnit"

-- | @Selector@ for @siemenUnitWithMetricPrefix:@
siemenUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
siemenUnitWithMetricPrefixSelector = mkSelector "siemenUnitWithMetricPrefix:"

-- | @Selector@ for @siemenUnit@
siemenUnitSelector :: Selector '[] (Id HKUnit)
siemenUnitSelector = mkSelector "siemenUnit"

-- | @Selector@ for @degreeCelsiusUnit@
degreeCelsiusUnitSelector :: Selector '[] (Id HKUnit)
degreeCelsiusUnitSelector = mkSelector "degreeCelsiusUnit"

-- | @Selector@ for @degreeFahrenheitUnit@
degreeFahrenheitUnitSelector :: Selector '[] (Id HKUnit)
degreeFahrenheitUnitSelector = mkSelector "degreeFahrenheitUnit"

-- | @Selector@ for @kelvinUnit@
kelvinUnitSelector :: Selector '[] (Id HKUnit)
kelvinUnitSelector = mkSelector "kelvinUnit"

-- | @Selector@ for @jouleUnitWithMetricPrefix:@
jouleUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
jouleUnitWithMetricPrefixSelector = mkSelector "jouleUnitWithMetricPrefix:"

-- | @Selector@ for @jouleUnit@
jouleUnitSelector :: Selector '[] (Id HKUnit)
jouleUnitSelector = mkSelector "jouleUnit"

-- | @Selector@ for @kilocalorieUnit@
kilocalorieUnitSelector :: Selector '[] (Id HKUnit)
kilocalorieUnitSelector = mkSelector "kilocalorieUnit"

-- | @Selector@ for @smallCalorieUnit@
smallCalorieUnitSelector :: Selector '[] (Id HKUnit)
smallCalorieUnitSelector = mkSelector "smallCalorieUnit"

-- | @Selector@ for @largeCalorieUnit@
largeCalorieUnitSelector :: Selector '[] (Id HKUnit)
largeCalorieUnitSelector = mkSelector "largeCalorieUnit"

-- | @Selector@ for @calorieUnit@
calorieUnitSelector :: Selector '[] (Id HKUnit)
calorieUnitSelector = mkSelector "calorieUnit"

-- | @Selector@ for @secondUnitWithMetricPrefix:@
secondUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
secondUnitWithMetricPrefixSelector = mkSelector "secondUnitWithMetricPrefix:"

-- | @Selector@ for @secondUnit@
secondUnitSelector :: Selector '[] (Id HKUnit)
secondUnitSelector = mkSelector "secondUnit"

-- | @Selector@ for @minuteUnit@
minuteUnitSelector :: Selector '[] (Id HKUnit)
minuteUnitSelector = mkSelector "minuteUnit"

-- | @Selector@ for @hourUnit@
hourUnitSelector :: Selector '[] (Id HKUnit)
hourUnitSelector = mkSelector "hourUnit"

-- | @Selector@ for @dayUnit@
dayUnitSelector :: Selector '[] (Id HKUnit)
dayUnitSelector = mkSelector "dayUnit"

-- | @Selector@ for @pascalUnitWithMetricPrefix:@
pascalUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
pascalUnitWithMetricPrefixSelector = mkSelector "pascalUnitWithMetricPrefix:"

-- | @Selector@ for @pascalUnit@
pascalUnitSelector :: Selector '[] (Id HKUnit)
pascalUnitSelector = mkSelector "pascalUnit"

-- | @Selector@ for @millimeterOfMercuryUnit@
millimeterOfMercuryUnitSelector :: Selector '[] (Id HKUnit)
millimeterOfMercuryUnitSelector = mkSelector "millimeterOfMercuryUnit"

-- | @Selector@ for @centimeterOfWaterUnit@
centimeterOfWaterUnitSelector :: Selector '[] (Id HKUnit)
centimeterOfWaterUnitSelector = mkSelector "centimeterOfWaterUnit"

-- | @Selector@ for @atmosphereUnit@
atmosphereUnitSelector :: Selector '[] (Id HKUnit)
atmosphereUnitSelector = mkSelector "atmosphereUnit"

-- | @Selector@ for @decibelAWeightedSoundPressureLevelUnit@
decibelAWeightedSoundPressureLevelUnitSelector :: Selector '[] (Id HKUnit)
decibelAWeightedSoundPressureLevelUnitSelector = mkSelector "decibelAWeightedSoundPressureLevelUnit"

-- | @Selector@ for @inchesOfMercuryUnit@
inchesOfMercuryUnitSelector :: Selector '[] (Id HKUnit)
inchesOfMercuryUnitSelector = mkSelector "inchesOfMercuryUnit"

-- | @Selector@ for @literUnitWithMetricPrefix:@
literUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
literUnitWithMetricPrefixSelector = mkSelector "literUnitWithMetricPrefix:"

-- | @Selector@ for @literUnit@
literUnitSelector :: Selector '[] (Id HKUnit)
literUnitSelector = mkSelector "literUnit"

-- | @Selector@ for @fluidOunceUSUnit@
fluidOunceUSUnitSelector :: Selector '[] (Id HKUnit)
fluidOunceUSUnitSelector = mkSelector "fluidOunceUSUnit"

-- | @Selector@ for @fluidOunceImperialUnit@
fluidOunceImperialUnitSelector :: Selector '[] (Id HKUnit)
fluidOunceImperialUnitSelector = mkSelector "fluidOunceImperialUnit"

-- | @Selector@ for @pintUSUnit@
pintUSUnitSelector :: Selector '[] (Id HKUnit)
pintUSUnitSelector = mkSelector "pintUSUnit"

-- | @Selector@ for @pintImperialUnit@
pintImperialUnitSelector :: Selector '[] (Id HKUnit)
pintImperialUnitSelector = mkSelector "pintImperialUnit"

-- | @Selector@ for @cupUSUnit@
cupUSUnitSelector :: Selector '[] (Id HKUnit)
cupUSUnitSelector = mkSelector "cupUSUnit"

-- | @Selector@ for @cupImperialUnit@
cupImperialUnitSelector :: Selector '[] (Id HKUnit)
cupImperialUnitSelector = mkSelector "cupImperialUnit"

-- | @Selector@ for @meterUnitWithMetricPrefix:@
meterUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
meterUnitWithMetricPrefixSelector = mkSelector "meterUnitWithMetricPrefix:"

-- | @Selector@ for @meterUnit@
meterUnitSelector :: Selector '[] (Id HKUnit)
meterUnitSelector = mkSelector "meterUnit"

-- | @Selector@ for @inchUnit@
inchUnitSelector :: Selector '[] (Id HKUnit)
inchUnitSelector = mkSelector "inchUnit"

-- | @Selector@ for @footUnit@
footUnitSelector :: Selector '[] (Id HKUnit)
footUnitSelector = mkSelector "footUnit"

-- | @Selector@ for @yardUnit@
yardUnitSelector :: Selector '[] (Id HKUnit)
yardUnitSelector = mkSelector "yardUnit"

-- | @Selector@ for @mileUnit@
mileUnitSelector :: Selector '[] (Id HKUnit)
mileUnitSelector = mkSelector "mileUnit"

-- | @Selector@ for @gramUnitWithMetricPrefix:@
gramUnitWithMetricPrefixSelector :: Selector '[HKMetricPrefix] (Id HKUnit)
gramUnitWithMetricPrefixSelector = mkSelector "gramUnitWithMetricPrefix:"

-- | @Selector@ for @gramUnit@
gramUnitSelector :: Selector '[] (Id HKUnit)
gramUnitSelector = mkSelector "gramUnit"

-- | @Selector@ for @ounceUnit@
ounceUnitSelector :: Selector '[] (Id HKUnit)
ounceUnitSelector = mkSelector "ounceUnit"

-- | @Selector@ for @poundUnit@
poundUnitSelector :: Selector '[] (Id HKUnit)
poundUnitSelector = mkSelector "poundUnit"

-- | @Selector@ for @stoneUnit@
stoneUnitSelector :: Selector '[] (Id HKUnit)
stoneUnitSelector = mkSelector "stoneUnit"

-- | @Selector@ for @moleUnitWithMetricPrefix:molarMass:@
moleUnitWithMetricPrefix_molarMassSelector :: Selector '[HKMetricPrefix, CDouble] (Id HKUnit)
moleUnitWithMetricPrefix_molarMassSelector = mkSelector "moleUnitWithMetricPrefix:molarMass:"

-- | @Selector@ for @moleUnitWithMolarMass:@
moleUnitWithMolarMassSelector :: Selector '[CDouble] (Id HKUnit)
moleUnitWithMolarMassSelector = mkSelector "moleUnitWithMolarMass:"

-- | @Selector@ for @unitString@
unitStringSelector :: Selector '[] (Id NSString)
unitStringSelector = mkSelector "unitString"

