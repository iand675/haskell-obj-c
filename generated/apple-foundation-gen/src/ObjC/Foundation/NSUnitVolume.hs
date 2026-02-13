{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitVolume@.
module ObjC.Foundation.NSUnitVolume
  ( NSUnitVolume
  , IsNSUnitVolume(..)
  , megaliters
  , kiloliters
  , liters
  , deciliters
  , centiliters
  , milliliters
  , cubicKilometers
  , cubicMeters
  , cubicDecimeters
  , cubicCentimeters
  , cubicMillimeters
  , cubicInches
  , cubicFeet
  , cubicYards
  , cubicMiles
  , acreFeet
  , bushels
  , teaspoons
  , tablespoons
  , fluidOunces
  , cups
  , pints
  , quarts
  , gallons
  , imperialTeaspoons
  , imperialTablespoons
  , imperialFluidOunces
  , imperialPints
  , imperialQuarts
  , imperialGallons
  , metricCups
  , acreFeetSelector
  , bushelsSelector
  , centilitersSelector
  , cubicCentimetersSelector
  , cubicDecimetersSelector
  , cubicFeetSelector
  , cubicInchesSelector
  , cubicKilometersSelector
  , cubicMetersSelector
  , cubicMilesSelector
  , cubicMillimetersSelector
  , cubicYardsSelector
  , cupsSelector
  , decilitersSelector
  , fluidOuncesSelector
  , gallonsSelector
  , imperialFluidOuncesSelector
  , imperialGallonsSelector
  , imperialPintsSelector
  , imperialQuartsSelector
  , imperialTablespoonsSelector
  , imperialTeaspoonsSelector
  , kilolitersSelector
  , litersSelector
  , megalitersSelector
  , metricCupsSelector
  , millilitersSelector
  , pintsSelector
  , quartsSelector
  , tablespoonsSelector
  , teaspoonsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ megaliters@
megaliters :: IO (Id NSUnitVolume)
megaliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' megalitersSelector

-- | @+ kiloliters@
kiloliters :: IO (Id NSUnitVolume)
kiloliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' kilolitersSelector

-- | @+ liters@
liters :: IO (Id NSUnitVolume)
liters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' litersSelector

-- | @+ deciliters@
deciliters :: IO (Id NSUnitVolume)
deciliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' decilitersSelector

-- | @+ centiliters@
centiliters :: IO (Id NSUnitVolume)
centiliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' centilitersSelector

-- | @+ milliliters@
milliliters :: IO (Id NSUnitVolume)
milliliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' millilitersSelector

-- | @+ cubicKilometers@
cubicKilometers :: IO (Id NSUnitVolume)
cubicKilometers  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicKilometersSelector

-- | @+ cubicMeters@
cubicMeters :: IO (Id NSUnitVolume)
cubicMeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicMetersSelector

-- | @+ cubicDecimeters@
cubicDecimeters :: IO (Id NSUnitVolume)
cubicDecimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicDecimetersSelector

-- | @+ cubicCentimeters@
cubicCentimeters :: IO (Id NSUnitVolume)
cubicCentimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicCentimetersSelector

-- | @+ cubicMillimeters@
cubicMillimeters :: IO (Id NSUnitVolume)
cubicMillimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicMillimetersSelector

-- | @+ cubicInches@
cubicInches :: IO (Id NSUnitVolume)
cubicInches  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicInchesSelector

-- | @+ cubicFeet@
cubicFeet :: IO (Id NSUnitVolume)
cubicFeet  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicFeetSelector

-- | @+ cubicYards@
cubicYards :: IO (Id NSUnitVolume)
cubicYards  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicYardsSelector

-- | @+ cubicMiles@
cubicMiles :: IO (Id NSUnitVolume)
cubicMiles  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cubicMilesSelector

-- | @+ acreFeet@
acreFeet :: IO (Id NSUnitVolume)
acreFeet  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' acreFeetSelector

-- | @+ bushels@
bushels :: IO (Id NSUnitVolume)
bushels  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' bushelsSelector

-- | @+ teaspoons@
teaspoons :: IO (Id NSUnitVolume)
teaspoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' teaspoonsSelector

-- | @+ tablespoons@
tablespoons :: IO (Id NSUnitVolume)
tablespoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' tablespoonsSelector

-- | @+ fluidOunces@
fluidOunces :: IO (Id NSUnitVolume)
fluidOunces  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' fluidOuncesSelector

-- | @+ cups@
cups :: IO (Id NSUnitVolume)
cups  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' cupsSelector

-- | @+ pints@
pints :: IO (Id NSUnitVolume)
pints  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' pintsSelector

-- | @+ quarts@
quarts :: IO (Id NSUnitVolume)
quarts  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' quartsSelector

-- | @+ gallons@
gallons :: IO (Id NSUnitVolume)
gallons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' gallonsSelector

-- | @+ imperialTeaspoons@
imperialTeaspoons :: IO (Id NSUnitVolume)
imperialTeaspoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialTeaspoonsSelector

-- | @+ imperialTablespoons@
imperialTablespoons :: IO (Id NSUnitVolume)
imperialTablespoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialTablespoonsSelector

-- | @+ imperialFluidOunces@
imperialFluidOunces :: IO (Id NSUnitVolume)
imperialFluidOunces  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialFluidOuncesSelector

-- | @+ imperialPints@
imperialPints :: IO (Id NSUnitVolume)
imperialPints  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialPintsSelector

-- | @+ imperialQuarts@
imperialQuarts :: IO (Id NSUnitVolume)
imperialQuarts  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialQuartsSelector

-- | @+ imperialGallons@
imperialGallons :: IO (Id NSUnitVolume)
imperialGallons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' imperialGallonsSelector

-- | @+ metricCups@
metricCups :: IO (Id NSUnitVolume)
metricCups  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMessage cls' metricCupsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaliters@
megalitersSelector :: Selector '[] (Id NSUnitVolume)
megalitersSelector = mkSelector "megaliters"

-- | @Selector@ for @kiloliters@
kilolitersSelector :: Selector '[] (Id NSUnitVolume)
kilolitersSelector = mkSelector "kiloliters"

-- | @Selector@ for @liters@
litersSelector :: Selector '[] (Id NSUnitVolume)
litersSelector = mkSelector "liters"

-- | @Selector@ for @deciliters@
decilitersSelector :: Selector '[] (Id NSUnitVolume)
decilitersSelector = mkSelector "deciliters"

-- | @Selector@ for @centiliters@
centilitersSelector :: Selector '[] (Id NSUnitVolume)
centilitersSelector = mkSelector "centiliters"

-- | @Selector@ for @milliliters@
millilitersSelector :: Selector '[] (Id NSUnitVolume)
millilitersSelector = mkSelector "milliliters"

-- | @Selector@ for @cubicKilometers@
cubicKilometersSelector :: Selector '[] (Id NSUnitVolume)
cubicKilometersSelector = mkSelector "cubicKilometers"

-- | @Selector@ for @cubicMeters@
cubicMetersSelector :: Selector '[] (Id NSUnitVolume)
cubicMetersSelector = mkSelector "cubicMeters"

-- | @Selector@ for @cubicDecimeters@
cubicDecimetersSelector :: Selector '[] (Id NSUnitVolume)
cubicDecimetersSelector = mkSelector "cubicDecimeters"

-- | @Selector@ for @cubicCentimeters@
cubicCentimetersSelector :: Selector '[] (Id NSUnitVolume)
cubicCentimetersSelector = mkSelector "cubicCentimeters"

-- | @Selector@ for @cubicMillimeters@
cubicMillimetersSelector :: Selector '[] (Id NSUnitVolume)
cubicMillimetersSelector = mkSelector "cubicMillimeters"

-- | @Selector@ for @cubicInches@
cubicInchesSelector :: Selector '[] (Id NSUnitVolume)
cubicInchesSelector = mkSelector "cubicInches"

-- | @Selector@ for @cubicFeet@
cubicFeetSelector :: Selector '[] (Id NSUnitVolume)
cubicFeetSelector = mkSelector "cubicFeet"

-- | @Selector@ for @cubicYards@
cubicYardsSelector :: Selector '[] (Id NSUnitVolume)
cubicYardsSelector = mkSelector "cubicYards"

-- | @Selector@ for @cubicMiles@
cubicMilesSelector :: Selector '[] (Id NSUnitVolume)
cubicMilesSelector = mkSelector "cubicMiles"

-- | @Selector@ for @acreFeet@
acreFeetSelector :: Selector '[] (Id NSUnitVolume)
acreFeetSelector = mkSelector "acreFeet"

-- | @Selector@ for @bushels@
bushelsSelector :: Selector '[] (Id NSUnitVolume)
bushelsSelector = mkSelector "bushels"

-- | @Selector@ for @teaspoons@
teaspoonsSelector :: Selector '[] (Id NSUnitVolume)
teaspoonsSelector = mkSelector "teaspoons"

-- | @Selector@ for @tablespoons@
tablespoonsSelector :: Selector '[] (Id NSUnitVolume)
tablespoonsSelector = mkSelector "tablespoons"

-- | @Selector@ for @fluidOunces@
fluidOuncesSelector :: Selector '[] (Id NSUnitVolume)
fluidOuncesSelector = mkSelector "fluidOunces"

-- | @Selector@ for @cups@
cupsSelector :: Selector '[] (Id NSUnitVolume)
cupsSelector = mkSelector "cups"

-- | @Selector@ for @pints@
pintsSelector :: Selector '[] (Id NSUnitVolume)
pintsSelector = mkSelector "pints"

-- | @Selector@ for @quarts@
quartsSelector :: Selector '[] (Id NSUnitVolume)
quartsSelector = mkSelector "quarts"

-- | @Selector@ for @gallons@
gallonsSelector :: Selector '[] (Id NSUnitVolume)
gallonsSelector = mkSelector "gallons"

-- | @Selector@ for @imperialTeaspoons@
imperialTeaspoonsSelector :: Selector '[] (Id NSUnitVolume)
imperialTeaspoonsSelector = mkSelector "imperialTeaspoons"

-- | @Selector@ for @imperialTablespoons@
imperialTablespoonsSelector :: Selector '[] (Id NSUnitVolume)
imperialTablespoonsSelector = mkSelector "imperialTablespoons"

-- | @Selector@ for @imperialFluidOunces@
imperialFluidOuncesSelector :: Selector '[] (Id NSUnitVolume)
imperialFluidOuncesSelector = mkSelector "imperialFluidOunces"

-- | @Selector@ for @imperialPints@
imperialPintsSelector :: Selector '[] (Id NSUnitVolume)
imperialPintsSelector = mkSelector "imperialPints"

-- | @Selector@ for @imperialQuarts@
imperialQuartsSelector :: Selector '[] (Id NSUnitVolume)
imperialQuartsSelector = mkSelector "imperialQuarts"

-- | @Selector@ for @imperialGallons@
imperialGallonsSelector :: Selector '[] (Id NSUnitVolume)
imperialGallonsSelector = mkSelector "imperialGallons"

-- | @Selector@ for @metricCups@
metricCupsSelector :: Selector '[] (Id NSUnitVolume)
metricCupsSelector = mkSelector "metricCups"

