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
  , megalitersSelector
  , kilolitersSelector
  , litersSelector
  , decilitersSelector
  , centilitersSelector
  , millilitersSelector
  , cubicKilometersSelector
  , cubicMetersSelector
  , cubicDecimetersSelector
  , cubicCentimetersSelector
  , cubicMillimetersSelector
  , cubicInchesSelector
  , cubicFeetSelector
  , cubicYardsSelector
  , cubicMilesSelector
  , acreFeetSelector
  , bushelsSelector
  , teaspoonsSelector
  , tablespoonsSelector
  , fluidOuncesSelector
  , cupsSelector
  , pintsSelector
  , quartsSelector
  , gallonsSelector
  , imperialTeaspoonsSelector
  , imperialTablespoonsSelector
  , imperialFluidOuncesSelector
  , imperialPintsSelector
  , imperialQuartsSelector
  , imperialGallonsSelector
  , metricCupsSelector


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

-- | @+ megaliters@
megaliters :: IO (Id NSUnitVolume)
megaliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "megaliters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kiloliters@
kiloliters :: IO (Id NSUnitVolume)
kiloliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "kiloliters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ liters@
liters :: IO (Id NSUnitVolume)
liters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "liters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deciliters@
deciliters :: IO (Id NSUnitVolume)
deciliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "deciliters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ centiliters@
centiliters :: IO (Id NSUnitVolume)
centiliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "centiliters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliliters@
milliliters :: IO (Id NSUnitVolume)
milliliters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "milliliters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicKilometers@
cubicKilometers :: IO (Id NSUnitVolume)
cubicKilometers  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicKilometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicMeters@
cubicMeters :: IO (Id NSUnitVolume)
cubicMeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicMeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicDecimeters@
cubicDecimeters :: IO (Id NSUnitVolume)
cubicDecimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicDecimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicCentimeters@
cubicCentimeters :: IO (Id NSUnitVolume)
cubicCentimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicCentimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicMillimeters@
cubicMillimeters :: IO (Id NSUnitVolume)
cubicMillimeters  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicMillimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicInches@
cubicInches :: IO (Id NSUnitVolume)
cubicInches  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicInches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicFeet@
cubicFeet :: IO (Id NSUnitVolume)
cubicFeet  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicFeet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicYards@
cubicYards :: IO (Id NSUnitVolume)
cubicYards  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicYards") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cubicMiles@
cubicMiles :: IO (Id NSUnitVolume)
cubicMiles  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cubicMiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ acreFeet@
acreFeet :: IO (Id NSUnitVolume)
acreFeet  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "acreFeet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ bushels@
bushels :: IO (Id NSUnitVolume)
bushels  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "bushels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ teaspoons@
teaspoons :: IO (Id NSUnitVolume)
teaspoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "teaspoons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ tablespoons@
tablespoons :: IO (Id NSUnitVolume)
tablespoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "tablespoons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fluidOunces@
fluidOunces :: IO (Id NSUnitVolume)
fluidOunces  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "fluidOunces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cups@
cups :: IO (Id NSUnitVolume)
cups  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "cups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pints@
pints :: IO (Id NSUnitVolume)
pints  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "pints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ quarts@
quarts :: IO (Id NSUnitVolume)
quarts  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "quarts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gallons@
gallons :: IO (Id NSUnitVolume)
gallons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "gallons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialTeaspoons@
imperialTeaspoons :: IO (Id NSUnitVolume)
imperialTeaspoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialTeaspoons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialTablespoons@
imperialTablespoons :: IO (Id NSUnitVolume)
imperialTablespoons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialTablespoons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialFluidOunces@
imperialFluidOunces :: IO (Id NSUnitVolume)
imperialFluidOunces  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialFluidOunces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialPints@
imperialPints :: IO (Id NSUnitVolume)
imperialPints  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialPints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialQuarts@
imperialQuarts :: IO (Id NSUnitVolume)
imperialQuarts  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialQuarts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ imperialGallons@
imperialGallons :: IO (Id NSUnitVolume)
imperialGallons  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "imperialGallons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ metricCups@
metricCups :: IO (Id NSUnitVolume)
metricCups  =
  do
    cls' <- getRequiredClass "NSUnitVolume"
    sendClassMsg cls' (mkSelector "metricCups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaliters@
megalitersSelector :: Selector
megalitersSelector = mkSelector "megaliters"

-- | @Selector@ for @kiloliters@
kilolitersSelector :: Selector
kilolitersSelector = mkSelector "kiloliters"

-- | @Selector@ for @liters@
litersSelector :: Selector
litersSelector = mkSelector "liters"

-- | @Selector@ for @deciliters@
decilitersSelector :: Selector
decilitersSelector = mkSelector "deciliters"

-- | @Selector@ for @centiliters@
centilitersSelector :: Selector
centilitersSelector = mkSelector "centiliters"

-- | @Selector@ for @milliliters@
millilitersSelector :: Selector
millilitersSelector = mkSelector "milliliters"

-- | @Selector@ for @cubicKilometers@
cubicKilometersSelector :: Selector
cubicKilometersSelector = mkSelector "cubicKilometers"

-- | @Selector@ for @cubicMeters@
cubicMetersSelector :: Selector
cubicMetersSelector = mkSelector "cubicMeters"

-- | @Selector@ for @cubicDecimeters@
cubicDecimetersSelector :: Selector
cubicDecimetersSelector = mkSelector "cubicDecimeters"

-- | @Selector@ for @cubicCentimeters@
cubicCentimetersSelector :: Selector
cubicCentimetersSelector = mkSelector "cubicCentimeters"

-- | @Selector@ for @cubicMillimeters@
cubicMillimetersSelector :: Selector
cubicMillimetersSelector = mkSelector "cubicMillimeters"

-- | @Selector@ for @cubicInches@
cubicInchesSelector :: Selector
cubicInchesSelector = mkSelector "cubicInches"

-- | @Selector@ for @cubicFeet@
cubicFeetSelector :: Selector
cubicFeetSelector = mkSelector "cubicFeet"

-- | @Selector@ for @cubicYards@
cubicYardsSelector :: Selector
cubicYardsSelector = mkSelector "cubicYards"

-- | @Selector@ for @cubicMiles@
cubicMilesSelector :: Selector
cubicMilesSelector = mkSelector "cubicMiles"

-- | @Selector@ for @acreFeet@
acreFeetSelector :: Selector
acreFeetSelector = mkSelector "acreFeet"

-- | @Selector@ for @bushels@
bushelsSelector :: Selector
bushelsSelector = mkSelector "bushels"

-- | @Selector@ for @teaspoons@
teaspoonsSelector :: Selector
teaspoonsSelector = mkSelector "teaspoons"

-- | @Selector@ for @tablespoons@
tablespoonsSelector :: Selector
tablespoonsSelector = mkSelector "tablespoons"

-- | @Selector@ for @fluidOunces@
fluidOuncesSelector :: Selector
fluidOuncesSelector = mkSelector "fluidOunces"

-- | @Selector@ for @cups@
cupsSelector :: Selector
cupsSelector = mkSelector "cups"

-- | @Selector@ for @pints@
pintsSelector :: Selector
pintsSelector = mkSelector "pints"

-- | @Selector@ for @quarts@
quartsSelector :: Selector
quartsSelector = mkSelector "quarts"

-- | @Selector@ for @gallons@
gallonsSelector :: Selector
gallonsSelector = mkSelector "gallons"

-- | @Selector@ for @imperialTeaspoons@
imperialTeaspoonsSelector :: Selector
imperialTeaspoonsSelector = mkSelector "imperialTeaspoons"

-- | @Selector@ for @imperialTablespoons@
imperialTablespoonsSelector :: Selector
imperialTablespoonsSelector = mkSelector "imperialTablespoons"

-- | @Selector@ for @imperialFluidOunces@
imperialFluidOuncesSelector :: Selector
imperialFluidOuncesSelector = mkSelector "imperialFluidOunces"

-- | @Selector@ for @imperialPints@
imperialPintsSelector :: Selector
imperialPintsSelector = mkSelector "imperialPints"

-- | @Selector@ for @imperialQuarts@
imperialQuartsSelector :: Selector
imperialQuartsSelector = mkSelector "imperialQuarts"

-- | @Selector@ for @imperialGallons@
imperialGallonsSelector :: Selector
imperialGallonsSelector = mkSelector "imperialGallons"

-- | @Selector@ for @metricCups@
metricCupsSelector :: Selector
metricCupsSelector = mkSelector "metricCups"

