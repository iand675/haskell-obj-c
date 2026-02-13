{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitLength@.
module ObjC.Foundation.NSUnitLength
  ( NSUnitLength
  , IsNSUnitLength(..)
  , megameters
  , kilometers
  , hectometers
  , decameters
  , meters
  , decimeters
  , centimeters
  , millimeters
  , micrometers
  , nanometers
  , picometers
  , inches
  , feet
  , yards
  , miles
  , scandinavianMiles
  , lightyears
  , nauticalMiles
  , fathoms
  , furlongs
  , astronomicalUnits
  , parsecs
  , astronomicalUnitsSelector
  , centimetersSelector
  , decametersSelector
  , decimetersSelector
  , fathomsSelector
  , feetSelector
  , furlongsSelector
  , hectometersSelector
  , inchesSelector
  , kilometersSelector
  , lightyearsSelector
  , megametersSelector
  , metersSelector
  , micrometersSelector
  , milesSelector
  , millimetersSelector
  , nanometersSelector
  , nauticalMilesSelector
  , parsecsSelector
  , picometersSelector
  , scandinavianMilesSelector
  , yardsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ megameters@
megameters :: IO (Id NSUnitLength)
megameters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' megametersSelector

-- | @+ kilometers@
kilometers :: IO (Id NSUnitLength)
kilometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' kilometersSelector

-- | @+ hectometers@
hectometers :: IO (Id NSUnitLength)
hectometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' hectometersSelector

-- | @+ decameters@
decameters :: IO (Id NSUnitLength)
decameters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' decametersSelector

-- | @+ meters@
meters :: IO (Id NSUnitLength)
meters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' metersSelector

-- | @+ decimeters@
decimeters :: IO (Id NSUnitLength)
decimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' decimetersSelector

-- | @+ centimeters@
centimeters :: IO (Id NSUnitLength)
centimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' centimetersSelector

-- | @+ millimeters@
millimeters :: IO (Id NSUnitLength)
millimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' millimetersSelector

-- | @+ micrometers@
micrometers :: IO (Id NSUnitLength)
micrometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' micrometersSelector

-- | @+ nanometers@
nanometers :: IO (Id NSUnitLength)
nanometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' nanometersSelector

-- | @+ picometers@
picometers :: IO (Id NSUnitLength)
picometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' picometersSelector

-- | @+ inches@
inches :: IO (Id NSUnitLength)
inches  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' inchesSelector

-- | @+ feet@
feet :: IO (Id NSUnitLength)
feet  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' feetSelector

-- | @+ yards@
yards :: IO (Id NSUnitLength)
yards  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' yardsSelector

-- | @+ miles@
miles :: IO (Id NSUnitLength)
miles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' milesSelector

-- | @+ scandinavianMiles@
scandinavianMiles :: IO (Id NSUnitLength)
scandinavianMiles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' scandinavianMilesSelector

-- | @+ lightyears@
lightyears :: IO (Id NSUnitLength)
lightyears  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' lightyearsSelector

-- | @+ nauticalMiles@
nauticalMiles :: IO (Id NSUnitLength)
nauticalMiles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' nauticalMilesSelector

-- | @+ fathoms@
fathoms :: IO (Id NSUnitLength)
fathoms  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' fathomsSelector

-- | @+ furlongs@
furlongs :: IO (Id NSUnitLength)
furlongs  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' furlongsSelector

-- | @+ astronomicalUnits@
astronomicalUnits :: IO (Id NSUnitLength)
astronomicalUnits  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' astronomicalUnitsSelector

-- | @+ parsecs@
parsecs :: IO (Id NSUnitLength)
parsecs  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMessage cls' parsecsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megameters@
megametersSelector :: Selector '[] (Id NSUnitLength)
megametersSelector = mkSelector "megameters"

-- | @Selector@ for @kilometers@
kilometersSelector :: Selector '[] (Id NSUnitLength)
kilometersSelector = mkSelector "kilometers"

-- | @Selector@ for @hectometers@
hectometersSelector :: Selector '[] (Id NSUnitLength)
hectometersSelector = mkSelector "hectometers"

-- | @Selector@ for @decameters@
decametersSelector :: Selector '[] (Id NSUnitLength)
decametersSelector = mkSelector "decameters"

-- | @Selector@ for @meters@
metersSelector :: Selector '[] (Id NSUnitLength)
metersSelector = mkSelector "meters"

-- | @Selector@ for @decimeters@
decimetersSelector :: Selector '[] (Id NSUnitLength)
decimetersSelector = mkSelector "decimeters"

-- | @Selector@ for @centimeters@
centimetersSelector :: Selector '[] (Id NSUnitLength)
centimetersSelector = mkSelector "centimeters"

-- | @Selector@ for @millimeters@
millimetersSelector :: Selector '[] (Id NSUnitLength)
millimetersSelector = mkSelector "millimeters"

-- | @Selector@ for @micrometers@
micrometersSelector :: Selector '[] (Id NSUnitLength)
micrometersSelector = mkSelector "micrometers"

-- | @Selector@ for @nanometers@
nanometersSelector :: Selector '[] (Id NSUnitLength)
nanometersSelector = mkSelector "nanometers"

-- | @Selector@ for @picometers@
picometersSelector :: Selector '[] (Id NSUnitLength)
picometersSelector = mkSelector "picometers"

-- | @Selector@ for @inches@
inchesSelector :: Selector '[] (Id NSUnitLength)
inchesSelector = mkSelector "inches"

-- | @Selector@ for @feet@
feetSelector :: Selector '[] (Id NSUnitLength)
feetSelector = mkSelector "feet"

-- | @Selector@ for @yards@
yardsSelector :: Selector '[] (Id NSUnitLength)
yardsSelector = mkSelector "yards"

-- | @Selector@ for @miles@
milesSelector :: Selector '[] (Id NSUnitLength)
milesSelector = mkSelector "miles"

-- | @Selector@ for @scandinavianMiles@
scandinavianMilesSelector :: Selector '[] (Id NSUnitLength)
scandinavianMilesSelector = mkSelector "scandinavianMiles"

-- | @Selector@ for @lightyears@
lightyearsSelector :: Selector '[] (Id NSUnitLength)
lightyearsSelector = mkSelector "lightyears"

-- | @Selector@ for @nauticalMiles@
nauticalMilesSelector :: Selector '[] (Id NSUnitLength)
nauticalMilesSelector = mkSelector "nauticalMiles"

-- | @Selector@ for @fathoms@
fathomsSelector :: Selector '[] (Id NSUnitLength)
fathomsSelector = mkSelector "fathoms"

-- | @Selector@ for @furlongs@
furlongsSelector :: Selector '[] (Id NSUnitLength)
furlongsSelector = mkSelector "furlongs"

-- | @Selector@ for @astronomicalUnits@
astronomicalUnitsSelector :: Selector '[] (Id NSUnitLength)
astronomicalUnitsSelector = mkSelector "astronomicalUnits"

-- | @Selector@ for @parsecs@
parsecsSelector :: Selector '[] (Id NSUnitLength)
parsecsSelector = mkSelector "parsecs"

