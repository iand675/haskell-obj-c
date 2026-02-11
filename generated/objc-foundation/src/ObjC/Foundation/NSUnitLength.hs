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
  , megametersSelector
  , kilometersSelector
  , hectometersSelector
  , decametersSelector
  , metersSelector
  , decimetersSelector
  , centimetersSelector
  , millimetersSelector
  , micrometersSelector
  , nanometersSelector
  , picometersSelector
  , inchesSelector
  , feetSelector
  , yardsSelector
  , milesSelector
  , scandinavianMilesSelector
  , lightyearsSelector
  , nauticalMilesSelector
  , fathomsSelector
  , furlongsSelector
  , astronomicalUnitsSelector
  , parsecsSelector


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

-- | @+ megameters@
megameters :: IO (Id NSUnitLength)
megameters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "megameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilometers@
kilometers :: IO (Id NSUnitLength)
kilometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "kilometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hectometers@
hectometers :: IO (Id NSUnitLength)
hectometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "hectometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decameters@
decameters :: IO (Id NSUnitLength)
decameters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "decameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ meters@
meters :: IO (Id NSUnitLength)
meters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "meters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decimeters@
decimeters :: IO (Id NSUnitLength)
decimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "decimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ centimeters@
centimeters :: IO (Id NSUnitLength)
centimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "centimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millimeters@
millimeters :: IO (Id NSUnitLength)
millimeters  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "millimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ micrometers@
micrometers :: IO (Id NSUnitLength)
micrometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "micrometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nanometers@
nanometers :: IO (Id NSUnitLength)
nanometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "nanometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ picometers@
picometers :: IO (Id NSUnitLength)
picometers  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "picometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ inches@
inches :: IO (Id NSUnitLength)
inches  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "inches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ feet@
feet :: IO (Id NSUnitLength)
feet  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "feet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yards@
yards :: IO (Id NSUnitLength)
yards  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "yards") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ miles@
miles :: IO (Id NSUnitLength)
miles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "miles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ scandinavianMiles@
scandinavianMiles :: IO (Id NSUnitLength)
scandinavianMiles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "scandinavianMiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lightyears@
lightyears :: IO (Id NSUnitLength)
lightyears  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "lightyears") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nauticalMiles@
nauticalMiles :: IO (Id NSUnitLength)
nauticalMiles  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "nauticalMiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fathoms@
fathoms :: IO (Id NSUnitLength)
fathoms  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "fathoms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ furlongs@
furlongs :: IO (Id NSUnitLength)
furlongs  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "furlongs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ astronomicalUnits@
astronomicalUnits :: IO (Id NSUnitLength)
astronomicalUnits  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "astronomicalUnits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ parsecs@
parsecs :: IO (Id NSUnitLength)
parsecs  =
  do
    cls' <- getRequiredClass "NSUnitLength"
    sendClassMsg cls' (mkSelector "parsecs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megameters@
megametersSelector :: Selector
megametersSelector = mkSelector "megameters"

-- | @Selector@ for @kilometers@
kilometersSelector :: Selector
kilometersSelector = mkSelector "kilometers"

-- | @Selector@ for @hectometers@
hectometersSelector :: Selector
hectometersSelector = mkSelector "hectometers"

-- | @Selector@ for @decameters@
decametersSelector :: Selector
decametersSelector = mkSelector "decameters"

-- | @Selector@ for @meters@
metersSelector :: Selector
metersSelector = mkSelector "meters"

-- | @Selector@ for @decimeters@
decimetersSelector :: Selector
decimetersSelector = mkSelector "decimeters"

-- | @Selector@ for @centimeters@
centimetersSelector :: Selector
centimetersSelector = mkSelector "centimeters"

-- | @Selector@ for @millimeters@
millimetersSelector :: Selector
millimetersSelector = mkSelector "millimeters"

-- | @Selector@ for @micrometers@
micrometersSelector :: Selector
micrometersSelector = mkSelector "micrometers"

-- | @Selector@ for @nanometers@
nanometersSelector :: Selector
nanometersSelector = mkSelector "nanometers"

-- | @Selector@ for @picometers@
picometersSelector :: Selector
picometersSelector = mkSelector "picometers"

-- | @Selector@ for @inches@
inchesSelector :: Selector
inchesSelector = mkSelector "inches"

-- | @Selector@ for @feet@
feetSelector :: Selector
feetSelector = mkSelector "feet"

-- | @Selector@ for @yards@
yardsSelector :: Selector
yardsSelector = mkSelector "yards"

-- | @Selector@ for @miles@
milesSelector :: Selector
milesSelector = mkSelector "miles"

-- | @Selector@ for @scandinavianMiles@
scandinavianMilesSelector :: Selector
scandinavianMilesSelector = mkSelector "scandinavianMiles"

-- | @Selector@ for @lightyears@
lightyearsSelector :: Selector
lightyearsSelector = mkSelector "lightyears"

-- | @Selector@ for @nauticalMiles@
nauticalMilesSelector :: Selector
nauticalMilesSelector = mkSelector "nauticalMiles"

-- | @Selector@ for @fathoms@
fathomsSelector :: Selector
fathomsSelector = mkSelector "fathoms"

-- | @Selector@ for @furlongs@
furlongsSelector :: Selector
furlongsSelector = mkSelector "furlongs"

-- | @Selector@ for @astronomicalUnits@
astronomicalUnitsSelector :: Selector
astronomicalUnitsSelector = mkSelector "astronomicalUnits"

-- | @Selector@ for @parsecs@
parsecsSelector :: Selector
parsecsSelector = mkSelector "parsecs"

