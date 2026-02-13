{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitArea@.
module ObjC.Foundation.NSUnitArea
  ( NSUnitArea
  , IsNSUnitArea(..)
  , squareMegameters
  , squareKilometers
  , squareMeters
  , squareCentimeters
  , squareMillimeters
  , squareMicrometers
  , squareNanometers
  , squareInches
  , squareFeet
  , squareYards
  , squareMiles
  , acres
  , ares
  , hectares
  , acresSelector
  , aresSelector
  , hectaresSelector
  , squareCentimetersSelector
  , squareFeetSelector
  , squareInchesSelector
  , squareKilometersSelector
  , squareMegametersSelector
  , squareMetersSelector
  , squareMicrometersSelector
  , squareMilesSelector
  , squareMillimetersSelector
  , squareNanometersSelector
  , squareYardsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ squareMegameters@
squareMegameters :: IO (Id NSUnitArea)
squareMegameters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareMegametersSelector

-- | @+ squareKilometers@
squareKilometers :: IO (Id NSUnitArea)
squareKilometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareKilometersSelector

-- | @+ squareMeters@
squareMeters :: IO (Id NSUnitArea)
squareMeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareMetersSelector

-- | @+ squareCentimeters@
squareCentimeters :: IO (Id NSUnitArea)
squareCentimeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareCentimetersSelector

-- | @+ squareMillimeters@
squareMillimeters :: IO (Id NSUnitArea)
squareMillimeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareMillimetersSelector

-- | @+ squareMicrometers@
squareMicrometers :: IO (Id NSUnitArea)
squareMicrometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareMicrometersSelector

-- | @+ squareNanometers@
squareNanometers :: IO (Id NSUnitArea)
squareNanometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareNanometersSelector

-- | @+ squareInches@
squareInches :: IO (Id NSUnitArea)
squareInches  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareInchesSelector

-- | @+ squareFeet@
squareFeet :: IO (Id NSUnitArea)
squareFeet  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareFeetSelector

-- | @+ squareYards@
squareYards :: IO (Id NSUnitArea)
squareYards  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareYardsSelector

-- | @+ squareMiles@
squareMiles :: IO (Id NSUnitArea)
squareMiles  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' squareMilesSelector

-- | @+ acres@
acres :: IO (Id NSUnitArea)
acres  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' acresSelector

-- | @+ ares@
ares :: IO (Id NSUnitArea)
ares  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' aresSelector

-- | @+ hectares@
hectares :: IO (Id NSUnitArea)
hectares  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMessage cls' hectaresSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @squareMegameters@
squareMegametersSelector :: Selector '[] (Id NSUnitArea)
squareMegametersSelector = mkSelector "squareMegameters"

-- | @Selector@ for @squareKilometers@
squareKilometersSelector :: Selector '[] (Id NSUnitArea)
squareKilometersSelector = mkSelector "squareKilometers"

-- | @Selector@ for @squareMeters@
squareMetersSelector :: Selector '[] (Id NSUnitArea)
squareMetersSelector = mkSelector "squareMeters"

-- | @Selector@ for @squareCentimeters@
squareCentimetersSelector :: Selector '[] (Id NSUnitArea)
squareCentimetersSelector = mkSelector "squareCentimeters"

-- | @Selector@ for @squareMillimeters@
squareMillimetersSelector :: Selector '[] (Id NSUnitArea)
squareMillimetersSelector = mkSelector "squareMillimeters"

-- | @Selector@ for @squareMicrometers@
squareMicrometersSelector :: Selector '[] (Id NSUnitArea)
squareMicrometersSelector = mkSelector "squareMicrometers"

-- | @Selector@ for @squareNanometers@
squareNanometersSelector :: Selector '[] (Id NSUnitArea)
squareNanometersSelector = mkSelector "squareNanometers"

-- | @Selector@ for @squareInches@
squareInchesSelector :: Selector '[] (Id NSUnitArea)
squareInchesSelector = mkSelector "squareInches"

-- | @Selector@ for @squareFeet@
squareFeetSelector :: Selector '[] (Id NSUnitArea)
squareFeetSelector = mkSelector "squareFeet"

-- | @Selector@ for @squareYards@
squareYardsSelector :: Selector '[] (Id NSUnitArea)
squareYardsSelector = mkSelector "squareYards"

-- | @Selector@ for @squareMiles@
squareMilesSelector :: Selector '[] (Id NSUnitArea)
squareMilesSelector = mkSelector "squareMiles"

-- | @Selector@ for @acres@
acresSelector :: Selector '[] (Id NSUnitArea)
acresSelector = mkSelector "acres"

-- | @Selector@ for @ares@
aresSelector :: Selector '[] (Id NSUnitArea)
aresSelector = mkSelector "ares"

-- | @Selector@ for @hectares@
hectaresSelector :: Selector '[] (Id NSUnitArea)
hectaresSelector = mkSelector "hectares"

