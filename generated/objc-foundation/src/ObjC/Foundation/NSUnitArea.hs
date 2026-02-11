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
  , squareMegametersSelector
  , squareKilometersSelector
  , squareMetersSelector
  , squareCentimetersSelector
  , squareMillimetersSelector
  , squareMicrometersSelector
  , squareNanometersSelector
  , squareInchesSelector
  , squareFeetSelector
  , squareYardsSelector
  , squareMilesSelector
  , acresSelector
  , aresSelector
  , hectaresSelector


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

-- | @+ squareMegameters@
squareMegameters :: IO (Id NSUnitArea)
squareMegameters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareMegameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareKilometers@
squareKilometers :: IO (Id NSUnitArea)
squareKilometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareKilometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareMeters@
squareMeters :: IO (Id NSUnitArea)
squareMeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareMeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareCentimeters@
squareCentimeters :: IO (Id NSUnitArea)
squareCentimeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareCentimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareMillimeters@
squareMillimeters :: IO (Id NSUnitArea)
squareMillimeters  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareMillimeters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareMicrometers@
squareMicrometers :: IO (Id NSUnitArea)
squareMicrometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareMicrometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareNanometers@
squareNanometers :: IO (Id NSUnitArea)
squareNanometers  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareNanometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareInches@
squareInches :: IO (Id NSUnitArea)
squareInches  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareInches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareFeet@
squareFeet :: IO (Id NSUnitArea)
squareFeet  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareFeet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareYards@
squareYards :: IO (Id NSUnitArea)
squareYards  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareYards") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ squareMiles@
squareMiles :: IO (Id NSUnitArea)
squareMiles  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "squareMiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ acres@
acres :: IO (Id NSUnitArea)
acres  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "acres") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ares@
ares :: IO (Id NSUnitArea)
ares  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "ares") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hectares@
hectares :: IO (Id NSUnitArea)
hectares  =
  do
    cls' <- getRequiredClass "NSUnitArea"
    sendClassMsg cls' (mkSelector "hectares") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @squareMegameters@
squareMegametersSelector :: Selector
squareMegametersSelector = mkSelector "squareMegameters"

-- | @Selector@ for @squareKilometers@
squareKilometersSelector :: Selector
squareKilometersSelector = mkSelector "squareKilometers"

-- | @Selector@ for @squareMeters@
squareMetersSelector :: Selector
squareMetersSelector = mkSelector "squareMeters"

-- | @Selector@ for @squareCentimeters@
squareCentimetersSelector :: Selector
squareCentimetersSelector = mkSelector "squareCentimeters"

-- | @Selector@ for @squareMillimeters@
squareMillimetersSelector :: Selector
squareMillimetersSelector = mkSelector "squareMillimeters"

-- | @Selector@ for @squareMicrometers@
squareMicrometersSelector :: Selector
squareMicrometersSelector = mkSelector "squareMicrometers"

-- | @Selector@ for @squareNanometers@
squareNanometersSelector :: Selector
squareNanometersSelector = mkSelector "squareNanometers"

-- | @Selector@ for @squareInches@
squareInchesSelector :: Selector
squareInchesSelector = mkSelector "squareInches"

-- | @Selector@ for @squareFeet@
squareFeetSelector :: Selector
squareFeetSelector = mkSelector "squareFeet"

-- | @Selector@ for @squareYards@
squareYardsSelector :: Selector
squareYardsSelector = mkSelector "squareYards"

-- | @Selector@ for @squareMiles@
squareMilesSelector :: Selector
squareMilesSelector = mkSelector "squareMiles"

-- | @Selector@ for @acres@
acresSelector :: Selector
acresSelector = mkSelector "acres"

-- | @Selector@ for @ares@
aresSelector :: Selector
aresSelector = mkSelector "ares"

-- | @Selector@ for @hectares@
hectaresSelector :: Selector
hectaresSelector = mkSelector "hectares"

