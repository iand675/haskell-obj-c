{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitPressure@.
module ObjC.Foundation.NSUnitPressure
  ( NSUnitPressure
  , IsNSUnitPressure(..)
  , newtonsPerMetersSquared
  , gigapascals
  , megapascals
  , kilopascals
  , hectopascals
  , inchesOfMercury
  , bars
  , millibars
  , millimetersOfMercury
  , poundsForcePerSquareInch
  , barsSelector
  , gigapascalsSelector
  , hectopascalsSelector
  , inchesOfMercurySelector
  , kilopascalsSelector
  , megapascalsSelector
  , millibarsSelector
  , millimetersOfMercurySelector
  , newtonsPerMetersSquaredSelector
  , poundsForcePerSquareInchSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ newtonsPerMetersSquared@
newtonsPerMetersSquared :: IO (Id NSUnitPressure)
newtonsPerMetersSquared  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendOwnedClassMessage cls' newtonsPerMetersSquaredSelector

-- | @+ gigapascals@
gigapascals :: IO (Id NSUnitPressure)
gigapascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' gigapascalsSelector

-- | @+ megapascals@
megapascals :: IO (Id NSUnitPressure)
megapascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' megapascalsSelector

-- | @+ kilopascals@
kilopascals :: IO (Id NSUnitPressure)
kilopascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' kilopascalsSelector

-- | @+ hectopascals@
hectopascals :: IO (Id NSUnitPressure)
hectopascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' hectopascalsSelector

-- | @+ inchesOfMercury@
inchesOfMercury :: IO (Id NSUnitPressure)
inchesOfMercury  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' inchesOfMercurySelector

-- | @+ bars@
bars :: IO (Id NSUnitPressure)
bars  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' barsSelector

-- | @+ millibars@
millibars :: IO (Id NSUnitPressure)
millibars  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' millibarsSelector

-- | @+ millimetersOfMercury@
millimetersOfMercury :: IO (Id NSUnitPressure)
millimetersOfMercury  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' millimetersOfMercurySelector

-- | @+ poundsForcePerSquareInch@
poundsForcePerSquareInch :: IO (Id NSUnitPressure)
poundsForcePerSquareInch  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMessage cls' poundsForcePerSquareInchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newtonsPerMetersSquared@
newtonsPerMetersSquaredSelector :: Selector '[] (Id NSUnitPressure)
newtonsPerMetersSquaredSelector = mkSelector "newtonsPerMetersSquared"

-- | @Selector@ for @gigapascals@
gigapascalsSelector :: Selector '[] (Id NSUnitPressure)
gigapascalsSelector = mkSelector "gigapascals"

-- | @Selector@ for @megapascals@
megapascalsSelector :: Selector '[] (Id NSUnitPressure)
megapascalsSelector = mkSelector "megapascals"

-- | @Selector@ for @kilopascals@
kilopascalsSelector :: Selector '[] (Id NSUnitPressure)
kilopascalsSelector = mkSelector "kilopascals"

-- | @Selector@ for @hectopascals@
hectopascalsSelector :: Selector '[] (Id NSUnitPressure)
hectopascalsSelector = mkSelector "hectopascals"

-- | @Selector@ for @inchesOfMercury@
inchesOfMercurySelector :: Selector '[] (Id NSUnitPressure)
inchesOfMercurySelector = mkSelector "inchesOfMercury"

-- | @Selector@ for @bars@
barsSelector :: Selector '[] (Id NSUnitPressure)
barsSelector = mkSelector "bars"

-- | @Selector@ for @millibars@
millibarsSelector :: Selector '[] (Id NSUnitPressure)
millibarsSelector = mkSelector "millibars"

-- | @Selector@ for @millimetersOfMercury@
millimetersOfMercurySelector :: Selector '[] (Id NSUnitPressure)
millimetersOfMercurySelector = mkSelector "millimetersOfMercury"

-- | @Selector@ for @poundsForcePerSquareInch@
poundsForcePerSquareInchSelector :: Selector '[] (Id NSUnitPressure)
poundsForcePerSquareInchSelector = mkSelector "poundsForcePerSquareInch"

