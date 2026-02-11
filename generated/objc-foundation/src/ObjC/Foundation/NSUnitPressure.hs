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
  , newtonsPerMetersSquaredSelector
  , gigapascalsSelector
  , megapascalsSelector
  , kilopascalsSelector
  , hectopascalsSelector
  , inchesOfMercurySelector
  , barsSelector
  , millibarsSelector
  , millimetersOfMercurySelector
  , poundsForcePerSquareInchSelector


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

-- | @+ newtonsPerMetersSquared@
newtonsPerMetersSquared :: IO (Id NSUnitPressure)
newtonsPerMetersSquared  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "newtonsPerMetersSquared") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ gigapascals@
gigapascals :: IO (Id NSUnitPressure)
gigapascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "gigapascals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megapascals@
megapascals :: IO (Id NSUnitPressure)
megapascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "megapascals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilopascals@
kilopascals :: IO (Id NSUnitPressure)
kilopascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "kilopascals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hectopascals@
hectopascals :: IO (Id NSUnitPressure)
hectopascals  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "hectopascals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ inchesOfMercury@
inchesOfMercury :: IO (Id NSUnitPressure)
inchesOfMercury  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "inchesOfMercury") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ bars@
bars :: IO (Id NSUnitPressure)
bars  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "bars") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millibars@
millibars :: IO (Id NSUnitPressure)
millibars  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "millibars") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millimetersOfMercury@
millimetersOfMercury :: IO (Id NSUnitPressure)
millimetersOfMercury  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "millimetersOfMercury") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ poundsForcePerSquareInch@
poundsForcePerSquareInch :: IO (Id NSUnitPressure)
poundsForcePerSquareInch  =
  do
    cls' <- getRequiredClass "NSUnitPressure"
    sendClassMsg cls' (mkSelector "poundsForcePerSquareInch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newtonsPerMetersSquared@
newtonsPerMetersSquaredSelector :: Selector
newtonsPerMetersSquaredSelector = mkSelector "newtonsPerMetersSquared"

-- | @Selector@ for @gigapascals@
gigapascalsSelector :: Selector
gigapascalsSelector = mkSelector "gigapascals"

-- | @Selector@ for @megapascals@
megapascalsSelector :: Selector
megapascalsSelector = mkSelector "megapascals"

-- | @Selector@ for @kilopascals@
kilopascalsSelector :: Selector
kilopascalsSelector = mkSelector "kilopascals"

-- | @Selector@ for @hectopascals@
hectopascalsSelector :: Selector
hectopascalsSelector = mkSelector "hectopascals"

-- | @Selector@ for @inchesOfMercury@
inchesOfMercurySelector :: Selector
inchesOfMercurySelector = mkSelector "inchesOfMercury"

-- | @Selector@ for @bars@
barsSelector :: Selector
barsSelector = mkSelector "bars"

-- | @Selector@ for @millibars@
millibarsSelector :: Selector
millibarsSelector = mkSelector "millibars"

-- | @Selector@ for @millimetersOfMercury@
millimetersOfMercurySelector :: Selector
millimetersOfMercurySelector = mkSelector "millimetersOfMercury"

-- | @Selector@ for @poundsForcePerSquareInch@
poundsForcePerSquareInchSelector :: Selector
poundsForcePerSquareInchSelector = mkSelector "poundsForcePerSquareInch"

