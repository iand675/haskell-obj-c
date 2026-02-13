{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitFuelEfficiency@.
module ObjC.Foundation.NSUnitFuelEfficiency
  ( NSUnitFuelEfficiency
  , IsNSUnitFuelEfficiency(..)
  , litersPer100Kilometers
  , milesPerImperialGallon
  , milesPerGallon
  , litersPer100KilometersSelector
  , milesPerGallonSelector
  , milesPerImperialGallonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ litersPer100Kilometers@
litersPer100Kilometers :: IO (Id NSUnitFuelEfficiency)
litersPer100Kilometers  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMessage cls' litersPer100KilometersSelector

-- | @+ milesPerImperialGallon@
milesPerImperialGallon :: IO (Id NSUnitFuelEfficiency)
milesPerImperialGallon  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMessage cls' milesPerImperialGallonSelector

-- | @+ milesPerGallon@
milesPerGallon :: IO (Id NSUnitFuelEfficiency)
milesPerGallon  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMessage cls' milesPerGallonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @litersPer100Kilometers@
litersPer100KilometersSelector :: Selector '[] (Id NSUnitFuelEfficiency)
litersPer100KilometersSelector = mkSelector "litersPer100Kilometers"

-- | @Selector@ for @milesPerImperialGallon@
milesPerImperialGallonSelector :: Selector '[] (Id NSUnitFuelEfficiency)
milesPerImperialGallonSelector = mkSelector "milesPerImperialGallon"

-- | @Selector@ for @milesPerGallon@
milesPerGallonSelector :: Selector '[] (Id NSUnitFuelEfficiency)
milesPerGallonSelector = mkSelector "milesPerGallon"

