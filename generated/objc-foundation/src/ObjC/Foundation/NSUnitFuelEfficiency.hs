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
  , milesPerImperialGallonSelector
  , milesPerGallonSelector


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

-- | @+ litersPer100Kilometers@
litersPer100Kilometers :: IO (Id NSUnitFuelEfficiency)
litersPer100Kilometers  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMsg cls' (mkSelector "litersPer100Kilometers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milesPerImperialGallon@
milesPerImperialGallon :: IO (Id NSUnitFuelEfficiency)
milesPerImperialGallon  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMsg cls' (mkSelector "milesPerImperialGallon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milesPerGallon@
milesPerGallon :: IO (Id NSUnitFuelEfficiency)
milesPerGallon  =
  do
    cls' <- getRequiredClass "NSUnitFuelEfficiency"
    sendClassMsg cls' (mkSelector "milesPerGallon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @litersPer100Kilometers@
litersPer100KilometersSelector :: Selector
litersPer100KilometersSelector = mkSelector "litersPer100Kilometers"

-- | @Selector@ for @milesPerImperialGallon@
milesPerImperialGallonSelector :: Selector
milesPerImperialGallonSelector = mkSelector "milesPerImperialGallon"

-- | @Selector@ for @milesPerGallon@
milesPerGallonSelector :: Selector
milesPerGallonSelector = mkSelector "milesPerGallon"

