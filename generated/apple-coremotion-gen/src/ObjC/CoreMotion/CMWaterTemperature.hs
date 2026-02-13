{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMWaterTemperature@.
module ObjC.CoreMotion.CMWaterTemperature
  ( CMWaterTemperature
  , IsCMWaterTemperature(..)
  , date
  , temperature
  , temperatureUncertainty
  , dateSelector
  , temperatureSelector
  , temperatureUncertaintySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSDate)
date cmWaterTemperature =
  sendMessage cmWaterTemperature dateSelector

-- | @- temperature@
temperature :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSMeasurement)
temperature cmWaterTemperature =
  sendMessage cmWaterTemperature temperatureSelector

-- | @- temperatureUncertainty@
temperatureUncertainty :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSMeasurement)
temperatureUncertainty cmWaterTemperature =
  sendMessage cmWaterTemperature temperatureUncertaintySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector '[] (Id NSMeasurement)
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @temperatureUncertainty@
temperatureUncertaintySelector :: Selector '[] (Id NSMeasurement)
temperatureUncertaintySelector = mkSelector "temperatureUncertainty"

