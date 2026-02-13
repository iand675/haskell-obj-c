{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAmbientPressureData@.
module ObjC.CoreMotion.CMAmbientPressureData
  ( CMAmbientPressureData
  , IsCMAmbientPressureData(..)
  , pressure
  , temperature
  , pressureSelector
  , temperatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Discussion:    The pressure as measured by the pressure sensor.    Pressure is in kPa (kilopascals).
--
-- ObjC selector: @- pressure@
pressure :: IsCMAmbientPressureData cmAmbientPressureData => cmAmbientPressureData -> IO (Id NSMeasurement)
pressure cmAmbientPressureData =
  sendMessage cmAmbientPressureData pressureSelector

-- | Discussion:    The temperature as measured by the pressure sensor.    Temperature is in C (degrees centrigrade).
--
-- ObjC selector: @- temperature@
temperature :: IsCMAmbientPressureData cmAmbientPressureData => cmAmbientPressureData -> IO (Id NSMeasurement)
temperature cmAmbientPressureData =
  sendMessage cmAmbientPressureData temperatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] (Id NSMeasurement)
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector '[] (Id NSMeasurement)
temperatureSelector = mkSelector "temperature"

