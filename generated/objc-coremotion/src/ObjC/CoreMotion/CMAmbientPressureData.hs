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

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Discussion:    The pressure as measured by the pressure sensor.    Pressure is in kPa (kilopascals).
--
-- ObjC selector: @- pressure@
pressure :: IsCMAmbientPressureData cmAmbientPressureData => cmAmbientPressureData -> IO (Id NSMeasurement)
pressure cmAmbientPressureData  =
  sendMsg cmAmbientPressureData (mkSelector "pressure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Discussion:    The temperature as measured by the pressure sensor.    Temperature is in C (degrees centrigrade).
--
-- ObjC selector: @- temperature@
temperature :: IsCMAmbientPressureData cmAmbientPressureData => cmAmbientPressureData -> IO (Id NSMeasurement)
temperature cmAmbientPressureData  =
  sendMsg cmAmbientPressureData (mkSelector "temperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector
temperatureSelector = mkSelector "temperature"

