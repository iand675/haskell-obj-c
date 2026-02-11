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

-- | @- date@
date :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSDate)
date cmWaterTemperature  =
  sendMsg cmWaterTemperature (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temperature@
temperature :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSMeasurement)
temperature cmWaterTemperature  =
  sendMsg cmWaterTemperature (mkSelector "temperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- temperatureUncertainty@
temperatureUncertainty :: IsCMWaterTemperature cmWaterTemperature => cmWaterTemperature -> IO (Id NSMeasurement)
temperatureUncertainty cmWaterTemperature  =
  sendMsg cmWaterTemperature (mkSelector "temperatureUncertainty") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @temperatureUncertainty@
temperatureUncertaintySelector :: Selector
temperatureUncertaintySelector = mkSelector "temperatureUncertainty"

