{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMeasurement@.
module ObjC.Foundation.NSMeasurement
  ( NSMeasurement
  , IsNSMeasurement(..)
  , init_
  , initWithDoubleValue_unit
  , canBeConvertedToUnit
  , measurementByConvertingToUnit
  , measurementByAddingMeasurement
  , measurementBySubtractingMeasurement
  , unit
  , doubleValue
  , canBeConvertedToUnitSelector
  , doubleValueSelector
  , initSelector
  , initWithDoubleValue_unitSelector
  , measurementByAddingMeasurementSelector
  , measurementByConvertingToUnitSelector
  , measurementBySubtractingMeasurementSelector
  , unitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO (Id NSMeasurement)
init_ nsMeasurement =
  sendOwnedMessage nsMeasurement initSelector

-- | @- initWithDoubleValue:unit:@
initWithDoubleValue_unit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> CDouble -> unit -> IO (Id NSMeasurement)
initWithDoubleValue_unit nsMeasurement doubleValue unit =
  sendOwnedMessage nsMeasurement initWithDoubleValue_unitSelector doubleValue (toNSUnit unit)

-- | @- canBeConvertedToUnit:@
canBeConvertedToUnit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> unit -> IO Bool
canBeConvertedToUnit nsMeasurement unit =
  sendMessage nsMeasurement canBeConvertedToUnitSelector (toNSUnit unit)

-- | @- measurementByConvertingToUnit:@
measurementByConvertingToUnit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> unit -> IO (Id NSMeasurement)
measurementByConvertingToUnit nsMeasurement unit =
  sendMessage nsMeasurement measurementByConvertingToUnitSelector (toNSUnit unit)

-- | @- measurementByAddingMeasurement:@
measurementByAddingMeasurement :: (IsNSMeasurement nsMeasurement, IsNSMeasurement measurement) => nsMeasurement -> measurement -> IO (Id NSMeasurement)
measurementByAddingMeasurement nsMeasurement measurement =
  sendMessage nsMeasurement measurementByAddingMeasurementSelector (toNSMeasurement measurement)

-- | @- measurementBySubtractingMeasurement:@
measurementBySubtractingMeasurement :: (IsNSMeasurement nsMeasurement, IsNSMeasurement measurement) => nsMeasurement -> measurement -> IO (Id NSMeasurement)
measurementBySubtractingMeasurement nsMeasurement measurement =
  sendMessage nsMeasurement measurementBySubtractingMeasurementSelector (toNSMeasurement measurement)

-- | @- unit@
unit :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO (Id NSUnit)
unit nsMeasurement =
  sendMessage nsMeasurement unitSelector

-- | @- doubleValue@
doubleValue :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO CDouble
doubleValue nsMeasurement =
  sendMessage nsMeasurement doubleValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDoubleValue:unit:@
initWithDoubleValue_unitSelector :: Selector '[CDouble, Id NSUnit] (Id NSMeasurement)
initWithDoubleValue_unitSelector = mkSelector "initWithDoubleValue:unit:"

-- | @Selector@ for @canBeConvertedToUnit:@
canBeConvertedToUnitSelector :: Selector '[Id NSUnit] Bool
canBeConvertedToUnitSelector = mkSelector "canBeConvertedToUnit:"

-- | @Selector@ for @measurementByConvertingToUnit:@
measurementByConvertingToUnitSelector :: Selector '[Id NSUnit] (Id NSMeasurement)
measurementByConvertingToUnitSelector = mkSelector "measurementByConvertingToUnit:"

-- | @Selector@ for @measurementByAddingMeasurement:@
measurementByAddingMeasurementSelector :: Selector '[Id NSMeasurement] (Id NSMeasurement)
measurementByAddingMeasurementSelector = mkSelector "measurementByAddingMeasurement:"

-- | @Selector@ for @measurementBySubtractingMeasurement:@
measurementBySubtractingMeasurementSelector :: Selector '[Id NSMeasurement] (Id NSMeasurement)
measurementBySubtractingMeasurementSelector = mkSelector "measurementBySubtractingMeasurement:"

-- | @Selector@ for @unit@
unitSelector :: Selector '[] (Id NSUnit)
unitSelector = mkSelector "unit"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

