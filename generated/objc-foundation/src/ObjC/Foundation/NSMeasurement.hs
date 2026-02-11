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
  , initSelector
  , initWithDoubleValue_unitSelector
  , canBeConvertedToUnitSelector
  , measurementByConvertingToUnitSelector
  , measurementByAddingMeasurementSelector
  , measurementBySubtractingMeasurementSelector
  , unitSelector
  , doubleValueSelector


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

-- | @- init@
init_ :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO (Id NSMeasurement)
init_ nsMeasurement  =
  sendMsg nsMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDoubleValue:unit:@
initWithDoubleValue_unit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> CDouble -> unit -> IO (Id NSMeasurement)
initWithDoubleValue_unit nsMeasurement  doubleValue unit =
withObjCPtr unit $ \raw_unit ->
    sendMsg nsMeasurement (mkSelector "initWithDoubleValue:unit:") (retPtr retVoid) [argCDouble (fromIntegral doubleValue), argPtr (castPtr raw_unit :: Ptr ())] >>= ownedObject . castPtr

-- | @- canBeConvertedToUnit:@
canBeConvertedToUnit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> unit -> IO Bool
canBeConvertedToUnit nsMeasurement  unit =
withObjCPtr unit $ \raw_unit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMeasurement (mkSelector "canBeConvertedToUnit:") retCULong [argPtr (castPtr raw_unit :: Ptr ())]

-- | @- measurementByConvertingToUnit:@
measurementByConvertingToUnit :: (IsNSMeasurement nsMeasurement, IsNSUnit unit) => nsMeasurement -> unit -> IO (Id NSMeasurement)
measurementByConvertingToUnit nsMeasurement  unit =
withObjCPtr unit $ \raw_unit ->
    sendMsg nsMeasurement (mkSelector "measurementByConvertingToUnit:") (retPtr retVoid) [argPtr (castPtr raw_unit :: Ptr ())] >>= retainedObject . castPtr

-- | @- measurementByAddingMeasurement:@
measurementByAddingMeasurement :: (IsNSMeasurement nsMeasurement, IsNSMeasurement measurement) => nsMeasurement -> measurement -> IO (Id NSMeasurement)
measurementByAddingMeasurement nsMeasurement  measurement =
withObjCPtr measurement $ \raw_measurement ->
    sendMsg nsMeasurement (mkSelector "measurementByAddingMeasurement:") (retPtr retVoid) [argPtr (castPtr raw_measurement :: Ptr ())] >>= retainedObject . castPtr

-- | @- measurementBySubtractingMeasurement:@
measurementBySubtractingMeasurement :: (IsNSMeasurement nsMeasurement, IsNSMeasurement measurement) => nsMeasurement -> measurement -> IO (Id NSMeasurement)
measurementBySubtractingMeasurement nsMeasurement  measurement =
withObjCPtr measurement $ \raw_measurement ->
    sendMsg nsMeasurement (mkSelector "measurementBySubtractingMeasurement:") (retPtr retVoid) [argPtr (castPtr raw_measurement :: Ptr ())] >>= retainedObject . castPtr

-- | @- unit@
unit :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO (Id NSUnit)
unit nsMeasurement  =
  sendMsg nsMeasurement (mkSelector "unit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- doubleValue@
doubleValue :: IsNSMeasurement nsMeasurement => nsMeasurement -> IO CDouble
doubleValue nsMeasurement  =
  sendMsg nsMeasurement (mkSelector "doubleValue") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDoubleValue:unit:@
initWithDoubleValue_unitSelector :: Selector
initWithDoubleValue_unitSelector = mkSelector "initWithDoubleValue:unit:"

-- | @Selector@ for @canBeConvertedToUnit:@
canBeConvertedToUnitSelector :: Selector
canBeConvertedToUnitSelector = mkSelector "canBeConvertedToUnit:"

-- | @Selector@ for @measurementByConvertingToUnit:@
measurementByConvertingToUnitSelector :: Selector
measurementByConvertingToUnitSelector = mkSelector "measurementByConvertingToUnit:"

-- | @Selector@ for @measurementByAddingMeasurement:@
measurementByAddingMeasurementSelector :: Selector
measurementByAddingMeasurementSelector = mkSelector "measurementByAddingMeasurement:"

-- | @Selector@ for @measurementBySubtractingMeasurement:@
measurementBySubtractingMeasurementSelector :: Selector
measurementBySubtractingMeasurementSelector = mkSelector "measurementBySubtractingMeasurement:"

-- | @Selector@ for @unit@
unitSelector :: Selector
unitSelector = mkSelector "unit"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

