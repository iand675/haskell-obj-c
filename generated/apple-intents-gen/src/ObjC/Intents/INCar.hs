{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCar@.
module ObjC.Intents.INCar
  ( INCar
  , IsINCar(..)
  , init_
  , initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectors
  , setMaximumPower_forChargingConnectorType
  , maximumPowerForChargingConnectorType
  , carIdentifier
  , displayName
  , year
  , make
  , model
  , color
  , headUnit
  , supportedChargingConnectors
  , carIdentifierSelector
  , colorSelector
  , displayNameSelector
  , headUnitSelector
  , initSelector
  , initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector
  , makeSelector
  , maximumPowerForChargingConnectorTypeSelector
  , modelSelector
  , setMaximumPower_forChargingConnectorTypeSelector
  , supportedChargingConnectorsSelector
  , yearSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCar inCar => inCar -> IO (Id INCar)
init_ inCar =
  sendOwnedMessage inCar initSelector

-- | @- initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:@
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectors :: (IsINCar inCar, IsNSString carIdentifier, IsNSString displayName, IsNSString year, IsNSString make, IsNSString model, IsINCarHeadUnit headUnit, IsNSArray supportedChargingConnectors) => inCar -> carIdentifier -> displayName -> year -> make -> model -> Ptr () -> headUnit -> supportedChargingConnectors -> IO (Id INCar)
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectors inCar carIdentifier displayName year make model color headUnit supportedChargingConnectors =
  sendOwnedMessage inCar initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector (toNSString carIdentifier) (toNSString displayName) (toNSString year) (toNSString make) (toNSString model) color (toINCarHeadUnit headUnit) (toNSArray supportedChargingConnectors)

-- | @- setMaximumPower:forChargingConnectorType:@
setMaximumPower_forChargingConnectorType :: (IsINCar inCar, IsNSMeasurement power, IsNSString chargingConnectorType) => inCar -> power -> chargingConnectorType -> IO ()
setMaximumPower_forChargingConnectorType inCar power chargingConnectorType =
  sendMessage inCar setMaximumPower_forChargingConnectorTypeSelector (toNSMeasurement power) (toNSString chargingConnectorType)

-- | @- maximumPowerForChargingConnectorType:@
maximumPowerForChargingConnectorType :: (IsINCar inCar, IsNSString chargingConnectorType) => inCar -> chargingConnectorType -> IO (Id NSMeasurement)
maximumPowerForChargingConnectorType inCar chargingConnectorType =
  sendMessage inCar maximumPowerForChargingConnectorTypeSelector (toNSString chargingConnectorType)

-- | @- carIdentifier@
carIdentifier :: IsINCar inCar => inCar -> IO (Id NSString)
carIdentifier inCar =
  sendMessage inCar carIdentifierSelector

-- | @- displayName@
displayName :: IsINCar inCar => inCar -> IO (Id NSString)
displayName inCar =
  sendMessage inCar displayNameSelector

-- | @- year@
year :: IsINCar inCar => inCar -> IO (Id NSString)
year inCar =
  sendMessage inCar yearSelector

-- | @- make@
make :: IsINCar inCar => inCar -> IO (Id NSString)
make inCar =
  sendMessage inCar makeSelector

-- | @- model@
model :: IsINCar inCar => inCar -> IO (Id NSString)
model inCar =
  sendMessage inCar modelSelector

-- | @- color@
color :: IsINCar inCar => inCar -> IO (Ptr ())
color inCar =
  sendMessage inCar colorSelector

-- | @- headUnit@
headUnit :: IsINCar inCar => inCar -> IO (Id INCarHeadUnit)
headUnit inCar =
  sendMessage inCar headUnitSelector

-- | @- supportedChargingConnectors@
supportedChargingConnectors :: IsINCar inCar => inCar -> IO (Id NSArray)
supportedChargingConnectors inCar =
  sendMessage inCar supportedChargingConnectorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCar)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:@
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Ptr (), Id INCarHeadUnit, Id NSArray] (Id INCar)
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector = mkSelector "initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:"

-- | @Selector@ for @setMaximumPower:forChargingConnectorType:@
setMaximumPower_forChargingConnectorTypeSelector :: Selector '[Id NSMeasurement, Id NSString] ()
setMaximumPower_forChargingConnectorTypeSelector = mkSelector "setMaximumPower:forChargingConnectorType:"

-- | @Selector@ for @maximumPowerForChargingConnectorType:@
maximumPowerForChargingConnectorTypeSelector :: Selector '[Id NSString] (Id NSMeasurement)
maximumPowerForChargingConnectorTypeSelector = mkSelector "maximumPowerForChargingConnectorType:"

-- | @Selector@ for @carIdentifier@
carIdentifierSelector :: Selector '[] (Id NSString)
carIdentifierSelector = mkSelector "carIdentifier"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @year@
yearSelector :: Selector '[] (Id NSString)
yearSelector = mkSelector "year"

-- | @Selector@ for @make@
makeSelector :: Selector '[] (Id NSString)
makeSelector = mkSelector "make"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Ptr ())
colorSelector = mkSelector "color"

-- | @Selector@ for @headUnit@
headUnitSelector :: Selector '[] (Id INCarHeadUnit)
headUnitSelector = mkSelector "headUnit"

-- | @Selector@ for @supportedChargingConnectors@
supportedChargingConnectorsSelector :: Selector '[] (Id NSArray)
supportedChargingConnectorsSelector = mkSelector "supportedChargingConnectors"

