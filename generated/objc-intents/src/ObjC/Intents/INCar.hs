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
  , initSelector
  , initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector
  , setMaximumPower_forChargingConnectorTypeSelector
  , maximumPowerForChargingConnectorTypeSelector
  , carIdentifierSelector
  , displayNameSelector
  , yearSelector
  , makeSelector
  , modelSelector
  , colorSelector
  , headUnitSelector
  , supportedChargingConnectorsSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCar inCar => inCar -> IO (Id INCar)
init_ inCar  =
  sendMsg inCar (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:@
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectors :: (IsINCar inCar, IsNSString carIdentifier, IsNSString displayName, IsNSString year, IsNSString make, IsNSString model, IsINCarHeadUnit headUnit, IsNSArray supportedChargingConnectors) => inCar -> carIdentifier -> displayName -> year -> make -> model -> Ptr () -> headUnit -> supportedChargingConnectors -> IO (Id INCar)
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectors inCar  carIdentifier displayName year make model color headUnit supportedChargingConnectors =
withObjCPtr carIdentifier $ \raw_carIdentifier ->
  withObjCPtr displayName $ \raw_displayName ->
    withObjCPtr year $ \raw_year ->
      withObjCPtr make $ \raw_make ->
        withObjCPtr model $ \raw_model ->
          withObjCPtr headUnit $ \raw_headUnit ->
            withObjCPtr supportedChargingConnectors $ \raw_supportedChargingConnectors ->
                sendMsg inCar (mkSelector "initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:") (retPtr retVoid) [argPtr (castPtr raw_carIdentifier :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_year :: Ptr ()), argPtr (castPtr raw_make :: Ptr ()), argPtr (castPtr raw_model :: Ptr ()), argPtr color, argPtr (castPtr raw_headUnit :: Ptr ()), argPtr (castPtr raw_supportedChargingConnectors :: Ptr ())] >>= ownedObject . castPtr

-- | @- setMaximumPower:forChargingConnectorType:@
setMaximumPower_forChargingConnectorType :: (IsINCar inCar, IsNSMeasurement power, IsNSString chargingConnectorType) => inCar -> power -> chargingConnectorType -> IO ()
setMaximumPower_forChargingConnectorType inCar  power chargingConnectorType =
withObjCPtr power $ \raw_power ->
  withObjCPtr chargingConnectorType $ \raw_chargingConnectorType ->
      sendMsg inCar (mkSelector "setMaximumPower:forChargingConnectorType:") retVoid [argPtr (castPtr raw_power :: Ptr ()), argPtr (castPtr raw_chargingConnectorType :: Ptr ())]

-- | @- maximumPowerForChargingConnectorType:@
maximumPowerForChargingConnectorType :: (IsINCar inCar, IsNSString chargingConnectorType) => inCar -> chargingConnectorType -> IO (Id NSMeasurement)
maximumPowerForChargingConnectorType inCar  chargingConnectorType =
withObjCPtr chargingConnectorType $ \raw_chargingConnectorType ->
    sendMsg inCar (mkSelector "maximumPowerForChargingConnectorType:") (retPtr retVoid) [argPtr (castPtr raw_chargingConnectorType :: Ptr ())] >>= retainedObject . castPtr

-- | @- carIdentifier@
carIdentifier :: IsINCar inCar => inCar -> IO (Id NSString)
carIdentifier inCar  =
  sendMsg inCar (mkSelector "carIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsINCar inCar => inCar -> IO (Id NSString)
displayName inCar  =
  sendMsg inCar (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- year@
year :: IsINCar inCar => inCar -> IO (Id NSString)
year inCar  =
  sendMsg inCar (mkSelector "year") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- make@
make :: IsINCar inCar => inCar -> IO (Id NSString)
make inCar  =
  sendMsg inCar (mkSelector "make") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- model@
model :: IsINCar inCar => inCar -> IO (Id NSString)
model inCar  =
  sendMsg inCar (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- color@
color :: IsINCar inCar => inCar -> IO (Ptr ())
color inCar  =
  fmap castPtr $ sendMsg inCar (mkSelector "color") (retPtr retVoid) []

-- | @- headUnit@
headUnit :: IsINCar inCar => inCar -> IO (Id INCarHeadUnit)
headUnit inCar  =
  sendMsg inCar (mkSelector "headUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supportedChargingConnectors@
supportedChargingConnectors :: IsINCar inCar => inCar -> IO (Id NSArray)
supportedChargingConnectors inCar  =
  sendMsg inCar (mkSelector "supportedChargingConnectors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:@
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector :: Selector
initWithCarIdentifier_displayName_year_make_model_color_headUnit_supportedChargingConnectorsSelector = mkSelector "initWithCarIdentifier:displayName:year:make:model:color:headUnit:supportedChargingConnectors:"

-- | @Selector@ for @setMaximumPower:forChargingConnectorType:@
setMaximumPower_forChargingConnectorTypeSelector :: Selector
setMaximumPower_forChargingConnectorTypeSelector = mkSelector "setMaximumPower:forChargingConnectorType:"

-- | @Selector@ for @maximumPowerForChargingConnectorType:@
maximumPowerForChargingConnectorTypeSelector :: Selector
maximumPowerForChargingConnectorTypeSelector = mkSelector "maximumPowerForChargingConnectorType:"

-- | @Selector@ for @carIdentifier@
carIdentifierSelector :: Selector
carIdentifierSelector = mkSelector "carIdentifier"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @year@
yearSelector :: Selector
yearSelector = mkSelector "year"

-- | @Selector@ for @make@
makeSelector :: Selector
makeSelector = mkSelector "make"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @headUnit@
headUnitSelector :: Selector
headUnitSelector = mkSelector "headUnit"

-- | @Selector@ for @supportedChargingConnectors@
supportedChargingConnectorsSelector :: Selector
supportedChargingConnectorsSelector = mkSelector "supportedChargingConnectors"

