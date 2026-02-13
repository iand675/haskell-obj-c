{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideVehicle@.
module ObjC.Intents.INRideVehicle
  ( INRideVehicle
  , IsINRideVehicle(..)
  , registrationPlate
  , setRegistrationPlate
  , manufacturer
  , setManufacturer
  , model
  , setModel
  , mapAnnotationImage
  , setMapAnnotationImage
  , manufacturerSelector
  , mapAnnotationImageSelector
  , modelSelector
  , registrationPlateSelector
  , setManufacturerSelector
  , setMapAnnotationImageSelector
  , setModelSelector
  , setRegistrationPlateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- registrationPlate@
registrationPlate :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
registrationPlate inRideVehicle =
  sendMessage inRideVehicle registrationPlateSelector

-- | @- setRegistrationPlate:@
setRegistrationPlate :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setRegistrationPlate inRideVehicle value =
  sendMessage inRideVehicle setRegistrationPlateSelector (toNSString value)

-- | @- manufacturer@
manufacturer :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
manufacturer inRideVehicle =
  sendMessage inRideVehicle manufacturerSelector

-- | @- setManufacturer:@
setManufacturer :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setManufacturer inRideVehicle value =
  sendMessage inRideVehicle setManufacturerSelector (toNSString value)

-- | @- model@
model :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
model inRideVehicle =
  sendMessage inRideVehicle modelSelector

-- | @- setModel:@
setModel :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setModel inRideVehicle value =
  sendMessage inRideVehicle setModelSelector (toNSString value)

-- | @- mapAnnotationImage@
mapAnnotationImage :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id INImage)
mapAnnotationImage inRideVehicle =
  sendMessage inRideVehicle mapAnnotationImageSelector

-- | @- setMapAnnotationImage:@
setMapAnnotationImage :: (IsINRideVehicle inRideVehicle, IsINImage value) => inRideVehicle -> value -> IO ()
setMapAnnotationImage inRideVehicle value =
  sendMessage inRideVehicle setMapAnnotationImageSelector (toINImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registrationPlate@
registrationPlateSelector :: Selector '[] (Id NSString)
registrationPlateSelector = mkSelector "registrationPlate"

-- | @Selector@ for @setRegistrationPlate:@
setRegistrationPlateSelector :: Selector '[Id NSString] ()
setRegistrationPlateSelector = mkSelector "setRegistrationPlate:"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector '[] (Id NSString)
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @setManufacturer:@
setManufacturerSelector :: Selector '[Id NSString] ()
setManufacturerSelector = mkSelector "setManufacturer:"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @setModel:@
setModelSelector :: Selector '[Id NSString] ()
setModelSelector = mkSelector "setModel:"

-- | @Selector@ for @mapAnnotationImage@
mapAnnotationImageSelector :: Selector '[] (Id INImage)
mapAnnotationImageSelector = mkSelector "mapAnnotationImage"

-- | @Selector@ for @setMapAnnotationImage:@
setMapAnnotationImageSelector :: Selector '[Id INImage] ()
setMapAnnotationImageSelector = mkSelector "setMapAnnotationImage:"

