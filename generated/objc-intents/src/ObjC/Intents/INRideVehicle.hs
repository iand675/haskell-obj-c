{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideVehicle@.
module ObjC.Intents.INRideVehicle
  ( INRideVehicle
  , IsINRideVehicle(..)
  , location
  , setLocation
  , registrationPlate
  , setRegistrationPlate
  , manufacturer
  , setManufacturer
  , model
  , setModel
  , mapAnnotationImage
  , setMapAnnotationImage
  , locationSelector
  , setLocationSelector
  , registrationPlateSelector
  , setRegistrationPlateSelector
  , manufacturerSelector
  , setManufacturerSelector
  , modelSelector
  , setModelSelector
  , mapAnnotationImageSelector
  , setMapAnnotationImageSelector


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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- location@
location :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id CLLocation)
location inRideVehicle  =
  sendMsg inRideVehicle (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsINRideVehicle inRideVehicle, IsCLLocation value) => inRideVehicle -> value -> IO ()
setLocation inRideVehicle  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideVehicle (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- registrationPlate@
registrationPlate :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
registrationPlate inRideVehicle  =
  sendMsg inRideVehicle (mkSelector "registrationPlate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRegistrationPlate:@
setRegistrationPlate :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setRegistrationPlate inRideVehicle  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideVehicle (mkSelector "setRegistrationPlate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- manufacturer@
manufacturer :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
manufacturer inRideVehicle  =
  sendMsg inRideVehicle (mkSelector "manufacturer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManufacturer:@
setManufacturer :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setManufacturer inRideVehicle  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideVehicle (mkSelector "setManufacturer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- model@
model :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id NSString)
model inRideVehicle  =
  sendMsg inRideVehicle (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModel:@
setModel :: (IsINRideVehicle inRideVehicle, IsNSString value) => inRideVehicle -> value -> IO ()
setModel inRideVehicle  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideVehicle (mkSelector "setModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mapAnnotationImage@
mapAnnotationImage :: IsINRideVehicle inRideVehicle => inRideVehicle -> IO (Id INImage)
mapAnnotationImage inRideVehicle  =
  sendMsg inRideVehicle (mkSelector "mapAnnotationImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapAnnotationImage:@
setMapAnnotationImage :: (IsINRideVehicle inRideVehicle, IsINImage value) => inRideVehicle -> value -> IO ()
setMapAnnotationImage inRideVehicle  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRideVehicle (mkSelector "setMapAnnotationImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @registrationPlate@
registrationPlateSelector :: Selector
registrationPlateSelector = mkSelector "registrationPlate"

-- | @Selector@ for @setRegistrationPlate:@
setRegistrationPlateSelector :: Selector
setRegistrationPlateSelector = mkSelector "setRegistrationPlate:"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @setManufacturer:@
setManufacturerSelector :: Selector
setManufacturerSelector = mkSelector "setManufacturer:"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @setModel:@
setModelSelector :: Selector
setModelSelector = mkSelector "setModel:"

-- | @Selector@ for @mapAnnotationImage@
mapAnnotationImageSelector :: Selector
mapAnnotationImageSelector = mkSelector "mapAnnotationImage"

-- | @Selector@ for @setMapAnnotationImage:@
setMapAnnotationImageSelector :: Selector
setMapAnnotationImageSelector = mkSelector "setMapAnnotationImage:"

