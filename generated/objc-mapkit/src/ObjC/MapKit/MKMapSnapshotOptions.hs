{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapSnapshotOptions@.
module ObjC.MapKit.MKMapSnapshotOptions
  ( MKMapSnapshotOptions
  , IsMKMapSnapshotOptions(..)
  , preferredConfiguration
  , setPreferredConfiguration
  , camera
  , setCamera
  , mapType
  , setMapType
  , showsPointsOfInterest
  , setShowsPointsOfInterest
  , showsBuildings
  , setShowsBuildings
  , size
  , setSize
  , preferredConfigurationSelector
  , setPreferredConfigurationSelector
  , cameraSelector
  , setCameraSelector
  , mapTypeSelector
  , setMapTypeSelector
  , showsPointsOfInterestSelector
  , setShowsPointsOfInterestSelector
  , showsBuildingsSelector
  , setShowsBuildingsSelector
  , sizeSelector
  , setSizeSelector

  -- * Enum types
  , MKMapType(MKMapType)
  , pattern MKMapTypeStandard
  , pattern MKMapTypeSatellite
  , pattern MKMapTypeHybrid
  , pattern MKMapTypeSatelliteFlyover
  , pattern MKMapTypeHybridFlyover
  , pattern MKMapTypeMutedStandard

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- preferredConfiguration@
preferredConfiguration :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id MKMapConfiguration)
preferredConfiguration mkMapSnapshotOptions  =
  sendMsg mkMapSnapshotOptions (mkSelector "preferredConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredConfiguration:@
setPreferredConfiguration :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsMKMapConfiguration value) => mkMapSnapshotOptions -> value -> IO ()
setPreferredConfiguration mkMapSnapshotOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMapSnapshotOptions (mkSelector "setPreferredConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- camera@
camera :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id MKMapCamera)
camera mkMapSnapshotOptions  =
  sendMsg mkMapSnapshotOptions (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCamera:@
setCamera :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsMKMapCamera value) => mkMapSnapshotOptions -> value -> IO ()
setCamera mkMapSnapshotOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkMapSnapshotOptions (mkSelector "setCamera:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mapType@
mapType :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO MKMapType
mapType mkMapSnapshotOptions  =
  fmap (coerce :: CULong -> MKMapType) $ sendMsg mkMapSnapshotOptions (mkSelector "mapType") retCULong []

-- | @- setMapType:@
setMapType :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> MKMapType -> IO ()
setMapType mkMapSnapshotOptions  value =
  sendMsg mkMapSnapshotOptions (mkSelector "setMapType:") retVoid [argCULong (coerce value)]

-- | @- showsPointsOfInterest@
showsPointsOfInterest :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO Bool
showsPointsOfInterest mkMapSnapshotOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapSnapshotOptions (mkSelector "showsPointsOfInterest") retCULong []

-- | @- setShowsPointsOfInterest:@
setShowsPointsOfInterest :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> Bool -> IO ()
setShowsPointsOfInterest mkMapSnapshotOptions  value =
  sendMsg mkMapSnapshotOptions (mkSelector "setShowsPointsOfInterest:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsBuildings@
showsBuildings :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO Bool
showsBuildings mkMapSnapshotOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapSnapshotOptions (mkSelector "showsBuildings") retCULong []

-- | @- setShowsBuildings:@
setShowsBuildings :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> Bool -> IO ()
setShowsBuildings mkMapSnapshotOptions  value =
  sendMsg mkMapSnapshotOptions (mkSelector "setShowsBuildings:") retVoid [argCULong (if value then 1 else 0)]

-- | @- size@
size :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO NSSize
size mkMapSnapshotOptions  =
  sendMsgStret mkMapSnapshotOptions (mkSelector "size") retNSSize []

-- | @- setSize:@
setSize :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> NSSize -> IO ()
setSize mkMapSnapshotOptions  value =
  sendMsg mkMapSnapshotOptions (mkSelector "setSize:") retVoid [argNSSize value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredConfiguration@
preferredConfigurationSelector :: Selector
preferredConfigurationSelector = mkSelector "preferredConfiguration"

-- | @Selector@ for @setPreferredConfiguration:@
setPreferredConfigurationSelector :: Selector
setPreferredConfigurationSelector = mkSelector "setPreferredConfiguration:"

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @setMapType:@
setMapTypeSelector :: Selector
setMapTypeSelector = mkSelector "setMapType:"

-- | @Selector@ for @showsPointsOfInterest@
showsPointsOfInterestSelector :: Selector
showsPointsOfInterestSelector = mkSelector "showsPointsOfInterest"

-- | @Selector@ for @setShowsPointsOfInterest:@
setShowsPointsOfInterestSelector :: Selector
setShowsPointsOfInterestSelector = mkSelector "setShowsPointsOfInterest:"

-- | @Selector@ for @showsBuildings@
showsBuildingsSelector :: Selector
showsBuildingsSelector = mkSelector "showsBuildings"

-- | @Selector@ for @setShowsBuildings:@
setShowsBuildingsSelector :: Selector
setShowsBuildingsSelector = mkSelector "setShowsBuildings:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

