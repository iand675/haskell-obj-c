{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , showsPointsOfInterest
  , setShowsPointsOfInterest
  , showsBuildings
  , setShowsBuildings
  , size
  , setSize
  , appearance
  , setAppearance
  , appearanceSelector
  , cameraSelector
  , mapTypeSelector
  , pointOfInterestFilterSelector
  , preferredConfigurationSelector
  , setAppearanceSelector
  , setCameraSelector
  , setMapTypeSelector
  , setPointOfInterestFilterSelector
  , setPreferredConfigurationSelector
  , setShowsBuildingsSelector
  , setShowsPointsOfInterestSelector
  , setSizeSelector
  , showsBuildingsSelector
  , showsPointsOfInterestSelector
  , sizeSelector

  -- * Enum types
  , MKMapType(MKMapType)
  , pattern MKMapTypeStandard
  , pattern MKMapTypeSatellite
  , pattern MKMapTypeHybrid
  , pattern MKMapTypeSatelliteFlyover
  , pattern MKMapTypeHybridFlyover
  , pattern MKMapTypeMutedStandard

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- preferredConfiguration@
preferredConfiguration :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id MKMapConfiguration)
preferredConfiguration mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions preferredConfigurationSelector

-- | @- setPreferredConfiguration:@
setPreferredConfiguration :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsMKMapConfiguration value) => mkMapSnapshotOptions -> value -> IO ()
setPreferredConfiguration mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setPreferredConfigurationSelector (toMKMapConfiguration value)

-- | @- camera@
camera :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id MKMapCamera)
camera mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions cameraSelector

-- | @- setCamera:@
setCamera :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsMKMapCamera value) => mkMapSnapshotOptions -> value -> IO ()
setCamera mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setCameraSelector (toMKMapCamera value)

-- | @- mapType@
mapType :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO MKMapType
mapType mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions mapTypeSelector

-- | @- setMapType:@
setMapType :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> MKMapType -> IO ()
setMapType mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setMapTypeSelector value

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions pointOfInterestFilterSelector

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsMKPointOfInterestFilter value) => mkMapSnapshotOptions -> value -> IO ()
setPointOfInterestFilter mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setPointOfInterestFilterSelector (toMKPointOfInterestFilter value)

-- | @- showsPointsOfInterest@
showsPointsOfInterest :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO Bool
showsPointsOfInterest mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions showsPointsOfInterestSelector

-- | @- setShowsPointsOfInterest:@
setShowsPointsOfInterest :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> Bool -> IO ()
setShowsPointsOfInterest mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setShowsPointsOfInterestSelector value

-- | @- showsBuildings@
showsBuildings :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO Bool
showsBuildings mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions showsBuildingsSelector

-- | @- setShowsBuildings:@
setShowsBuildings :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> Bool -> IO ()
setShowsBuildings mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setShowsBuildingsSelector value

-- | @- size@
size :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO NSSize
size mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions sizeSelector

-- | @- setSize:@
setSize :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> NSSize -> IO ()
setSize mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setSizeSelector value

-- | @- appearance@
appearance :: IsMKMapSnapshotOptions mkMapSnapshotOptions => mkMapSnapshotOptions -> IO (Id NSAppearance)
appearance mkMapSnapshotOptions =
  sendMessage mkMapSnapshotOptions appearanceSelector

-- | @- setAppearance:@
setAppearance :: (IsMKMapSnapshotOptions mkMapSnapshotOptions, IsNSAppearance value) => mkMapSnapshotOptions -> value -> IO ()
setAppearance mkMapSnapshotOptions value =
  sendMessage mkMapSnapshotOptions setAppearanceSelector (toNSAppearance value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredConfiguration@
preferredConfigurationSelector :: Selector '[] (Id MKMapConfiguration)
preferredConfigurationSelector = mkSelector "preferredConfiguration"

-- | @Selector@ for @setPreferredConfiguration:@
setPreferredConfigurationSelector :: Selector '[Id MKMapConfiguration] ()
setPreferredConfigurationSelector = mkSelector "setPreferredConfiguration:"

-- | @Selector@ for @camera@
cameraSelector :: Selector '[] (Id MKMapCamera)
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector '[Id MKMapCamera] ()
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector '[] MKMapType
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @setMapType:@
setMapTypeSelector :: Selector '[MKMapType] ()
setMapTypeSelector = mkSelector "setMapType:"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector '[] (Id MKPointOfInterestFilter)
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector '[Id MKPointOfInterestFilter] ()
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

-- | @Selector@ for @showsPointsOfInterest@
showsPointsOfInterestSelector :: Selector '[] Bool
showsPointsOfInterestSelector = mkSelector "showsPointsOfInterest"

-- | @Selector@ for @setShowsPointsOfInterest:@
setShowsPointsOfInterestSelector :: Selector '[Bool] ()
setShowsPointsOfInterestSelector = mkSelector "setShowsPointsOfInterest:"

-- | @Selector@ for @showsBuildings@
showsBuildingsSelector :: Selector '[] Bool
showsBuildingsSelector = mkSelector "showsBuildings"

-- | @Selector@ for @setShowsBuildings:@
setShowsBuildingsSelector :: Selector '[Bool] ()
setShowsBuildingsSelector = mkSelector "setShowsBuildings:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[NSSize] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @appearance@
appearanceSelector :: Selector '[] (Id NSAppearance)
appearanceSelector = mkSelector "appearance"

-- | @Selector@ for @setAppearance:@
setAppearanceSelector :: Selector '[Id NSAppearance] ()
setAppearanceSelector = mkSelector "setAppearance:"

