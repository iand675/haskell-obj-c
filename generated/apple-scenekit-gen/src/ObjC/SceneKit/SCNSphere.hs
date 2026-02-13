{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNSphere
--
-- SCNSphere represents a sphere with controllable radius
--
-- Generated bindings for @SCNSphere@.
module ObjC.SceneKit.SCNSphere
  ( SCNSphere
  , IsSCNSphere(..)
  , sphereWithRadius
  , radius
  , setRadius
  , geodesic
  , setGeodesic
  , segmentCount
  , setSegmentCount
  , geodesicSelector
  , radiusSelector
  , segmentCountSelector
  , setGeodesicSelector
  , setRadiusSelector
  , setSegmentCountSelector
  , sphereWithRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sphereWithRadius:
--
-- Creates and returns a sphere with given radius.
--
-- @radius@ — The radius of the sphere.
--
-- ObjC selector: @+ sphereWithRadius:@
sphereWithRadius :: CDouble -> IO (Id SCNSphere)
sphereWithRadius radius =
  do
    cls' <- getRequiredClass "SCNSphere"
    sendClassMessage cls' sphereWithRadiusSelector radius

-- | radius
--
-- The sphere radius. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- radius@
radius :: IsSCNSphere scnSphere => scnSphere -> IO CDouble
radius scnSphere =
  sendMessage scnSphere radiusSelector

-- | radius
--
-- The sphere radius. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNSphere scnSphere => scnSphere -> CDouble -> IO ()
setRadius scnSphere value =
  sendMessage scnSphere setRadiusSelector value

-- | geodesic
--
-- Indicate if the geometry is a geosphere.
--
-- The default value is NO.
--
-- ObjC selector: @- geodesic@
geodesic :: IsSCNSphere scnSphere => scnSphere -> IO Bool
geodesic scnSphere =
  sendMessage scnSphere geodesicSelector

-- | geodesic
--
-- Indicate if the geometry is a geosphere.
--
-- The default value is NO.
--
-- ObjC selector: @- setGeodesic:@
setGeodesic :: IsSCNSphere scnSphere => scnSphere -> Bool -> IO ()
setGeodesic scnSphere value =
  sendMessage scnSphere setGeodesicSelector value

-- | segmentCount
--
-- The number of segments along both spherical coordinates. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsSCNSphere scnSphere => scnSphere -> IO CLong
segmentCount scnSphere =
  sendMessage scnSphere segmentCountSelector

-- | segmentCount
--
-- The number of segments along both spherical coordinates. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsSCNSphere scnSphere => scnSphere -> CLong -> IO ()
setSegmentCount scnSphere value =
  sendMessage scnSphere setSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sphereWithRadius:@
sphereWithRadiusSelector :: Selector '[CDouble] (Id SCNSphere)
sphereWithRadiusSelector = mkSelector "sphereWithRadius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CDouble] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @geodesic@
geodesicSelector :: Selector '[] Bool
geodesicSelector = mkSelector "geodesic"

-- | @Selector@ for @setGeodesic:@
setGeodesicSelector :: Selector '[Bool] ()
setGeodesicSelector = mkSelector "setGeodesic:"

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector '[] CLong
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector '[CLong] ()
setSegmentCountSelector = mkSelector "setSegmentCount:"

