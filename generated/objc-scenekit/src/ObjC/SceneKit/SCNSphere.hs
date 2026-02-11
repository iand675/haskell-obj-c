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
  , sphereWithRadiusSelector
  , radiusSelector
  , setRadiusSelector
  , geodesicSelector
  , setGeodesicSelector
  , segmentCountSelector
  , setSegmentCountSelector


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
    sendClassMsg cls' (mkSelector "sphereWithRadius:") (retPtr retVoid) [argCDouble (fromIntegral radius)] >>= retainedObject . castPtr

-- | radius
--
-- The sphere radius. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- radius@
radius :: IsSCNSphere scnSphere => scnSphere -> IO CDouble
radius scnSphere  =
  sendMsg scnSphere (mkSelector "radius") retCDouble []

-- | radius
--
-- The sphere radius. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNSphere scnSphere => scnSphere -> CDouble -> IO ()
setRadius scnSphere  value =
  sendMsg scnSphere (mkSelector "setRadius:") retVoid [argCDouble (fromIntegral value)]

-- | geodesic
--
-- Indicate if the geometry is a geosphere.
--
-- The default value is NO.
--
-- ObjC selector: @- geodesic@
geodesic :: IsSCNSphere scnSphere => scnSphere -> IO Bool
geodesic scnSphere  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnSphere (mkSelector "geodesic") retCULong []

-- | geodesic
--
-- Indicate if the geometry is a geosphere.
--
-- The default value is NO.
--
-- ObjC selector: @- setGeodesic:@
setGeodesic :: IsSCNSphere scnSphere => scnSphere -> Bool -> IO ()
setGeodesic scnSphere  value =
  sendMsg scnSphere (mkSelector "setGeodesic:") retVoid [argCULong (if value then 1 else 0)]

-- | segmentCount
--
-- The number of segments along both spherical coordinates. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsSCNSphere scnSphere => scnSphere -> IO CLong
segmentCount scnSphere  =
  sendMsg scnSphere (mkSelector "segmentCount") retCLong []

-- | segmentCount
--
-- The number of segments along both spherical coordinates. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setSegmentCount:@
setSegmentCount :: IsSCNSphere scnSphere => scnSphere -> CLong -> IO ()
setSegmentCount scnSphere  value =
  sendMsg scnSphere (mkSelector "setSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sphereWithRadius:@
sphereWithRadiusSelector :: Selector
sphereWithRadiusSelector = mkSelector "sphereWithRadius:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @geodesic@
geodesicSelector :: Selector
geodesicSelector = mkSelector "geodesic"

-- | @Selector@ for @setGeodesic:@
setGeodesicSelector :: Selector
setGeodesicSelector = mkSelector "setGeodesic:"

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector
segmentCountSelector = mkSelector "segmentCount"

-- | @Selector@ for @setSegmentCount:@
setSegmentCountSelector :: Selector
setSegmentCountSelector = mkSelector "setSegmentCount:"

