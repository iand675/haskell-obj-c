{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNCylinder
--
-- SCNCylinder represents a cylinder with controllable height and radius.
--
-- Generated bindings for @SCNCylinder@.
module ObjC.SceneKit.SCNCylinder
  ( SCNCylinder
  , IsSCNCylinder(..)
  , cylinderWithRadius_height
  , radius
  , setRadius
  , height
  , setHeight
  , radialSegmentCount
  , setRadialSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , cylinderWithRadius_heightSelector
  , heightSegmentCountSelector
  , heightSelector
  , radialSegmentCountSelector
  , radiusSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
  , setRadialSegmentCountSelector
  , setRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cylinderWithRadius:height:
--
-- Creates and returns a cylinder with given radius and height.
--
-- @radius@ — The radius of the cylinder.
--
-- @height@ — The height of the cylinder.
--
-- ObjC selector: @+ cylinderWithRadius:height:@
cylinderWithRadius_height :: CDouble -> CDouble -> IO (Id SCNCylinder)
cylinderWithRadius_height radius height =
  do
    cls' <- getRequiredClass "SCNCylinder"
    sendClassMessage cls' cylinderWithRadius_heightSelector radius height

-- | radius
--
-- The radius of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- radius@
radius :: IsSCNCylinder scnCylinder => scnCylinder -> IO CDouble
radius scnCylinder =
  sendMessage scnCylinder radiusSelector

-- | radius
--
-- The radius of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNCylinder scnCylinder => scnCylinder -> CDouble -> IO ()
setRadius scnCylinder value =
  sendMessage scnCylinder setRadiusSelector value

-- | height
--
-- The height of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNCylinder scnCylinder => scnCylinder -> IO CDouble
height scnCylinder =
  sendMessage scnCylinder heightSelector

-- | height
--
-- The height of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCylinder scnCylinder => scnCylinder -> CDouble -> IO ()
setHeight scnCylinder value =
  sendMessage scnCylinder setHeightSelector value

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> IO CLong
radialSegmentCount scnCylinder =
  sendMessage scnCylinder radialSegmentCountSelector

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> CLong -> IO ()
setRadialSegmentCount scnCylinder value =
  sendMessage scnCylinder setRadialSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> IO CLong
heightSegmentCount scnCylinder =
  sendMessage scnCylinder heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> CLong -> IO ()
setHeightSegmentCount scnCylinder value =
  sendMessage scnCylinder setHeightSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cylinderWithRadius:height:@
cylinderWithRadius_heightSelector :: Selector '[CDouble, CDouble] (Id SCNCylinder)
cylinderWithRadius_heightSelector = mkSelector "cylinderWithRadius:height:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CDouble] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CDouble
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CDouble] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @radialSegmentCount@
radialSegmentCountSelector :: Selector '[] CLong
radialSegmentCountSelector = mkSelector "radialSegmentCount"

-- | @Selector@ for @setRadialSegmentCount:@
setRadialSegmentCountSelector :: Selector '[CLong] ()
setRadialSegmentCountSelector = mkSelector "setRadialSegmentCount:"

-- | @Selector@ for @heightSegmentCount@
heightSegmentCountSelector :: Selector '[] CLong
heightSegmentCountSelector = mkSelector "heightSegmentCount"

-- | @Selector@ for @setHeightSegmentCount:@
setHeightSegmentCountSelector :: Selector '[CLong] ()
setHeightSegmentCountSelector = mkSelector "setHeightSegmentCount:"

