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
  , radiusSelector
  , setRadiusSelector
  , heightSelector
  , setHeightSelector
  , radialSegmentCountSelector
  , setRadialSegmentCountSelector
  , heightSegmentCountSelector
  , setHeightSegmentCountSelector


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
    sendClassMsg cls' (mkSelector "cylinderWithRadius:height:") (retPtr retVoid) [argCDouble (fromIntegral radius), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- | radius
--
-- The radius of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- radius@
radius :: IsSCNCylinder scnCylinder => scnCylinder -> IO CDouble
radius scnCylinder  =
  sendMsg scnCylinder (mkSelector "radius") retCDouble []

-- | radius
--
-- The radius of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNCylinder scnCylinder => scnCylinder -> CDouble -> IO ()
setRadius scnCylinder  value =
  sendMsg scnCylinder (mkSelector "setRadius:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNCylinder scnCylinder => scnCylinder -> IO CDouble
height scnCylinder  =
  sendMsg scnCylinder (mkSelector "height") retCDouble []

-- | height
--
-- The height of the cylinder. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCylinder scnCylinder => scnCylinder -> CDouble -> IO ()
setHeight scnCylinder  value =
  sendMsg scnCylinder (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> IO CLong
radialSegmentCount scnCylinder  =
  sendMsg scnCylinder (mkSelector "radialSegmentCount") retCLong []

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> CLong -> IO ()
setRadialSegmentCount scnCylinder  value =
  sendMsg scnCylinder (mkSelector "setRadialSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> IO CLong
heightSegmentCount scnCylinder  =
  sendMsg scnCylinder (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCylinder scnCylinder => scnCylinder -> CLong -> IO ()
setHeightSegmentCount scnCylinder  value =
  sendMsg scnCylinder (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cylinderWithRadius:height:@
cylinderWithRadius_heightSelector :: Selector
cylinderWithRadius_heightSelector = mkSelector "cylinderWithRadius:height:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @radialSegmentCount@
radialSegmentCountSelector :: Selector
radialSegmentCountSelector = mkSelector "radialSegmentCount"

-- | @Selector@ for @setRadialSegmentCount:@
setRadialSegmentCountSelector :: Selector
setRadialSegmentCountSelector = mkSelector "setRadialSegmentCount:"

-- | @Selector@ for @heightSegmentCount@
heightSegmentCountSelector :: Selector
heightSegmentCountSelector = mkSelector "heightSegmentCount"

-- | @Selector@ for @setHeightSegmentCount:@
setHeightSegmentCountSelector :: Selector
setHeightSegmentCountSelector = mkSelector "setHeightSegmentCount:"

