{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNCone
--
-- SCNCone represents a cone with controllable height, top radius and bottom radius.
--
-- Generated bindings for @SCNCone@.
module ObjC.SceneKit.SCNCone
  ( SCNCone
  , IsSCNCone(..)
  , coneWithTopRadius_bottomRadius_height
  , topRadius
  , setTopRadius
  , bottomRadius
  , setBottomRadius
  , height
  , setHeight
  , radialSegmentCount
  , setRadialSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , bottomRadiusSelector
  , coneWithTopRadius_bottomRadius_heightSelector
  , heightSegmentCountSelector
  , heightSelector
  , radialSegmentCountSelector
  , setBottomRadiusSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
  , setRadialSegmentCountSelector
  , setTopRadiusSelector
  , topRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | coneWithTopRadius:bottomRadius:height:
--
-- Creates and returns a cone with given top radius, bottom radius and height.
--
-- @topRadius@ — The radius at the top of the cone.
--
-- @bottomRadius@ — The radius at the bottom of the cone.
--
-- @height@ — The height of the cone.
--
-- ObjC selector: @+ coneWithTopRadius:bottomRadius:height:@
coneWithTopRadius_bottomRadius_height :: CDouble -> CDouble -> CDouble -> IO (Id SCNCone)
coneWithTopRadius_bottomRadius_height topRadius bottomRadius height =
  do
    cls' <- getRequiredClass "SCNCone"
    sendClassMessage cls' coneWithTopRadius_bottomRadius_heightSelector topRadius bottomRadius height

-- | topRadius
--
-- The radius at the top of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- topRadius@
topRadius :: IsSCNCone scnCone => scnCone -> IO CDouble
topRadius scnCone =
  sendMessage scnCone topRadiusSelector

-- | topRadius
--
-- The radius at the top of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setTopRadius:@
setTopRadius :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setTopRadius scnCone value =
  sendMessage scnCone setTopRadiusSelector value

-- | bottomRadius
--
-- The radius at the bottom of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- bottomRadius@
bottomRadius :: IsSCNCone scnCone => scnCone -> IO CDouble
bottomRadius scnCone =
  sendMessage scnCone bottomRadiusSelector

-- | bottomRadius
--
-- The radius at the bottom of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setBottomRadius:@
setBottomRadius :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setBottomRadius scnCone value =
  sendMessage scnCone setBottomRadiusSelector value

-- | height
--
-- The height of the cone. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNCone scnCone => scnCone -> IO CDouble
height scnCone =
  sendMessage scnCone heightSelector

-- | height
--
-- The height of the cone. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setHeight scnCone value =
  sendMessage scnCone setHeightSelector value

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCone scnCone => scnCone -> IO CLong
radialSegmentCount scnCone =
  sendMessage scnCone radialSegmentCountSelector

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCone scnCone => scnCone -> CLong -> IO ()
setRadialSegmentCount scnCone value =
  sendMessage scnCone setRadialSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCone scnCone => scnCone -> IO CLong
heightSegmentCount scnCone =
  sendMessage scnCone heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCone scnCone => scnCone -> CLong -> IO ()
setHeightSegmentCount scnCone value =
  sendMessage scnCone setHeightSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @coneWithTopRadius:bottomRadius:height:@
coneWithTopRadius_bottomRadius_heightSelector :: Selector '[CDouble, CDouble, CDouble] (Id SCNCone)
coneWithTopRadius_bottomRadius_heightSelector = mkSelector "coneWithTopRadius:bottomRadius:height:"

-- | @Selector@ for @topRadius@
topRadiusSelector :: Selector '[] CDouble
topRadiusSelector = mkSelector "topRadius"

-- | @Selector@ for @setTopRadius:@
setTopRadiusSelector :: Selector '[CDouble] ()
setTopRadiusSelector = mkSelector "setTopRadius:"

-- | @Selector@ for @bottomRadius@
bottomRadiusSelector :: Selector '[] CDouble
bottomRadiusSelector = mkSelector "bottomRadius"

-- | @Selector@ for @setBottomRadius:@
setBottomRadiusSelector :: Selector '[CDouble] ()
setBottomRadiusSelector = mkSelector "setBottomRadius:"

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

