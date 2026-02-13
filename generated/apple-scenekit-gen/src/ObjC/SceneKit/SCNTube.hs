{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNTube
--
-- SCNTube represents a tube with controllable height, inner radius and outer radius.
--
-- Generated bindings for @SCNTube@.
module ObjC.SceneKit.SCNTube
  ( SCNTube
  , IsSCNTube(..)
  , tubeWithInnerRadius_outerRadius_height
  , innerRadius
  , setInnerRadius
  , outerRadius
  , setOuterRadius
  , height
  , setHeight
  , radialSegmentCount
  , setRadialSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , heightSegmentCountSelector
  , heightSelector
  , innerRadiusSelector
  , outerRadiusSelector
  , radialSegmentCountSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
  , setInnerRadiusSelector
  , setOuterRadiusSelector
  , setRadialSegmentCountSelector
  , tubeWithInnerRadius_outerRadius_heightSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | tubeWithInnerRadius:outerRadius:height:
--
-- Creates and returns a tube with given inner radius, outer radius and height.
--
-- @innerRadius@ — The inner radius of the tube.
--
-- @outerRadius@ — The outer radius of the tube.
--
-- @height@ — The height of the tube.
--
-- ObjC selector: @+ tubeWithInnerRadius:outerRadius:height:@
tubeWithInnerRadius_outerRadius_height :: CDouble -> CDouble -> CDouble -> IO (Id SCNTube)
tubeWithInnerRadius_outerRadius_height innerRadius outerRadius height =
  do
    cls' <- getRequiredClass "SCNTube"
    sendClassMessage cls' tubeWithInnerRadius_outerRadius_heightSelector innerRadius outerRadius height

-- | innerRadius
--
-- The inner radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is greater than or equal to the outer radius, then the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- innerRadius@
innerRadius :: IsSCNTube scnTube => scnTube -> IO CDouble
innerRadius scnTube =
  sendMessage scnTube innerRadiusSelector

-- | innerRadius
--
-- The inner radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is greater than or equal to the outer radius, then the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- setInnerRadius:@
setInnerRadius :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setInnerRadius scnTube value =
  sendMessage scnTube setInnerRadiusSelector value

-- | outerRadius
--
-- The outer radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is less than or equal to the inner radius, then the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- outerRadius@
outerRadius :: IsSCNTube scnTube => scnTube -> IO CDouble
outerRadius scnTube =
  sendMessage scnTube outerRadiusSelector

-- | outerRadius
--
-- The outer radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is less than or equal to the inner radius, then the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setOuterRadius:@
setOuterRadius :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setOuterRadius scnTube value =
  sendMessage scnTube setOuterRadiusSelector value

-- | height
--
-- The height of the tube. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNTube scnTube => scnTube -> IO CDouble
height scnTube =
  sendMessage scnTube heightSelector

-- | height
--
-- The height of the tube. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setHeight scnTube value =
  sendMessage scnTube setHeightSelector value

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNTube scnTube => scnTube -> IO CLong
radialSegmentCount scnTube =
  sendMessage scnTube radialSegmentCountSelector

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNTube scnTube => scnTube -> CLong -> IO ()
setRadialSegmentCount scnTube value =
  sendMessage scnTube setRadialSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNTube scnTube => scnTube -> IO CLong
heightSegmentCount scnTube =
  sendMessage scnTube heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNTube scnTube => scnTube -> CLong -> IO ()
setHeightSegmentCount scnTube value =
  sendMessage scnTube setHeightSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tubeWithInnerRadius:outerRadius:height:@
tubeWithInnerRadius_outerRadius_heightSelector :: Selector '[CDouble, CDouble, CDouble] (Id SCNTube)
tubeWithInnerRadius_outerRadius_heightSelector = mkSelector "tubeWithInnerRadius:outerRadius:height:"

-- | @Selector@ for @innerRadius@
innerRadiusSelector :: Selector '[] CDouble
innerRadiusSelector = mkSelector "innerRadius"

-- | @Selector@ for @setInnerRadius:@
setInnerRadiusSelector :: Selector '[CDouble] ()
setInnerRadiusSelector = mkSelector "setInnerRadius:"

-- | @Selector@ for @outerRadius@
outerRadiusSelector :: Selector '[] CDouble
outerRadiusSelector = mkSelector "outerRadius"

-- | @Selector@ for @setOuterRadius:@
setOuterRadiusSelector :: Selector '[CDouble] ()
setOuterRadiusSelector = mkSelector "setOuterRadius:"

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

