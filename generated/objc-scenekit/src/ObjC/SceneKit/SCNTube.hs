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
  , tubeWithInnerRadius_outerRadius_heightSelector
  , innerRadiusSelector
  , setInnerRadiusSelector
  , outerRadiusSelector
  , setOuterRadiusSelector
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
    sendClassMsg cls' (mkSelector "tubeWithInnerRadius:outerRadius:height:") (retPtr retVoid) [argCDouble (fromIntegral innerRadius), argCDouble (fromIntegral outerRadius), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- | innerRadius
--
-- The inner radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is greater than or equal to the outer radius, then the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- innerRadius@
innerRadius :: IsSCNTube scnTube => scnTube -> IO CDouble
innerRadius scnTube  =
  sendMsg scnTube (mkSelector "innerRadius") retCDouble []

-- | innerRadius
--
-- The inner radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is greater than or equal to the outer radius, then the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- setInnerRadius:@
setInnerRadius :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setInnerRadius scnTube  value =
  sendMsg scnTube (mkSelector "setInnerRadius:") retVoid [argCDouble (fromIntegral value)]

-- | outerRadius
--
-- The outer radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is less than or equal to the inner radius, then the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- outerRadius@
outerRadius :: IsSCNTube scnTube => scnTube -> IO CDouble
outerRadius scnTube  =
  sendMsg scnTube (mkSelector "outerRadius") retCDouble []

-- | outerRadius
--
-- The outer radius of the tube. Animatable.
--
-- If the value is less than or equal to 0, or if it is less than or equal to the inner radius, then the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setOuterRadius:@
setOuterRadius :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setOuterRadius scnTube  value =
  sendMsg scnTube (mkSelector "setOuterRadius:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the tube. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNTube scnTube => scnTube -> IO CDouble
height scnTube  =
  sendMsg scnTube (mkSelector "height") retCDouble []

-- | height
--
-- The height of the tube. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNTube scnTube => scnTube -> CDouble -> IO ()
setHeight scnTube  value =
  sendMsg scnTube (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNTube scnTube => scnTube -> IO CLong
radialSegmentCount scnTube  =
  sendMsg scnTube (mkSelector "radialSegmentCount") retCLong []

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNTube scnTube => scnTube -> CLong -> IO ()
setRadialSegmentCount scnTube  value =
  sendMsg scnTube (mkSelector "setRadialSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNTube scnTube => scnTube -> IO CLong
heightSegmentCount scnTube  =
  sendMsg scnTube (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNTube scnTube => scnTube -> CLong -> IO ()
setHeightSegmentCount scnTube  value =
  sendMsg scnTube (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tubeWithInnerRadius:outerRadius:height:@
tubeWithInnerRadius_outerRadius_heightSelector :: Selector
tubeWithInnerRadius_outerRadius_heightSelector = mkSelector "tubeWithInnerRadius:outerRadius:height:"

-- | @Selector@ for @innerRadius@
innerRadiusSelector :: Selector
innerRadiusSelector = mkSelector "innerRadius"

-- | @Selector@ for @setInnerRadius:@
setInnerRadiusSelector :: Selector
setInnerRadiusSelector = mkSelector "setInnerRadius:"

-- | @Selector@ for @outerRadius@
outerRadiusSelector :: Selector
outerRadiusSelector = mkSelector "outerRadius"

-- | @Selector@ for @setOuterRadius:@
setOuterRadiusSelector :: Selector
setOuterRadiusSelector = mkSelector "setOuterRadius:"

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

