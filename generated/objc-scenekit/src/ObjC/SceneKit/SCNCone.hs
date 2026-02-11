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
  , coneWithTopRadius_bottomRadius_heightSelector
  , topRadiusSelector
  , setTopRadiusSelector
  , bottomRadiusSelector
  , setBottomRadiusSelector
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
    sendClassMsg cls' (mkSelector "coneWithTopRadius:bottomRadius:height:") (retPtr retVoid) [argCDouble (fromIntegral topRadius), argCDouble (fromIntegral bottomRadius), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- | topRadius
--
-- The radius at the top of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- topRadius@
topRadius :: IsSCNCone scnCone => scnCone -> IO CDouble
topRadius scnCone  =
  sendMsg scnCone (mkSelector "topRadius") retCDouble []

-- | topRadius
--
-- The radius at the top of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setTopRadius:@
setTopRadius :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setTopRadius scnCone  value =
  sendMsg scnCone (mkSelector "setTopRadius:") retVoid [argCDouble (fromIntegral value)]

-- | bottomRadius
--
-- The radius at the bottom of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- bottomRadius@
bottomRadius :: IsSCNCone scnCone => scnCone -> IO CDouble
bottomRadius scnCone  =
  sendMsg scnCone (mkSelector "bottomRadius") retCDouble []

-- | bottomRadius
--
-- The radius at the bottom of the cone. Animatable.
--
-- If the value is less than 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setBottomRadius:@
setBottomRadius :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setBottomRadius scnCone  value =
  sendMsg scnCone (mkSelector "setBottomRadius:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the cone. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNCone scnCone => scnCone -> IO CDouble
height scnCone  =
  sendMsg scnCone (mkSelector "height") retCDouble []

-- | height
--
-- The height of the cone. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCone scnCone => scnCone -> CDouble -> IO ()
setHeight scnCone  value =
  sendMsg scnCone (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCone scnCone => scnCone -> IO CLong
radialSegmentCount scnCone  =
  sendMsg scnCone (mkSelector "radialSegmentCount") retCLong []

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCone scnCone => scnCone -> CLong -> IO ()
setRadialSegmentCount scnCone  value =
  sendMsg scnCone (mkSelector "setRadialSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCone scnCone => scnCone -> IO CLong
heightSegmentCount scnCone  =
  sendMsg scnCone (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCone scnCone => scnCone -> CLong -> IO ()
setHeightSegmentCount scnCone  value =
  sendMsg scnCone (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @coneWithTopRadius:bottomRadius:height:@
coneWithTopRadius_bottomRadius_heightSelector :: Selector
coneWithTopRadius_bottomRadius_heightSelector = mkSelector "coneWithTopRadius:bottomRadius:height:"

-- | @Selector@ for @topRadius@
topRadiusSelector :: Selector
topRadiusSelector = mkSelector "topRadius"

-- | @Selector@ for @setTopRadius:@
setTopRadiusSelector :: Selector
setTopRadiusSelector = mkSelector "setTopRadius:"

-- | @Selector@ for @bottomRadius@
bottomRadiusSelector :: Selector
bottomRadiusSelector = mkSelector "bottomRadius"

-- | @Selector@ for @setBottomRadius:@
setBottomRadiusSelector :: Selector
setBottomRadiusSelector = mkSelector "setBottomRadius:"

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

