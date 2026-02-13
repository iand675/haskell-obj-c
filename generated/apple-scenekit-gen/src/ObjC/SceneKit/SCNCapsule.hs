{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNCapsule
--
-- SCNCapsule represents a capsule with controllable height and cap radius.
--
-- Generated bindings for @SCNCapsule@.
module ObjC.SceneKit.SCNCapsule
  ( SCNCapsule
  , IsSCNCapsule(..)
  , capsuleWithCapRadius_height
  , capRadius
  , setCapRadius
  , height
  , setHeight
  , radialSegmentCount
  , setRadialSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , capSegmentCount
  , setCapSegmentCount
  , capRadiusSelector
  , capSegmentCountSelector
  , capsuleWithCapRadius_heightSelector
  , heightSegmentCountSelector
  , heightSelector
  , radialSegmentCountSelector
  , setCapRadiusSelector
  , setCapSegmentCountSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
  , setRadialSegmentCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | capsuleWithCapRadius:height:
--
-- Creates and returns a capsule with given radius and height.
--
-- @capRadius@ — The radius of the capsule.
--
-- @height@ — The height of the capsule.
--
-- ObjC selector: @+ capsuleWithCapRadius:height:@
capsuleWithCapRadius_height :: CDouble -> CDouble -> IO (Id SCNCapsule)
capsuleWithCapRadius_height capRadius height =
  do
    cls' <- getRequiredClass "SCNCapsule"
    sendClassMessage cls' capsuleWithCapRadius_heightSelector capRadius height

-- | capRadius
--
-- The cap radius of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- capRadius@
capRadius :: IsSCNCapsule scnCapsule => scnCapsule -> IO CDouble
capRadius scnCapsule =
  sendMessage scnCapsule capRadiusSelector

-- | capRadius
--
-- The cap radius of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setCapRadius:@
setCapRadius :: IsSCNCapsule scnCapsule => scnCapsule -> CDouble -> IO ()
setCapRadius scnCapsule value =
  sendMessage scnCapsule setCapRadiusSelector value

-- | height
--
-- The height of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 2.
--
-- ObjC selector: @- height@
height :: IsSCNCapsule scnCapsule => scnCapsule -> IO CDouble
height scnCapsule =
  sendMessage scnCapsule heightSelector

-- | height
--
-- The height of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 2.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCapsule scnCapsule => scnCapsule -> CDouble -> IO ()
setHeight scnCapsule value =
  sendMessage scnCapsule setHeightSelector value

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
radialSegmentCount scnCapsule =
  sendMessage scnCapsule radialSegmentCountSelector

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setRadialSegmentCount scnCapsule value =
  sendMessage scnCapsule setRadialSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
heightSegmentCount scnCapsule =
  sendMessage scnCapsule heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setHeightSegmentCount scnCapsule value =
  sendMessage scnCapsule setHeightSegmentCountSelector value

-- | capSegmentCount
--
-- The number of subdivisions in the cap. Animatable.
--
-- If the value is less than 2, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- capSegmentCount@
capSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
capSegmentCount scnCapsule =
  sendMessage scnCapsule capSegmentCountSelector

-- | capSegmentCount
--
-- The number of subdivisions in the cap. Animatable.
--
-- If the value is less than 2, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- setCapSegmentCount:@
setCapSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setCapSegmentCount scnCapsule value =
  sendMessage scnCapsule setCapSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capsuleWithCapRadius:height:@
capsuleWithCapRadius_heightSelector :: Selector '[CDouble, CDouble] (Id SCNCapsule)
capsuleWithCapRadius_heightSelector = mkSelector "capsuleWithCapRadius:height:"

-- | @Selector@ for @capRadius@
capRadiusSelector :: Selector '[] CDouble
capRadiusSelector = mkSelector "capRadius"

-- | @Selector@ for @setCapRadius:@
setCapRadiusSelector :: Selector '[CDouble] ()
setCapRadiusSelector = mkSelector "setCapRadius:"

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

-- | @Selector@ for @capSegmentCount@
capSegmentCountSelector :: Selector '[] CLong
capSegmentCountSelector = mkSelector "capSegmentCount"

-- | @Selector@ for @setCapSegmentCount:@
setCapSegmentCountSelector :: Selector '[CLong] ()
setCapSegmentCountSelector = mkSelector "setCapSegmentCount:"

