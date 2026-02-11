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
  , capsuleWithCapRadius_heightSelector
  , capRadiusSelector
  , setCapRadiusSelector
  , heightSelector
  , setHeightSelector
  , radialSegmentCountSelector
  , setRadialSegmentCountSelector
  , heightSegmentCountSelector
  , setHeightSegmentCountSelector
  , capSegmentCountSelector
  , setCapSegmentCountSelector


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
    sendClassMsg cls' (mkSelector "capsuleWithCapRadius:height:") (retPtr retVoid) [argCDouble (fromIntegral capRadius), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- | capRadius
--
-- The cap radius of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- capRadius@
capRadius :: IsSCNCapsule scnCapsule => scnCapsule -> IO CDouble
capRadius scnCapsule  =
  sendMsg scnCapsule (mkSelector "capRadius") retCDouble []

-- | capRadius
--
-- The cap radius of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setCapRadius:@
setCapRadius :: IsSCNCapsule scnCapsule => scnCapsule -> CDouble -> IO ()
setCapRadius scnCapsule  value =
  sendMsg scnCapsule (mkSelector "setCapRadius:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 2.
--
-- ObjC selector: @- height@
height :: IsSCNCapsule scnCapsule => scnCapsule -> IO CDouble
height scnCapsule  =
  sendMsg scnCapsule (mkSelector "height") retCDouble []

-- | height
--
-- The height of the capsule. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 2.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNCapsule scnCapsule => scnCapsule -> CDouble -> IO ()
setHeight scnCapsule  value =
  sendMsg scnCapsule (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- radialSegmentCount@
radialSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
radialSegmentCount scnCapsule  =
  sendMsg scnCapsule (mkSelector "radialSegmentCount") retCLong []

-- | radialSegmentCount
--
-- The number of subdivisions along the radial coordinate. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRadialSegmentCount:@
setRadialSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setRadialSegmentCount scnCapsule  value =
  sendMsg scnCapsule (mkSelector "setRadialSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
heightSegmentCount scnCapsule  =
  sendMsg scnCapsule (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setHeightSegmentCount scnCapsule  value =
  sendMsg scnCapsule (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | capSegmentCount
--
-- The number of subdivisions in the cap. Animatable.
--
-- If the value is less than 2, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- capSegmentCount@
capSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> IO CLong
capSegmentCount scnCapsule  =
  sendMsg scnCapsule (mkSelector "capSegmentCount") retCLong []

-- | capSegmentCount
--
-- The number of subdivisions in the cap. Animatable.
--
-- If the value is less than 2, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- setCapSegmentCount:@
setCapSegmentCount :: IsSCNCapsule scnCapsule => scnCapsule -> CLong -> IO ()
setCapSegmentCount scnCapsule  value =
  sendMsg scnCapsule (mkSelector "setCapSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capsuleWithCapRadius:height:@
capsuleWithCapRadius_heightSelector :: Selector
capsuleWithCapRadius_heightSelector = mkSelector "capsuleWithCapRadius:height:"

-- | @Selector@ for @capRadius@
capRadiusSelector :: Selector
capRadiusSelector = mkSelector "capRadius"

-- | @Selector@ for @setCapRadius:@
setCapRadiusSelector :: Selector
setCapRadiusSelector = mkSelector "setCapRadius:"

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

-- | @Selector@ for @capSegmentCount@
capSegmentCountSelector :: Selector
capSegmentCountSelector = mkSelector "capSegmentCount"

-- | @Selector@ for @setCapSegmentCount:@
setCapSegmentCountSelector :: Selector
setCapSegmentCountSelector = mkSelector "setCapSegmentCount:"

