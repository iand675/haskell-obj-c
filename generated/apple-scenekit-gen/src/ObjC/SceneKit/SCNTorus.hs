{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNTorus
--
-- SCNTorus represents a torus with controllable ring radius and pipe radius.
--
-- Generated bindings for @SCNTorus@.
module ObjC.SceneKit.SCNTorus
  ( SCNTorus
  , IsSCNTorus(..)
  , torusWithRingRadius_pipeRadius
  , ringRadius
  , setRingRadius
  , pipeRadius
  , setPipeRadius
  , ringSegmentCount
  , setRingSegmentCount
  , pipeSegmentCount
  , setPipeSegmentCount
  , pipeRadiusSelector
  , pipeSegmentCountSelector
  , ringRadiusSelector
  , ringSegmentCountSelector
  , setPipeRadiusSelector
  , setPipeSegmentCountSelector
  , setRingRadiusSelector
  , setRingSegmentCountSelector
  , torusWithRingRadius_pipeRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | torusWithRingRadius:pipeRadius:
--
-- Creates and returns a torus with given ring radius and pipe radius.
--
-- @ringRadius@ — The radius of the ring.
--
-- @pipeRadius@ — The radius of the pipe.
--
-- ObjC selector: @+ torusWithRingRadius:pipeRadius:@
torusWithRingRadius_pipeRadius :: CDouble -> CDouble -> IO (Id SCNTorus)
torusWithRingRadius_pipeRadius ringRadius pipeRadius =
  do
    cls' <- getRequiredClass "SCNTorus"
    sendClassMessage cls' torusWithRingRadius_pipeRadiusSelector ringRadius pipeRadius

-- | ringRadius
--
-- The radius of the torus ring. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- ringRadius@
ringRadius :: IsSCNTorus scnTorus => scnTorus -> IO CDouble
ringRadius scnTorus =
  sendMessage scnTorus ringRadiusSelector

-- | ringRadius
--
-- The radius of the torus ring. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRingRadius:@
setRingRadius :: IsSCNTorus scnTorus => scnTorus -> CDouble -> IO ()
setRingRadius scnTorus value =
  sendMessage scnTorus setRingRadiusSelector value

-- | pipeRadius
--
-- The radius of the torus pipe. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- pipeRadius@
pipeRadius :: IsSCNTorus scnTorus => scnTorus -> IO CDouble
pipeRadius scnTorus =
  sendMessage scnTorus pipeRadiusSelector

-- | pipeRadius
--
-- The radius of the torus pipe. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- setPipeRadius:@
setPipeRadius :: IsSCNTorus scnTorus => scnTorus -> CDouble -> IO ()
setPipeRadius scnTorus value =
  sendMessage scnTorus setPipeRadiusSelector value

-- | ringSegmentCount
--
-- The number of subdivisions of the ring. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- ringSegmentCount@
ringSegmentCount :: IsSCNTorus scnTorus => scnTorus -> IO CLong
ringSegmentCount scnTorus =
  sendMessage scnTorus ringSegmentCountSelector

-- | ringSegmentCount
--
-- The number of subdivisions of the ring. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRingSegmentCount:@
setRingSegmentCount :: IsSCNTorus scnTorus => scnTorus -> CLong -> IO ()
setRingSegmentCount scnTorus value =
  sendMessage scnTorus setRingSegmentCountSelector value

-- | pipeSegmentCount
--
-- The number of subdivisions of the pipe. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- pipeSegmentCount@
pipeSegmentCount :: IsSCNTorus scnTorus => scnTorus -> IO CLong
pipeSegmentCount scnTorus =
  sendMessage scnTorus pipeSegmentCountSelector

-- | pipeSegmentCount
--
-- The number of subdivisions of the pipe. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- setPipeSegmentCount:@
setPipeSegmentCount :: IsSCNTorus scnTorus => scnTorus -> CLong -> IO ()
setPipeSegmentCount scnTorus value =
  sendMessage scnTorus setPipeSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @torusWithRingRadius:pipeRadius:@
torusWithRingRadius_pipeRadiusSelector :: Selector '[CDouble, CDouble] (Id SCNTorus)
torusWithRingRadius_pipeRadiusSelector = mkSelector "torusWithRingRadius:pipeRadius:"

-- | @Selector@ for @ringRadius@
ringRadiusSelector :: Selector '[] CDouble
ringRadiusSelector = mkSelector "ringRadius"

-- | @Selector@ for @setRingRadius:@
setRingRadiusSelector :: Selector '[CDouble] ()
setRingRadiusSelector = mkSelector "setRingRadius:"

-- | @Selector@ for @pipeRadius@
pipeRadiusSelector :: Selector '[] CDouble
pipeRadiusSelector = mkSelector "pipeRadius"

-- | @Selector@ for @setPipeRadius:@
setPipeRadiusSelector :: Selector '[CDouble] ()
setPipeRadiusSelector = mkSelector "setPipeRadius:"

-- | @Selector@ for @ringSegmentCount@
ringSegmentCountSelector :: Selector '[] CLong
ringSegmentCountSelector = mkSelector "ringSegmentCount"

-- | @Selector@ for @setRingSegmentCount:@
setRingSegmentCountSelector :: Selector '[CLong] ()
setRingSegmentCountSelector = mkSelector "setRingSegmentCount:"

-- | @Selector@ for @pipeSegmentCount@
pipeSegmentCountSelector :: Selector '[] CLong
pipeSegmentCountSelector = mkSelector "pipeSegmentCount"

-- | @Selector@ for @setPipeSegmentCount:@
setPipeSegmentCountSelector :: Selector '[CLong] ()
setPipeSegmentCountSelector = mkSelector "setPipeSegmentCount:"

