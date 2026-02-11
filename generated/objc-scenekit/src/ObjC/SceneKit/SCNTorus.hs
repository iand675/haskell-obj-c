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
  , torusWithRingRadius_pipeRadiusSelector
  , ringRadiusSelector
  , setRingRadiusSelector
  , pipeRadiusSelector
  , setPipeRadiusSelector
  , ringSegmentCountSelector
  , setRingSegmentCountSelector
  , pipeSegmentCountSelector
  , setPipeSegmentCountSelector


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
    sendClassMsg cls' (mkSelector "torusWithRingRadius:pipeRadius:") (retPtr retVoid) [argCDouble (fromIntegral ringRadius), argCDouble (fromIntegral pipeRadius)] >>= retainedObject . castPtr

-- | ringRadius
--
-- The radius of the torus ring. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- ringRadius@
ringRadius :: IsSCNTorus scnTorus => scnTorus -> IO CDouble
ringRadius scnTorus  =
  sendMsg scnTorus (mkSelector "ringRadius") retCDouble []

-- | ringRadius
--
-- The radius of the torus ring. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.5.
--
-- ObjC selector: @- setRingRadius:@
setRingRadius :: IsSCNTorus scnTorus => scnTorus -> CDouble -> IO ()
setRingRadius scnTorus  value =
  sendMsg scnTorus (mkSelector "setRingRadius:") retVoid [argCDouble (fromIntegral value)]

-- | pipeRadius
--
-- The radius of the torus pipe. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- pipeRadius@
pipeRadius :: IsSCNTorus scnTorus => scnTorus -> IO CDouble
pipeRadius scnTorus  =
  sendMsg scnTorus (mkSelector "pipeRadius") retCDouble []

-- | pipeRadius
--
-- The radius of the torus pipe. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 0.25.
--
-- ObjC selector: @- setPipeRadius:@
setPipeRadius :: IsSCNTorus scnTorus => scnTorus -> CDouble -> IO ()
setPipeRadius scnTorus  value =
  sendMsg scnTorus (mkSelector "setPipeRadius:") retVoid [argCDouble (fromIntegral value)]

-- | ringSegmentCount
--
-- The number of subdivisions of the ring. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- ringSegmentCount@
ringSegmentCount :: IsSCNTorus scnTorus => scnTorus -> IO CLong
ringSegmentCount scnTorus  =
  sendMsg scnTorus (mkSelector "ringSegmentCount") retCLong []

-- | ringSegmentCount
--
-- The number of subdivisions of the ring. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 48.
--
-- ObjC selector: @- setRingSegmentCount:@
setRingSegmentCount :: IsSCNTorus scnTorus => scnTorus -> CLong -> IO ()
setRingSegmentCount scnTorus  value =
  sendMsg scnTorus (mkSelector "setRingSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | pipeSegmentCount
--
-- The number of subdivisions of the pipe. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- pipeSegmentCount@
pipeSegmentCount :: IsSCNTorus scnTorus => scnTorus -> IO CLong
pipeSegmentCount scnTorus  =
  sendMsg scnTorus (mkSelector "pipeSegmentCount") retCLong []

-- | pipeSegmentCount
--
-- The number of subdivisions of the pipe. Animatable.
--
-- If the value is less than 3, the behavior is undefined. The default value is 24.
--
-- ObjC selector: @- setPipeSegmentCount:@
setPipeSegmentCount :: IsSCNTorus scnTorus => scnTorus -> CLong -> IO ()
setPipeSegmentCount scnTorus  value =
  sendMsg scnTorus (mkSelector "setPipeSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @torusWithRingRadius:pipeRadius:@
torusWithRingRadius_pipeRadiusSelector :: Selector
torusWithRingRadius_pipeRadiusSelector = mkSelector "torusWithRingRadius:pipeRadius:"

-- | @Selector@ for @ringRadius@
ringRadiusSelector :: Selector
ringRadiusSelector = mkSelector "ringRadius"

-- | @Selector@ for @setRingRadius:@
setRingRadiusSelector :: Selector
setRingRadiusSelector = mkSelector "setRingRadius:"

-- | @Selector@ for @pipeRadius@
pipeRadiusSelector :: Selector
pipeRadiusSelector = mkSelector "pipeRadius"

-- | @Selector@ for @setPipeRadius:@
setPipeRadiusSelector :: Selector
setPipeRadiusSelector = mkSelector "setPipeRadius:"

-- | @Selector@ for @ringSegmentCount@
ringSegmentCountSelector :: Selector
ringSegmentCountSelector = mkSelector "ringSegmentCount"

-- | @Selector@ for @setRingSegmentCount:@
setRingSegmentCountSelector :: Selector
setRingSegmentCountSelector = mkSelector "setRingSegmentCount:"

-- | @Selector@ for @pipeSegmentCount@
pipeSegmentCountSelector :: Selector
pipeSegmentCountSelector = mkSelector "pipeSegmentCount"

-- | @Selector@ for @setPipeSegmentCount:@
setPipeSegmentCountSelector :: Selector
setPipeSegmentCountSelector = mkSelector "setPipeSegmentCount:"

