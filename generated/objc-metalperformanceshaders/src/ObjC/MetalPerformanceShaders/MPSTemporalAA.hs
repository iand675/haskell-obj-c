{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Reduces aliasing in an image by accumulating samples over multiple frames
--
-- The color for the previous frame will be sampled using the provided motion vector texture and blended with the current frame according to the blendFactor property. The colors from the previous frame will be clamped to the color-space bounding box formed by the center pixel's neighbors to avoid reprojection artifacts, and the motion vector texture will be dilated to avoid aliased silhouette edges under motion.
--
-- For the best result, the sample positions produced by the renderer should be jittered every frame, ideally using a low discrepency sequence. This will ensure that different sample positions along edges will be visited over time even if the camera is not moving. This will also reduce aliasing due to textures and high-frequency shading.
--
-- For reference, see "High-Quality Temporal Supersampling" by Karis.
--
-- Generated bindings for @MPSTemporalAA@.
module ObjC.MetalPerformanceShaders.MPSTemporalAA
  ( MPSTemporalAA
  , IsMPSTemporalAA(..)
  , initWithDevice
  , initWithCoder_device
  , copyWithZone_device
  , encodeWithCoder
  , encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTexture
  , blendFactor
  , setBlendFactor
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , encodeWithCoderSelector
  , encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTextureSelector
  , blendFactorSelector
  , setBlendFactorSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSTemporalAA mpsTemporalAA => mpsTemporalAA -> RawId -> IO (Id MPSTemporalAA)
initWithDevice mpsTemporalAA  device =
  sendMsg mpsTemporalAA (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSTemporalAA mpsTemporalAA, IsNSCoder aDecoder) => mpsTemporalAA -> aDecoder -> RawId -> IO (Id MPSTemporalAA)
initWithCoder_device mpsTemporalAA  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsTemporalAA (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- copyWithZone:device:@
copyWithZone_device :: IsMPSTemporalAA mpsTemporalAA => mpsTemporalAA -> Ptr () -> RawId -> IO (Id MPSTemporalAA)
copyWithZone_device mpsTemporalAA  zone device =
  sendMsg mpsTemporalAA (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- encodeWithCoder:@
encodeWithCoder :: (IsMPSTemporalAA mpsTemporalAA, IsNSCoder coder) => mpsTemporalAA -> coder -> IO ()
encodeWithCoder mpsTemporalAA  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsTemporalAA (mkSelector "encodeWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | Encode temporal antialiasing a command buffer
--
-- The motion vector texture must be at least a two channel texture representing how many texels each texel in the source image(s) have moved since the previous frame. The remaining channels will be ignored if present. This texture may be nil, in which case the motion vector is assumed to be zero, which is suitable for static images.
--
-- The depth texture must contain the depth values for directly visible geometry for the current frame for each pixel. The first channel must store the depth value from zero to infinity. The depth texture may be nil, but this will prevent motion vectors from being dilated and may introduce aliasing along silhouette edges.
--
-- The destination texture should be used as the previous texture in the next frame.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @sourceTexture@ — Current frame to denoise
--
-- @previousTexture@ — Previous denoised frame to reproject into current                            frame
--
-- @destinationTexture@ — Output blended image
--
-- @motionVectorTexture@ — Motion vector texture
--
-- @depthTexture@ — The depth values for the current frame
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:previousTexture:destinationTexture:motionVectorTexture:depthTexture:@
encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTexture :: IsMPSTemporalAA mpsTemporalAA => mpsTemporalAA -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTexture mpsTemporalAA  commandBuffer sourceTexture previousTexture destinationTexture motionVectorTexture depthTexture =
  sendMsg mpsTemporalAA (mkSelector "encodeToCommandBuffer:sourceTexture:previousTexture:destinationTexture:motionVectorTexture:depthTexture:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceTexture) :: Ptr ()), argPtr (castPtr (unRawId previousTexture) :: Ptr ()), argPtr (castPtr (unRawId destinationTexture) :: Ptr ()), argPtr (castPtr (unRawId motionVectorTexture) :: Ptr ()), argPtr (castPtr (unRawId depthTexture) :: Ptr ())]

-- | How much to blend the current frame with the previous frame during temporal antialiasing. The final value is given by current * blendFactor + previous * (1 - blendFactor). Must be between zero and one, inclusive. Defaults to 0.1.
--
-- ObjC selector: @- blendFactor@
blendFactor :: IsMPSTemporalAA mpsTemporalAA => mpsTemporalAA -> IO CFloat
blendFactor mpsTemporalAA  =
  sendMsg mpsTemporalAA (mkSelector "blendFactor") retCFloat []

-- | How much to blend the current frame with the previous frame during temporal antialiasing. The final value is given by current * blendFactor + previous * (1 - blendFactor). Must be between zero and one, inclusive. Defaults to 0.1.
--
-- ObjC selector: @- setBlendFactor:@
setBlendFactor :: IsMPSTemporalAA mpsTemporalAA => mpsTemporalAA -> CFloat -> IO ()
setBlendFactor mpsTemporalAA  value =
  sendMsg mpsTemporalAA (mkSelector "setBlendFactor:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:previousTexture:destinationTexture:motionVectorTexture:depthTexture:@
encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTextureSelector :: Selector
encodeToCommandBuffer_sourceTexture_previousTexture_destinationTexture_motionVectorTexture_depthTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:previousTexture:destinationTexture:motionVectorTexture:depthTexture:"

-- | @Selector@ for @blendFactor@
blendFactorSelector :: Selector
blendFactorSelector = mkSelector "blendFactor"

-- | @Selector@ for @setBlendFactor:@
setBlendFactorSelector :: Selector
setBlendFactorSelector = mkSelector "setBlendFactor:"

