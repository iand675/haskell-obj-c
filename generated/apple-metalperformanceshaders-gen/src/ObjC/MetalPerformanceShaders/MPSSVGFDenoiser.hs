{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A convenience object which uses an MPSSVGF object to manage the denoising process
--
-- The MPSSVGF object can also be used directly to customize the denoising process. This object keeps track of auxilary textures used by the MPSSVGF object, manages a temporal history, and encodes the entire denoising process into a command buffer.
--
-- To use this class, first create and customize an MPSSVGF object. This object allows you to tweak various aspect of the denoising process such as temporal reprojection and bilateral blur settings. Then create a texture allocator object which will allocate temporary textures during denoising. This can either be an object conforming to the MPSSVGFTextureAllocator protocol or an instance of the MPSSVGFDefaultTextureAllocator class. Next, create an MPSSVGFDenoiser object. To perform denoising, assign inputs textures to the denoiser object's properties and call encodeToCommandBuffer:. Finally, read the output from the destinationTexture property. Note that this class can denoise up to two independent textures simultaneously, e.g. specular and diffuse, direct and indirect lighting, shadows and AO, etc.
--
-- MPSSVGF *svgf = [[MPSSVGF alloc] initWithDevice:device];
--
-- // configure svgf properties
--
-- MPSSVGFDefaultTextureAllocator *allocator =
-- [[MPSSVGFDefaultTextureAllocator alloc] initWithDevice:device];
--
-- MPSSVGFDenoiser *denoiser = [[MPSSVGFDenoiser alloc] initWithSVGF:svgf
-- textureAllocator:allocator];
--
-- // configure denoiser properties
--
-- denoiser.sourceTexture = noisyTexture;
-- denoiser.depthNormalTexture = depthNormalTexture;
-- denoiser.previousDepthNormalTexture = depthNormalTextureFromPreviousFrame;
-- denoiser.motionVectorTexture = motionVectorTexture;
--
-- [denoiser encodeToCommandBuffer:commandBuffer];
--
-- id <MTLTexture> cleanTexture = denoiser.destinationTexture;
--
-- Generated bindings for @MPSSVGFDenoiser@.
module ObjC.MetalPerformanceShaders.MPSSVGFDenoiser
  ( MPSSVGFDenoiser
  , IsMPSSVGFDenoiser(..)
  , initWithDevice
  , initWithSVGF_textureAllocator
  , clearTemporalHistory
  , releaseTemporaryTextures
  , encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture
  , encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture
  , svgf
  , textureAllocator
  , bilateralFilterIterations
  , setBilateralFilterIterations
  , bilateralFilterIterationsSelector
  , clearTemporalHistorySelector
  , encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , initWithDeviceSelector
  , initWithSVGF_textureAllocatorSelector
  , releaseTemporaryTexturesSelector
  , setBilateralFilterIterationsSelector
  , svgfSelector
  , textureAllocatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the MPSSVGFDenoiser object
--
-- device The Metal device to use for denoising
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> IO (Id MPSSVGFDenoiser)
initWithDevice mpssvgfDenoiser device =
  sendOwnedMessage mpssvgfDenoiser initWithDeviceSelector device

-- | Initialize the MPSSVGFDenoiser object
--
-- svgf             MPSSVGF kernels to use for denoising. This object can be used to                             configure temporal reprojection, bilateral blur settings, etc.  textureAllocator An object conforming to the MPSSVGFTextureAllocator protocol. This                             object will be used to allocate temporary intermediate and output                             textures. This can be a custom object or an instance of the                             MPSSVGFDefaultTextureAllocator class.
--
-- ObjC selector: @- initWithSVGF:textureAllocator:@
initWithSVGF_textureAllocator :: (IsMPSSVGFDenoiser mpssvgfDenoiser, IsMPSSVGF svgf) => mpssvgfDenoiser -> svgf -> RawId -> IO (Id MPSSVGFDenoiser)
initWithSVGF_textureAllocator mpssvgfDenoiser svgf textureAllocator =
  sendOwnedMessage mpssvgfDenoiser initWithSVGF_textureAllocatorSelector (toMPSSVGF svgf) textureAllocator

-- | Clear the temporal history. Reprojection and temporal accumulation will restart on the next call to encodeToCommandBuffer:
--
-- ObjC selector: @- clearTemporalHistory@
clearTemporalHistory :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO ()
clearTemporalHistory mpssvgfDenoiser =
  sendMessage mpssvgfDenoiser clearTemporalHistorySelector

-- | Return any temporary textures to the texture allocator. Also clears the temporal history. This should be called before resizing the source texture(s).
--
-- ObjC selector: @- releaseTemporaryTextures@
releaseTemporaryTextures :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO ()
releaseTemporaryTextures mpssvgfDenoiser =
  sendMessage mpssvgfDenoiser releaseTemporaryTexturesSelector

-- | Encode denoising kernels to a command buffer
--
-- Removes noise from the source texture, using the additional data in the motion vector, depth/normal, and previous depth/normal textures. Returns the resulting texture. The depth/normal texture should be provided as the previous depth/normal texture for the next call to this method. This method will also update an internally managed temporal history to aid the denoising process. To reset this history, call the clearTemporalHistory method. This method will allocate and return several textures from and to the texture allocator the MPSSVGFDenoiser was initialized with. The number of iterations of the bilateral filter is controlled by the bilateralFilterIterations property. Larger numbers of iterations will improve the quality but reduce performance. To configure other parameters of the denoising process, modify the properties of the MPSSVGF object the MPSSVGFDenoiser was initialized with.
--
-- commandBuffer              Command buffer to encode into  sourceTexture              Source image to denoiser  motionVectorTexture        Motion vector texture describing how much each texel has moved,                                       in texels, since the previous frame. See the MPSSVGF object for                                       more details.  depthNormalTexture         Texture containing linear depth in the X component and signed                                       normals in the YZW components. See the MPSSVGF object for more                                       details.  previousDepthNormalTexture Depth/normal texture from the previous frame. See the MPSSVGF                                       object for more details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgfDenoiser commandBuffer sourceTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  sendMessage mpssvgfDenoiser encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector commandBuffer sourceTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture

-- | Encode denoising kernels to a command buffer
--
-- Simultaneously removes noise from the source texture and optional second source texture, using the additional data in the motion vector, depth/normal, and previous depth/normal textures. Returns the result through the destination texture pointers. The depth/normal texture should be provided as the previous depth/normal texture for the next call to this method. This method will also update an internally managed temporal history to aid the denoising process. To reset this history, call the clearTemporalHistory method. This method will allocate and return several textures from and to the texture allocator the MPSSVGFDenoiser was initialized with. The number of iterations of the bilateral filter is controlled by the bilateralFilterIterations property. Larger numbers of iterations will improve the quality but reduce performance. To configure other parameters of the denoising process, modify the properties of the MPSSVGF object the MPSSVGFDenoiser was initialized with.
--
-- commandBuffer              Command buffer to encode into  sourceTexture              Source image to denoiser  destinationTexture         Denoised output image  sourceTexture2             Optional second source image to denoise  destinationTexture2        Denoised second output image, if there is a second source image  motionVectorTexture        Motion vector texture describing how much each texel has moved,                                       in texels, since the previous frame. See the MPSSVGF object for                                       more details.  depthNormalTexture         Texture containing linear depth in the X component and signed                                       normals in the YZW components. See the MPSSVGF object for more                                       details.  previousDepthNormalTexture Depth/normal texture from the previous frame. See the MPSSVGF                                       object for more details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgfDenoiser commandBuffer sourceTexture destinationTexture sourceTexture2 destinationTexture2 motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  sendMessage mpssvgfDenoiser encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector commandBuffer sourceTexture destinationTexture sourceTexture2 destinationTexture2 motionVectorTexture depthNormalTexture previousDepthNormalTexture

-- | The underlying MPSSVGF kernels object which will be used for denoising. Use this object to customize the denoising process.
--
-- ObjC selector: @- svgf@
svgf :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO (Id MPSSVGF)
svgf mpssvgfDenoiser =
  sendMessage mpssvgfDenoiser svgfSelector

-- | The object which will be used to allocate intermediate and output textures.
--
-- ObjC selector: @- textureAllocator@
textureAllocator :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO RawId
textureAllocator mpssvgfDenoiser =
  sendMessage mpssvgfDenoiser textureAllocatorSelector

-- | The number of bilateral filter iterations to run. More iterations will improve quality at the cost of performance. Defaults to 5. Must be at least 1.
--
-- ObjC selector: @- bilateralFilterIterations@
bilateralFilterIterations :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO CULong
bilateralFilterIterations mpssvgfDenoiser =
  sendMessage mpssvgfDenoiser bilateralFilterIterationsSelector

-- | The number of bilateral filter iterations to run. More iterations will improve quality at the cost of performance. Defaults to 5. Must be at least 1.
--
-- ObjC selector: @- setBilateralFilterIterations:@
setBilateralFilterIterations :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> CULong -> IO ()
setBilateralFilterIterations mpssvgfDenoiser value =
  sendMessage mpssvgfDenoiser setBilateralFilterIterationsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSSVGFDenoiser)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithSVGF:textureAllocator:@
initWithSVGF_textureAllocatorSelector :: Selector '[Id MPSSVGF, RawId] (Id MPSSVGFDenoiser)
initWithSVGF_textureAllocatorSelector = mkSelector "initWithSVGF:textureAllocator:"

-- | @Selector@ for @clearTemporalHistory@
clearTemporalHistorySelector :: Selector '[] ()
clearTemporalHistorySelector = mkSelector "clearTemporalHistory"

-- | @Selector@ for @releaseTemporaryTextures@
releaseTemporaryTexturesSelector :: Selector '[] ()
releaseTemporaryTexturesSelector = mkSelector "releaseTemporaryTextures"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId] RawId
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @svgf@
svgfSelector :: Selector '[] (Id MPSSVGF)
svgfSelector = mkSelector "svgf"

-- | @Selector@ for @textureAllocator@
textureAllocatorSelector :: Selector '[] RawId
textureAllocatorSelector = mkSelector "textureAllocator"

-- | @Selector@ for @bilateralFilterIterations@
bilateralFilterIterationsSelector :: Selector '[] CULong
bilateralFilterIterationsSelector = mkSelector "bilateralFilterIterations"

-- | @Selector@ for @setBilateralFilterIterations:@
setBilateralFilterIterationsSelector :: Selector '[CULong] ()
setBilateralFilterIterationsSelector = mkSelector "setBilateralFilterIterations:"

