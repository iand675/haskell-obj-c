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
  , bilateralFilterIterations
  , setBilateralFilterIterations
  , initWithDeviceSelector
  , initWithSVGF_textureAllocatorSelector
  , clearTemporalHistorySelector
  , releaseTemporaryTexturesSelector
  , encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , svgfSelector
  , bilateralFilterIterationsSelector
  , setBilateralFilterIterationsSelector


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

-- | Initialize the MPSSVGFDenoiser object
--
-- device The Metal device to use for denoising
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> IO (Id MPSSVGFDenoiser)
initWithDevice mpssvgfDenoiser  device =
  sendMsg mpssvgfDenoiser (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the MPSSVGFDenoiser object
--
-- svgf             MPSSVGF kernels to use for denoising. This object can be used to                             configure temporal reprojection, bilateral blur settings, etc.  textureAllocator An object conforming to the MPSSVGFTextureAllocator protocol. This                             object will be used to allocate temporary intermediate and output                             textures. This can be a custom object or an instance of the                             MPSSVGFDefaultTextureAllocator class.
--
-- ObjC selector: @- initWithSVGF:textureAllocator:@
initWithSVGF_textureAllocator :: (IsMPSSVGFDenoiser mpssvgfDenoiser, IsMPSSVGF svgf) => mpssvgfDenoiser -> svgf -> RawId -> IO (Id MPSSVGFDenoiser)
initWithSVGF_textureAllocator mpssvgfDenoiser  svgf textureAllocator =
withObjCPtr svgf $ \raw_svgf ->
    sendMsg mpssvgfDenoiser (mkSelector "initWithSVGF:textureAllocator:") (retPtr retVoid) [argPtr (castPtr raw_svgf :: Ptr ()), argPtr (castPtr (unRawId textureAllocator) :: Ptr ())] >>= ownedObject . castPtr

-- | Clear the temporal history. Reprojection and temporal accumulation will restart on the next call to encodeToCommandBuffer:
--
-- ObjC selector: @- clearTemporalHistory@
clearTemporalHistory :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO ()
clearTemporalHistory mpssvgfDenoiser  =
  sendMsg mpssvgfDenoiser (mkSelector "clearTemporalHistory") retVoid []

-- | Return any temporary textures to the texture allocator. Also clears the temporal history. This should be called before resizing the source texture(s).
--
-- ObjC selector: @- releaseTemporaryTextures@
releaseTemporaryTextures :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO ()
releaseTemporaryTextures mpssvgfDenoiser  =
  sendMsg mpssvgfDenoiser (mkSelector "releaseTemporaryTextures") retVoid []

-- | Encode denoising kernels to a command buffer
--
-- Removes noise from the source texture, using the additional data in the motion vector, depth/normal, and previous depth/normal textures. Returns the resulting texture. The depth/normal texture should be provided as the previous depth/normal texture for the next call to this method. This method will also update an internally managed temporal history to aid the denoising process. To reset this history, call the clearTemporalHistory method. This method will allocate and return several textures from and to the texture allocator the MPSSVGFDenoiser was initialized with. The number of iterations of the bilateral filter is controlled by the bilateralFilterIterations property. Larger numbers of iterations will improve the quality but reduce performance. To configure other parameters of the denoising process, modify the properties of the MPSSVGF object the MPSSVGFDenoiser was initialized with.
--
-- commandBuffer              Command buffer to encode into  sourceTexture              Source image to denoiser  motionVectorTexture        Motion vector texture describing how much each texel has moved,                                       in texels, since the previous frame. See the MPSSVGF object for                                       more details.  depthNormalTexture         Texture containing linear depth in the X component and signed                                       normals in the YZW components. See the MPSSVGF object for more                                       details.  previousDepthNormalTexture Depth/normal texture from the previous frame. See the MPSSVGF                                       object for more details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgfDenoiser  commandBuffer sourceTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  fmap (RawId . castPtr) $ sendMsg mpssvgfDenoiser (mkSelector "encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceTexture) :: Ptr ()), argPtr (castPtr (unRawId motionVectorTexture) :: Ptr ()), argPtr (castPtr (unRawId depthNormalTexture) :: Ptr ()), argPtr (castPtr (unRawId previousDepthNormalTexture) :: Ptr ())]

-- | Encode denoising kernels to a command buffer
--
-- Simultaneously removes noise from the source texture and optional second source texture, using the additional data in the motion vector, depth/normal, and previous depth/normal textures. Returns the result through the destination texture pointers. The depth/normal texture should be provided as the previous depth/normal texture for the next call to this method. This method will also update an internally managed temporal history to aid the denoising process. To reset this history, call the clearTemporalHistory method. This method will allocate and return several textures from and to the texture allocator the MPSSVGFDenoiser was initialized with. The number of iterations of the bilateral filter is controlled by the bilateralFilterIterations property. Larger numbers of iterations will improve the quality but reduce performance. To configure other parameters of the denoising process, modify the properties of the MPSSVGF object the MPSSVGFDenoiser was initialized with.
--
-- commandBuffer              Command buffer to encode into  sourceTexture              Source image to denoiser  destinationTexture         Denoised output image  sourceTexture2             Optional second source image to denoise  destinationTexture2        Denoised second output image, if there is a second source image  motionVectorTexture        Motion vector texture describing how much each texel has moved,                                       in texels, since the previous frame. See the MPSSVGF object for                                       more details.  depthNormalTexture         Texture containing linear depth in the X component and signed                                       normals in the YZW components. See the MPSSVGF object for more                                       details.  previousDepthNormalTexture Depth/normal texture from the previous frame. See the MPSSVGF                                       object for more details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgfDenoiser  commandBuffer sourceTexture destinationTexture sourceTexture2 destinationTexture2 motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  sendMsg mpssvgfDenoiser (mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceTexture) :: Ptr ()), argPtr (castPtr (unRawId destinationTexture) :: Ptr ()), argPtr (castPtr (unRawId sourceTexture2) :: Ptr ()), argPtr (castPtr (unRawId destinationTexture2) :: Ptr ()), argPtr (castPtr (unRawId motionVectorTexture) :: Ptr ()), argPtr (castPtr (unRawId depthNormalTexture) :: Ptr ()), argPtr (castPtr (unRawId previousDepthNormalTexture) :: Ptr ())]

-- | The underlying MPSSVGF kernels object which will be used for denoising. Use this object to customize the denoising process.
--
-- ObjC selector: @- svgf@
svgf :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO (Id MPSSVGF)
svgf mpssvgfDenoiser  =
  sendMsg mpssvgfDenoiser (mkSelector "svgf") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The number of bilateral filter iterations to run. More iterations will improve quality at the cost of performance. Defaults to 5. Must be at least 1.
--
-- ObjC selector: @- bilateralFilterIterations@
bilateralFilterIterations :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> IO CULong
bilateralFilterIterations mpssvgfDenoiser  =
  sendMsg mpssvgfDenoiser (mkSelector "bilateralFilterIterations") retCULong []

-- | The number of bilateral filter iterations to run. More iterations will improve quality at the cost of performance. Defaults to 5. Must be at least 1.
--
-- ObjC selector: @- setBilateralFilterIterations:@
setBilateralFilterIterations :: IsMPSSVGFDenoiser mpssvgfDenoiser => mpssvgfDenoiser -> CULong -> IO ()
setBilateralFilterIterations mpssvgfDenoiser  value =
  sendMsg mpssvgfDenoiser (mkSelector "setBilateralFilterIterations:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithSVGF:textureAllocator:@
initWithSVGF_textureAllocatorSelector :: Selector
initWithSVGF_textureAllocatorSelector = mkSelector "initWithSVGF:textureAllocator:"

-- | @Selector@ for @clearTemporalHistory@
clearTemporalHistorySelector :: Selector
clearTemporalHistorySelector = mkSelector "clearTemporalHistory"

-- | @Selector@ for @releaseTemporaryTextures@
releaseTemporaryTexturesSelector :: Selector
releaseTemporaryTexturesSelector = mkSelector "releaseTemporaryTextures"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector
encodeToCommandBuffer_sourceTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector
encodeToCommandBuffer_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @svgf@
svgfSelector :: Selector
svgfSelector = mkSelector "svgf"

-- | @Selector@ for @bilateralFilterIterations@
bilateralFilterIterationsSelector :: Selector
bilateralFilterIterationsSelector = mkSelector "bilateralFilterIterations"

-- | @Selector@ for @setBilateralFilterIterations:@
setBilateralFilterIterationsSelector :: Selector
setBilateralFilterIterationsSelector = mkSelector "setBilateralFilterIterations:"

