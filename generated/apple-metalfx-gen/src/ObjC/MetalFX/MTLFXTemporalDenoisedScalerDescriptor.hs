{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLFXTemporalDenoisedScalerDescriptor@.
module ObjC.MetalFX.MTLFXTemporalDenoisedScalerDescriptor
  ( MTLFXTemporalDenoisedScalerDescriptor
  , IsMTLFXTemporalDenoisedScalerDescriptor(..)
  , newTemporalDenoisedScalerWithDevice
  , newTemporalDenoisedScalerWithDevice_compiler
  , supportedInputContentMinScaleForDevice
  , supportedInputContentMaxScaleForDevice
  , supportsMetal4FX
  , supportsDevice
  , colorTextureFormat
  , setColorTextureFormat
  , depthTextureFormat
  , setDepthTextureFormat
  , motionTextureFormat
  , setMotionTextureFormat
  , diffuseAlbedoTextureFormat
  , setDiffuseAlbedoTextureFormat
  , specularAlbedoTextureFormat
  , setSpecularAlbedoTextureFormat
  , normalTextureFormat
  , setNormalTextureFormat
  , roughnessTextureFormat
  , setRoughnessTextureFormat
  , specularHitDistanceTextureFormat
  , setSpecularHitDistanceTextureFormat
  , denoiseStrengthMaskTextureFormat
  , setDenoiseStrengthMaskTextureFormat
  , transparencyOverlayTextureFormat
  , setTransparencyOverlayTextureFormat
  , outputTextureFormat
  , setOutputTextureFormat
  , inputWidth
  , setInputWidth
  , inputHeight
  , setInputHeight
  , outputWidth
  , setOutputWidth
  , outputHeight
  , setOutputHeight
  , requiresSynchronousInitialization
  , setRequiresSynchronousInitialization
  , autoExposureEnabled
  , setAutoExposureEnabled
  , reactiveMaskTextureEnabled
  , setReactiveMaskTextureEnabled
  , reactiveMaskTextureFormat
  , setReactiveMaskTextureFormat
  , specularHitDistanceTextureEnabled
  , setSpecularHitDistanceTextureEnabled
  , denoiseStrengthMaskTextureEnabled
  , setDenoiseStrengthMaskTextureEnabled
  , transparencyOverlayTextureEnabled
  , setTransparencyOverlayTextureEnabled
  , autoExposureEnabledSelector
  , colorTextureFormatSelector
  , denoiseStrengthMaskTextureEnabledSelector
  , denoiseStrengthMaskTextureFormatSelector
  , depthTextureFormatSelector
  , diffuseAlbedoTextureFormatSelector
  , inputHeightSelector
  , inputWidthSelector
  , motionTextureFormatSelector
  , newTemporalDenoisedScalerWithDeviceSelector
  , newTemporalDenoisedScalerWithDevice_compilerSelector
  , normalTextureFormatSelector
  , outputHeightSelector
  , outputTextureFormatSelector
  , outputWidthSelector
  , reactiveMaskTextureEnabledSelector
  , reactiveMaskTextureFormatSelector
  , requiresSynchronousInitializationSelector
  , roughnessTextureFormatSelector
  , setAutoExposureEnabledSelector
  , setColorTextureFormatSelector
  , setDenoiseStrengthMaskTextureEnabledSelector
  , setDenoiseStrengthMaskTextureFormatSelector
  , setDepthTextureFormatSelector
  , setDiffuseAlbedoTextureFormatSelector
  , setInputHeightSelector
  , setInputWidthSelector
  , setMotionTextureFormatSelector
  , setNormalTextureFormatSelector
  , setOutputHeightSelector
  , setOutputTextureFormatSelector
  , setOutputWidthSelector
  , setReactiveMaskTextureEnabledSelector
  , setReactiveMaskTextureFormatSelector
  , setRequiresSynchronousInitializationSelector
  , setRoughnessTextureFormatSelector
  , setSpecularAlbedoTextureFormatSelector
  , setSpecularHitDistanceTextureEnabledSelector
  , setSpecularHitDistanceTextureFormatSelector
  , setTransparencyOverlayTextureEnabledSelector
  , setTransparencyOverlayTextureFormatSelector
  , specularAlbedoTextureFormatSelector
  , specularHitDistanceTextureEnabledSelector
  , specularHitDistanceTextureFormatSelector
  , supportedInputContentMaxScaleForDeviceSelector
  , supportedInputContentMinScaleForDeviceSelector
  , supportsDeviceSelector
  , supportsMetal4FXSelector
  , transparencyOverlayTextureEnabledSelector
  , transparencyOverlayTextureFormatSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalFX.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a denoiser scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the denoiser scaler. - Returns:    A denoiser scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalDenoisedScalerWithDevice:@
newTemporalDenoisedScalerWithDevice :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> RawId -> IO RawId
newTemporalDenoisedScalerWithDevice mtlfxTemporalDenoisedScalerDescriptor device =
  sendOwnedMessage mtlfxTemporalDenoisedScalerDescriptor newTemporalDenoisedScalerWithDeviceSelector device

-- | Creates a denoiser scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the denoiser scaler.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A denoiser scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalDenoisedScalerWithDevice:compiler:@
newTemporalDenoisedScalerWithDevice_compiler :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> RawId -> RawId -> IO RawId
newTemporalDenoisedScalerWithDevice_compiler mtlfxTemporalDenoisedScalerDescriptor device compiler =
  sendOwnedMessage mtlfxTemporalDenoisedScalerDescriptor newTemporalDenoisedScalerWithDevice_compilerSelector device compiler

-- | Returns the smallest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method checks the minimum input content scale it supports.
--
-- - Returns: the minimum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDevice :: RawId -> IO CFloat
supportedInputContentMinScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMessage cls' supportedInputContentMinScaleForDeviceSelector device

-- | Returns the largest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method checks the maximum input content scale it supports.
--
-- - Returns: the maximum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDevice :: RawId -> IO CFloat
supportedInputContentMaxScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMessage cls' supportedInputContentMaxScaleForDeviceSelector device

-- | Queries whether a Metal device supports denosing scaling compatible on Metal 4.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports denoising scaling for             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMessage cls' supportsMetal4FXSelector device

-- | Queries whether a Metal device supports denoising scaling.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports denoising scaling,             <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMessage cls' supportsDeviceSelector device

-- | The pixel format of the input color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
colorTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor colorTextureFormatSelector

-- | The pixel format of the input color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setColorTextureFormatSelector value

-- | The pixel format of the input depth texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- depthTextureFormat@
depthTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
depthTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor depthTextureFormatSelector

-- | The pixel format of the input depth texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setDepthTextureFormat:@
setDepthTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDepthTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setDepthTextureFormatSelector value

-- | The pixel format of the input motion texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- motionTextureFormat@
motionTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
motionTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor motionTextureFormatSelector

-- | The pixel format of the input motion texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setMotionTextureFormat:@
setMotionTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setMotionTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setMotionTextureFormatSelector value

-- | The pixel format of the input diffuse albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- diffuseAlbedoTextureFormat@
diffuseAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
diffuseAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor diffuseAlbedoTextureFormatSelector

-- | The pixel format of the input diffuse albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setDiffuseAlbedoTextureFormat:@
setDiffuseAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDiffuseAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setDiffuseAlbedoTextureFormatSelector value

-- | The pixel format of the input specular albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- specularAlbedoTextureFormat@
specularAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
specularAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor specularAlbedoTextureFormatSelector

-- | The pixel format of the input specular albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setSpecularAlbedoTextureFormat:@
setSpecularAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setSpecularAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setSpecularAlbedoTextureFormatSelector value

-- | The pixel format of the input normal texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- normalTextureFormat@
normalTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
normalTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor normalTextureFormatSelector

-- | The pixel format of the input normal texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setNormalTextureFormat:@
setNormalTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setNormalTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setNormalTextureFormatSelector value

-- | The pixel format of the input roughness texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- roughnessTextureFormat@
roughnessTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
roughnessTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor roughnessTextureFormatSelector

-- | The pixel format of the input roughness texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setRoughnessTextureFormat:@
setRoughnessTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setRoughnessTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setRoughnessTextureFormatSelector value

-- | The pixel format of the input specular hit texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- specularHitDistanceTextureFormat@
specularHitDistanceTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
specularHitDistanceTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor specularHitDistanceTextureFormatSelector

-- | The pixel format of the input specular hit texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setSpecularHitDistanceTextureFormat:@
setSpecularHitDistanceTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setSpecularHitDistanceTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setSpecularHitDistanceTextureFormatSelector value

-- | The pixel format of the input denoise strength mask texture for the scaler you create with this descriptor.
--
-- You typically set this to a single-channel texture format.
--
-- ObjC selector: @- denoiseStrengthMaskTextureFormat@
denoiseStrengthMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
denoiseStrengthMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor denoiseStrengthMaskTextureFormatSelector

-- | The pixel format of the input denoise strength mask texture for the scaler you create with this descriptor.
--
-- You typically set this to a single-channel texture format.
--
-- ObjC selector: @- setDenoiseStrengthMaskTextureFormat:@
setDenoiseStrengthMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDenoiseStrengthMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setDenoiseStrengthMaskTextureFormatSelector value

-- | The pixel format of the input transparency overlay texture for the scaler you create with this descriptor.
--
-- You typically set this to a 4-channel RGBA texture format.
--
-- ObjC selector: @- transparencyOverlayTextureFormat@
transparencyOverlayTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
transparencyOverlayTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor transparencyOverlayTextureFormatSelector

-- | The pixel format of the input transparency overlay texture for the scaler you create with this descriptor.
--
-- You typically set this to a 4-channel RGBA texture format.
--
-- ObjC selector: @- setTransparencyOverlayTextureFormat:@
setTransparencyOverlayTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setTransparencyOverlayTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setTransparencyOverlayTextureFormatSelector value

-- | The pixel format of the output color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
outputTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor outputTextureFormatSelector

-- | The pixel format of the output color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setOutputTextureFormatSelector value

-- | The width, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
inputWidth mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor inputWidthSelector

-- | The width, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setInputWidth mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setInputWidthSelector value

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
inputHeight mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor inputHeightSelector

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setInputHeight mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setInputHeightSelector value

-- | The width, in pixels, of the output color texture for the denoiser scaler.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
outputWidth mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor outputWidthSelector

-- | The width, in pixels, of the output color texture for the denoiser scaler.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setOutputWidth mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setOutputWidthSelector value

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
outputHeight mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor outputHeightSelector

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setOutputHeight mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setOutputHeightSelector value

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal denoised scaler instance that can denoise and upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalDenoisedScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- requiresSynchronousInitialization@
requiresSynchronousInitialization :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
requiresSynchronousInitialization mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor requiresSynchronousInitializationSelector

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal denoised scaler instance that can denoise and upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalDenoisedScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- setRequiresSynchronousInitialization:@
setRequiresSynchronousInitialization :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setRequiresSynchronousInitialization mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setRequiresSynchronousInitializationSelector value

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Denoiser scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- autoExposureEnabled@
autoExposureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
autoExposureEnabled mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor autoExposureEnabledSelector

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Denoiser scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setAutoExposureEnabled:@
setAutoExposureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setAutoExposureEnabled mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setAutoExposureEnabledSelector value

-- | A Boolean value that indicates whether a scaler you create from this descriptor applies a reactive mask.
--
-- ObjC selector: @- reactiveMaskTextureEnabled@
reactiveMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
reactiveMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor reactiveMaskTextureEnabledSelector

-- | A Boolean value that indicates whether a scaler you create from this descriptor applies a reactive mask.
--
-- ObjC selector: @- setReactiveMaskTextureEnabled:@
setReactiveMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setReactiveMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setReactiveMaskTextureEnabledSelector value

-- | The pixel format of the reactive mask input texture for a scaler you create from this descriptor.
--
-- ObjC selector: @- reactiveMaskTextureFormat@
reactiveMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
reactiveMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor reactiveMaskTextureFormatSelector

-- | The pixel format of the reactive mask input texture for a scaler you create from this descriptor.
--
-- ObjC selector: @- setReactiveMaskTextureFormat:@
setReactiveMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setReactiveMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setReactiveMaskTextureFormatSelector value

-- | A Boolean value indicating whether the scaler evaluates a specular hit distance texture as part of its operation.
--
-- ObjC selector: @- specularHitDistanceTextureEnabled@
specularHitDistanceTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
specularHitDistanceTextureEnabled mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor specularHitDistanceTextureEnabledSelector

-- | A Boolean value indicating whether the scaler evaluates a specular hit distance texture as part of its operation.
--
-- ObjC selector: @- setSpecularHitDistanceTextureEnabled:@
setSpecularHitDistanceTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setSpecularHitDistanceTextureEnabled mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setSpecularHitDistanceTextureEnabledSelector value

-- | A Boolean value indicating whether the scaler evaluates a denoise strength mask texture as part of its operation.
--
-- ObjC selector: @- denoiseStrengthMaskTextureEnabled@
denoiseStrengthMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
denoiseStrengthMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor denoiseStrengthMaskTextureEnabledSelector

-- | A Boolean value indicating whether the scaler evaluates a denoise strength mask texture as part of its operation.
--
-- ObjC selector: @- setDenoiseStrengthMaskTextureEnabled:@
setDenoiseStrengthMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setDenoiseStrengthMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setDenoiseStrengthMaskTextureEnabledSelector value

-- | A Boolean value indicating whether the scaler evaluates a transparency overlay texture as part of its operation.
--
-- ObjC selector: @- transparencyOverlayTextureEnabled@
transparencyOverlayTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
transparencyOverlayTextureEnabled mtlfxTemporalDenoisedScalerDescriptor =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor transparencyOverlayTextureEnabledSelector

-- | A Boolean value indicating whether the scaler evaluates a transparency overlay texture as part of its operation.
--
-- ObjC selector: @- setTransparencyOverlayTextureEnabled:@
setTransparencyOverlayTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setTransparencyOverlayTextureEnabled mtlfxTemporalDenoisedScalerDescriptor value =
  sendMessage mtlfxTemporalDenoisedScalerDescriptor setTransparencyOverlayTextureEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newTemporalDenoisedScalerWithDevice:@
newTemporalDenoisedScalerWithDeviceSelector :: Selector '[RawId] RawId
newTemporalDenoisedScalerWithDeviceSelector = mkSelector "newTemporalDenoisedScalerWithDevice:"

-- | @Selector@ for @newTemporalDenoisedScalerWithDevice:compiler:@
newTemporalDenoisedScalerWithDevice_compilerSelector :: Selector '[RawId, RawId] RawId
newTemporalDenoisedScalerWithDevice_compilerSelector = mkSelector "newTemporalDenoisedScalerWithDevice:compiler:"

-- | @Selector@ for @supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDeviceSelector :: Selector '[RawId] CFloat
supportedInputContentMinScaleForDeviceSelector = mkSelector "supportedInputContentMinScaleForDevice:"

-- | @Selector@ for @supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDeviceSelector :: Selector '[RawId] CFloat
supportedInputContentMaxScaleForDeviceSelector = mkSelector "supportedInputContentMaxScaleForDevice:"

-- | @Selector@ for @supportsMetal4FX:@
supportsMetal4FXSelector :: Selector '[RawId] Bool
supportsMetal4FXSelector = mkSelector "supportsMetal4FX:"

-- | @Selector@ for @supportsDevice:@
supportsDeviceSelector :: Selector '[RawId] Bool
supportsDeviceSelector = mkSelector "supportsDevice:"

-- | @Selector@ for @colorTextureFormat@
colorTextureFormatSelector :: Selector '[] CInt
colorTextureFormatSelector = mkSelector "colorTextureFormat"

-- | @Selector@ for @setColorTextureFormat:@
setColorTextureFormatSelector :: Selector '[CInt] ()
setColorTextureFormatSelector = mkSelector "setColorTextureFormat:"

-- | @Selector@ for @depthTextureFormat@
depthTextureFormatSelector :: Selector '[] CInt
depthTextureFormatSelector = mkSelector "depthTextureFormat"

-- | @Selector@ for @setDepthTextureFormat:@
setDepthTextureFormatSelector :: Selector '[CInt] ()
setDepthTextureFormatSelector = mkSelector "setDepthTextureFormat:"

-- | @Selector@ for @motionTextureFormat@
motionTextureFormatSelector :: Selector '[] CInt
motionTextureFormatSelector = mkSelector "motionTextureFormat"

-- | @Selector@ for @setMotionTextureFormat:@
setMotionTextureFormatSelector :: Selector '[CInt] ()
setMotionTextureFormatSelector = mkSelector "setMotionTextureFormat:"

-- | @Selector@ for @diffuseAlbedoTextureFormat@
diffuseAlbedoTextureFormatSelector :: Selector '[] CInt
diffuseAlbedoTextureFormatSelector = mkSelector "diffuseAlbedoTextureFormat"

-- | @Selector@ for @setDiffuseAlbedoTextureFormat:@
setDiffuseAlbedoTextureFormatSelector :: Selector '[CInt] ()
setDiffuseAlbedoTextureFormatSelector = mkSelector "setDiffuseAlbedoTextureFormat:"

-- | @Selector@ for @specularAlbedoTextureFormat@
specularAlbedoTextureFormatSelector :: Selector '[] CInt
specularAlbedoTextureFormatSelector = mkSelector "specularAlbedoTextureFormat"

-- | @Selector@ for @setSpecularAlbedoTextureFormat:@
setSpecularAlbedoTextureFormatSelector :: Selector '[CInt] ()
setSpecularAlbedoTextureFormatSelector = mkSelector "setSpecularAlbedoTextureFormat:"

-- | @Selector@ for @normalTextureFormat@
normalTextureFormatSelector :: Selector '[] CInt
normalTextureFormatSelector = mkSelector "normalTextureFormat"

-- | @Selector@ for @setNormalTextureFormat:@
setNormalTextureFormatSelector :: Selector '[CInt] ()
setNormalTextureFormatSelector = mkSelector "setNormalTextureFormat:"

-- | @Selector@ for @roughnessTextureFormat@
roughnessTextureFormatSelector :: Selector '[] CInt
roughnessTextureFormatSelector = mkSelector "roughnessTextureFormat"

-- | @Selector@ for @setRoughnessTextureFormat:@
setRoughnessTextureFormatSelector :: Selector '[CInt] ()
setRoughnessTextureFormatSelector = mkSelector "setRoughnessTextureFormat:"

-- | @Selector@ for @specularHitDistanceTextureFormat@
specularHitDistanceTextureFormatSelector :: Selector '[] CInt
specularHitDistanceTextureFormatSelector = mkSelector "specularHitDistanceTextureFormat"

-- | @Selector@ for @setSpecularHitDistanceTextureFormat:@
setSpecularHitDistanceTextureFormatSelector :: Selector '[CInt] ()
setSpecularHitDistanceTextureFormatSelector = mkSelector "setSpecularHitDistanceTextureFormat:"

-- | @Selector@ for @denoiseStrengthMaskTextureFormat@
denoiseStrengthMaskTextureFormatSelector :: Selector '[] CInt
denoiseStrengthMaskTextureFormatSelector = mkSelector "denoiseStrengthMaskTextureFormat"

-- | @Selector@ for @setDenoiseStrengthMaskTextureFormat:@
setDenoiseStrengthMaskTextureFormatSelector :: Selector '[CInt] ()
setDenoiseStrengthMaskTextureFormatSelector = mkSelector "setDenoiseStrengthMaskTextureFormat:"

-- | @Selector@ for @transparencyOverlayTextureFormat@
transparencyOverlayTextureFormatSelector :: Selector '[] CInt
transparencyOverlayTextureFormatSelector = mkSelector "transparencyOverlayTextureFormat"

-- | @Selector@ for @setTransparencyOverlayTextureFormat:@
setTransparencyOverlayTextureFormatSelector :: Selector '[CInt] ()
setTransparencyOverlayTextureFormatSelector = mkSelector "setTransparencyOverlayTextureFormat:"

-- | @Selector@ for @outputTextureFormat@
outputTextureFormatSelector :: Selector '[] CInt
outputTextureFormatSelector = mkSelector "outputTextureFormat"

-- | @Selector@ for @setOutputTextureFormat:@
setOutputTextureFormatSelector :: Selector '[CInt] ()
setOutputTextureFormatSelector = mkSelector "setOutputTextureFormat:"

-- | @Selector@ for @inputWidth@
inputWidthSelector :: Selector '[] CULong
inputWidthSelector = mkSelector "inputWidth"

-- | @Selector@ for @setInputWidth:@
setInputWidthSelector :: Selector '[CULong] ()
setInputWidthSelector = mkSelector "setInputWidth:"

-- | @Selector@ for @inputHeight@
inputHeightSelector :: Selector '[] CULong
inputHeightSelector = mkSelector "inputHeight"

-- | @Selector@ for @setInputHeight:@
setInputHeightSelector :: Selector '[CULong] ()
setInputHeightSelector = mkSelector "setInputHeight:"

-- | @Selector@ for @outputWidth@
outputWidthSelector :: Selector '[] CULong
outputWidthSelector = mkSelector "outputWidth"

-- | @Selector@ for @setOutputWidth:@
setOutputWidthSelector :: Selector '[CULong] ()
setOutputWidthSelector = mkSelector "setOutputWidth:"

-- | @Selector@ for @outputHeight@
outputHeightSelector :: Selector '[] CULong
outputHeightSelector = mkSelector "outputHeight"

-- | @Selector@ for @setOutputHeight:@
setOutputHeightSelector :: Selector '[CULong] ()
setOutputHeightSelector = mkSelector "setOutputHeight:"

-- | @Selector@ for @requiresSynchronousInitialization@
requiresSynchronousInitializationSelector :: Selector '[] Bool
requiresSynchronousInitializationSelector = mkSelector "requiresSynchronousInitialization"

-- | @Selector@ for @setRequiresSynchronousInitialization:@
setRequiresSynchronousInitializationSelector :: Selector '[Bool] ()
setRequiresSynchronousInitializationSelector = mkSelector "setRequiresSynchronousInitialization:"

-- | @Selector@ for @autoExposureEnabled@
autoExposureEnabledSelector :: Selector '[] Bool
autoExposureEnabledSelector = mkSelector "autoExposureEnabled"

-- | @Selector@ for @setAutoExposureEnabled:@
setAutoExposureEnabledSelector :: Selector '[Bool] ()
setAutoExposureEnabledSelector = mkSelector "setAutoExposureEnabled:"

-- | @Selector@ for @reactiveMaskTextureEnabled@
reactiveMaskTextureEnabledSelector :: Selector '[] Bool
reactiveMaskTextureEnabledSelector = mkSelector "reactiveMaskTextureEnabled"

-- | @Selector@ for @setReactiveMaskTextureEnabled:@
setReactiveMaskTextureEnabledSelector :: Selector '[Bool] ()
setReactiveMaskTextureEnabledSelector = mkSelector "setReactiveMaskTextureEnabled:"

-- | @Selector@ for @reactiveMaskTextureFormat@
reactiveMaskTextureFormatSelector :: Selector '[] CInt
reactiveMaskTextureFormatSelector = mkSelector "reactiveMaskTextureFormat"

-- | @Selector@ for @setReactiveMaskTextureFormat:@
setReactiveMaskTextureFormatSelector :: Selector '[CInt] ()
setReactiveMaskTextureFormatSelector = mkSelector "setReactiveMaskTextureFormat:"

-- | @Selector@ for @specularHitDistanceTextureEnabled@
specularHitDistanceTextureEnabledSelector :: Selector '[] Bool
specularHitDistanceTextureEnabledSelector = mkSelector "specularHitDistanceTextureEnabled"

-- | @Selector@ for @setSpecularHitDistanceTextureEnabled:@
setSpecularHitDistanceTextureEnabledSelector :: Selector '[Bool] ()
setSpecularHitDistanceTextureEnabledSelector = mkSelector "setSpecularHitDistanceTextureEnabled:"

-- | @Selector@ for @denoiseStrengthMaskTextureEnabled@
denoiseStrengthMaskTextureEnabledSelector :: Selector '[] Bool
denoiseStrengthMaskTextureEnabledSelector = mkSelector "denoiseStrengthMaskTextureEnabled"

-- | @Selector@ for @setDenoiseStrengthMaskTextureEnabled:@
setDenoiseStrengthMaskTextureEnabledSelector :: Selector '[Bool] ()
setDenoiseStrengthMaskTextureEnabledSelector = mkSelector "setDenoiseStrengthMaskTextureEnabled:"

-- | @Selector@ for @transparencyOverlayTextureEnabled@
transparencyOverlayTextureEnabledSelector :: Selector '[] Bool
transparencyOverlayTextureEnabledSelector = mkSelector "transparencyOverlayTextureEnabled"

-- | @Selector@ for @setTransparencyOverlayTextureEnabled:@
setTransparencyOverlayTextureEnabledSelector :: Selector '[Bool] ()
setTransparencyOverlayTextureEnabledSelector = mkSelector "setTransparencyOverlayTextureEnabled:"

