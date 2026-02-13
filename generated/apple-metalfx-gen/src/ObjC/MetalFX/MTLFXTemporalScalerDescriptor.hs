{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLFXTemporalScalerDescriptor@.
module ObjC.MetalFX.MTLFXTemporalScalerDescriptor
  ( MTLFXTemporalScalerDescriptor
  , IsMTLFXTemporalScalerDescriptor(..)
  , newTemporalScalerWithDevice
  , newTemporalScalerWithDevice_compiler
  , supportedInputContentMinScaleForDevice
  , supportedInputContentMaxScaleForDevice
  , supportsDevice
  , supportsMetal4FX
  , colorTextureFormat
  , setColorTextureFormat
  , depthTextureFormat
  , setDepthTextureFormat
  , motionTextureFormat
  , setMotionTextureFormat
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
  , autoExposureEnabled
  , setAutoExposureEnabled
  , requiresSynchronousInitialization
  , setRequiresSynchronousInitialization
  , inputContentPropertiesEnabled
  , setInputContentPropertiesEnabled
  , inputContentMinScale
  , setInputContentMinScale
  , inputContentMaxScale
  , setInputContentMaxScale
  , reactiveMaskTextureEnabled
  , setReactiveMaskTextureEnabled
  , reactiveMaskTextureFormat
  , setReactiveMaskTextureFormat
  , autoExposureEnabledSelector
  , colorTextureFormatSelector
  , depthTextureFormatSelector
  , inputContentMaxScaleSelector
  , inputContentMinScaleSelector
  , inputContentPropertiesEnabledSelector
  , inputHeightSelector
  , inputWidthSelector
  , motionTextureFormatSelector
  , newTemporalScalerWithDeviceSelector
  , newTemporalScalerWithDevice_compilerSelector
  , outputHeightSelector
  , outputTextureFormatSelector
  , outputWidthSelector
  , reactiveMaskTextureEnabledSelector
  , reactiveMaskTextureFormatSelector
  , requiresSynchronousInitializationSelector
  , setAutoExposureEnabledSelector
  , setColorTextureFormatSelector
  , setDepthTextureFormatSelector
  , setInputContentMaxScaleSelector
  , setInputContentMinScaleSelector
  , setInputContentPropertiesEnabledSelector
  , setInputHeightSelector
  , setInputWidthSelector
  , setMotionTextureFormatSelector
  , setOutputHeightSelector
  , setOutputTextureFormatSelector
  , setOutputWidthSelector
  , setReactiveMaskTextureEnabledSelector
  , setReactiveMaskTextureFormatSelector
  , setRequiresSynchronousInitializationSelector
  , supportedInputContentMaxScaleForDeviceSelector
  , supportedInputContentMinScaleForDeviceSelector
  , supportsDeviceSelector
  , supportsMetal4FXSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalFX.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a temporal scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the temporal scaler. - Returns:    A new temporal scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalScalerWithDevice:@
newTemporalScalerWithDevice :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> RawId -> IO RawId
newTemporalScalerWithDevice mtlfxTemporalScalerDescriptor device =
  sendOwnedMessage mtlfxTemporalScalerDescriptor newTemporalScalerWithDeviceSelector device

-- | Creates a temporal scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the temporal scaler.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A new temporal scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalScalerWithDevice:compiler:@
newTemporalScalerWithDevice_compiler :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> RawId -> RawId -> IO RawId
newTemporalScalerWithDevice_compiler mtlfxTemporalScalerDescriptor device compiler =
  sendOwnedMessage mtlfxTemporalScalerDescriptor newTemporalScalerWithDevice_compilerSelector device compiler

-- | Returns the smallest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method performs this check.
--
-- - Returns: the minimum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDevice :: RawId -> IO CFloat
supportedInputContentMinScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalScalerDescriptor"
    sendClassMessage cls' supportedInputContentMinScaleForDeviceSelector device

-- | Returns the largest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method performs this check.
--
-- - Returns: the maximum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDevice :: RawId -> IO CFloat
supportedInputContentMaxScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalScalerDescriptor"
    sendClassMessage cls' supportedInputContentMaxScaleForDeviceSelector device

-- | Returns a Boolean value that indicates whether the temporal scaler works with a GPU.
--
-- - Parameters:    - device: A device instance that represents a GPU.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports temporal scaling,            <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalScalerDescriptor"
    sendClassMessage cls' supportsDeviceSelector device

-- | Queries whether a Metal device supports temporal scaling compatible with Metal 4.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports temporal scaling with             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXTemporalScalerDescriptor"
    sendClassMessage cls' supportsMetal4FXSelector device

-- | The pixel format of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CInt
colorTextureFormat mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor colorTextureFormatSelector

-- | The pixel format of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setColorTextureFormatSelector value

-- | The pixel format of the input depth texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- depthTextureFormat@
depthTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CInt
depthTextureFormat mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor depthTextureFormatSelector

-- | The pixel format of the input depth texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setDepthTextureFormat:@
setDepthTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CInt -> IO ()
setDepthTextureFormat mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setDepthTextureFormatSelector value

-- | The pixel format of the input motion texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- motionTextureFormat@
motionTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CInt
motionTextureFormat mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor motionTextureFormatSelector

-- | The pixel format of the input motion texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setMotionTextureFormat:@
setMotionTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CInt -> IO ()
setMotionTextureFormat mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setMotionTextureFormatSelector value

-- | The pixel format of the output texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CInt
outputTextureFormat mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor outputTextureFormatSelector

-- | The pixel format of the output texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setOutputTextureFormatSelector value

-- | The width of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CULong
inputWidth mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor inputWidthSelector

-- | The width of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CULong -> IO ()
setInputWidth mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setInputWidthSelector value

-- | The height of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CULong
inputHeight mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor inputHeightSelector

-- | The height of the input color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CULong -> IO ()
setInputHeight mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setInputHeightSelector value

-- | The width of the output color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CULong
outputWidth mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor outputWidthSelector

-- | The width of the output color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CULong -> IO ()
setOutputWidth mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setOutputWidthSelector value

-- | The height of the output color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CULong
outputHeight mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor outputHeightSelector

-- | The height of the output color texture for the temporal scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CULong -> IO ()
setOutputHeight mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setOutputHeightSelector value

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Temporal scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- autoExposureEnabled@
autoExposureEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO Bool
autoExposureEnabled mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor autoExposureEnabledSelector

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Temporal scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setAutoExposureEnabled:@
setAutoExposureEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> Bool -> IO ()
setAutoExposureEnabled mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setAutoExposureEnabledSelector value

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal scaler that can upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- requiresSynchronousInitialization@
requiresSynchronousInitialization :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO Bool
requiresSynchronousInitialization mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor requiresSynchronousInitializationSelector

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal scaler that can upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- setRequiresSynchronousInitialization:@
setRequiresSynchronousInitialization :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> Bool -> IO ()
setRequiresSynchronousInitialization mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setRequiresSynchronousInitializationSelector value

-- | A Boolean value that indicates whether the temporal scaler you create with this descriptor uses dynamic resolution.
--
-- When you set this property to <doc://com.apple.documentation/documentation/swift/true> to enable dynamic resolution, scale properties ``inputContentMinScale`` and ``inputContentMaxScale`` represent the input and output resolution both the width and height.
--
-- * Note: The scaler assumes that aspect ratio of the input and output textures doesn't change.
--
-- ObjC selector: @- inputContentPropertiesEnabled@
inputContentPropertiesEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO Bool
inputContentPropertiesEnabled mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor inputContentPropertiesEnabledSelector

-- | A Boolean value that indicates whether the temporal scaler you create with this descriptor uses dynamic resolution.
--
-- When you set this property to <doc://com.apple.documentation/documentation/swift/true> to enable dynamic resolution, scale properties ``inputContentMinScale`` and ``inputContentMaxScale`` represent the input and output resolution both the width and height.
--
-- * Note: The scaler assumes that aspect ratio of the input and output textures doesn't change.
--
-- ObjC selector: @- setInputContentPropertiesEnabled:@
setInputContentPropertiesEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> Bool -> IO ()
setInputContentPropertiesEnabled mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setInputContentPropertiesEnabledSelector value

-- | The smallest scale factor the temporal scaler you create with this descriptor can use to generate output textures.
--
-- ObjC selector: @- inputContentMinScale@
inputContentMinScale :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CFloat
inputContentMinScale mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor inputContentMinScaleSelector

-- | The smallest scale factor the temporal scaler you create with this descriptor can use to generate output textures.
--
-- ObjC selector: @- setInputContentMinScale:@
setInputContentMinScale :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CFloat -> IO ()
setInputContentMinScale mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setInputContentMinScaleSelector value

-- | The largest scale factor the temporal scaler you create with this descriptor can use to generate output textures.
--
-- ObjC selector: @- inputContentMaxScale@
inputContentMaxScale :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CFloat
inputContentMaxScale mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor inputContentMaxScaleSelector

-- | The largest scale factor the temporal scaler you create with this descriptor can use to generate output textures.
--
-- ObjC selector: @- setInputContentMaxScale:@
setInputContentMaxScale :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CFloat -> IO ()
setInputContentMaxScale mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setInputContentMaxScaleSelector value

-- | A Boolean value that indicates whether a temporal scaler you create with the descriptor applies a reactive mask.
--
-- ObjC selector: @- reactiveMaskTextureEnabled@
reactiveMaskTextureEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO Bool
reactiveMaskTextureEnabled mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor reactiveMaskTextureEnabledSelector

-- | A Boolean value that indicates whether a temporal scaler you create with the descriptor applies a reactive mask.
--
-- ObjC selector: @- setReactiveMaskTextureEnabled:@
setReactiveMaskTextureEnabled :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> Bool -> IO ()
setReactiveMaskTextureEnabled mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setReactiveMaskTextureEnabledSelector value

-- | The pixel format of the reactive mask input texture for a temporal scaler you create with the descriptor.
--
-- ObjC selector: @- reactiveMaskTextureFormat@
reactiveMaskTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> IO CInt
reactiveMaskTextureFormat mtlfxTemporalScalerDescriptor =
  sendMessage mtlfxTemporalScalerDescriptor reactiveMaskTextureFormatSelector

-- | The pixel format of the reactive mask input texture for a temporal scaler you create with the descriptor.
--
-- ObjC selector: @- setReactiveMaskTextureFormat:@
setReactiveMaskTextureFormat :: IsMTLFXTemporalScalerDescriptor mtlfxTemporalScalerDescriptor => mtlfxTemporalScalerDescriptor -> CInt -> IO ()
setReactiveMaskTextureFormat mtlfxTemporalScalerDescriptor value =
  sendMessage mtlfxTemporalScalerDescriptor setReactiveMaskTextureFormatSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newTemporalScalerWithDevice:@
newTemporalScalerWithDeviceSelector :: Selector '[RawId] RawId
newTemporalScalerWithDeviceSelector = mkSelector "newTemporalScalerWithDevice:"

-- | @Selector@ for @newTemporalScalerWithDevice:compiler:@
newTemporalScalerWithDevice_compilerSelector :: Selector '[RawId, RawId] RawId
newTemporalScalerWithDevice_compilerSelector = mkSelector "newTemporalScalerWithDevice:compiler:"

-- | @Selector@ for @supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDeviceSelector :: Selector '[RawId] CFloat
supportedInputContentMinScaleForDeviceSelector = mkSelector "supportedInputContentMinScaleForDevice:"

-- | @Selector@ for @supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDeviceSelector :: Selector '[RawId] CFloat
supportedInputContentMaxScaleForDeviceSelector = mkSelector "supportedInputContentMaxScaleForDevice:"

-- | @Selector@ for @supportsDevice:@
supportsDeviceSelector :: Selector '[RawId] Bool
supportsDeviceSelector = mkSelector "supportsDevice:"

-- | @Selector@ for @supportsMetal4FX:@
supportsMetal4FXSelector :: Selector '[RawId] Bool
supportsMetal4FXSelector = mkSelector "supportsMetal4FX:"

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

-- | @Selector@ for @autoExposureEnabled@
autoExposureEnabledSelector :: Selector '[] Bool
autoExposureEnabledSelector = mkSelector "autoExposureEnabled"

-- | @Selector@ for @setAutoExposureEnabled:@
setAutoExposureEnabledSelector :: Selector '[Bool] ()
setAutoExposureEnabledSelector = mkSelector "setAutoExposureEnabled:"

-- | @Selector@ for @requiresSynchronousInitialization@
requiresSynchronousInitializationSelector :: Selector '[] Bool
requiresSynchronousInitializationSelector = mkSelector "requiresSynchronousInitialization"

-- | @Selector@ for @setRequiresSynchronousInitialization:@
setRequiresSynchronousInitializationSelector :: Selector '[Bool] ()
setRequiresSynchronousInitializationSelector = mkSelector "setRequiresSynchronousInitialization:"

-- | @Selector@ for @inputContentPropertiesEnabled@
inputContentPropertiesEnabledSelector :: Selector '[] Bool
inputContentPropertiesEnabledSelector = mkSelector "inputContentPropertiesEnabled"

-- | @Selector@ for @setInputContentPropertiesEnabled:@
setInputContentPropertiesEnabledSelector :: Selector '[Bool] ()
setInputContentPropertiesEnabledSelector = mkSelector "setInputContentPropertiesEnabled:"

-- | @Selector@ for @inputContentMinScale@
inputContentMinScaleSelector :: Selector '[] CFloat
inputContentMinScaleSelector = mkSelector "inputContentMinScale"

-- | @Selector@ for @setInputContentMinScale:@
setInputContentMinScaleSelector :: Selector '[CFloat] ()
setInputContentMinScaleSelector = mkSelector "setInputContentMinScale:"

-- | @Selector@ for @inputContentMaxScale@
inputContentMaxScaleSelector :: Selector '[] CFloat
inputContentMaxScaleSelector = mkSelector "inputContentMaxScale"

-- | @Selector@ for @setInputContentMaxScale:@
setInputContentMaxScaleSelector :: Selector '[CFloat] ()
setInputContentMaxScaleSelector = mkSelector "setInputContentMaxScale:"

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

