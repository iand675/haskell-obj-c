{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A set of properties that configure a frame interpolator, and a factory method that creates the effect.
--
-- A frame interpolator inspects two frames your game or app renders and, based on their properties, generates an extra frame at a fraction of the cost, helping you to increase your frame rate.
--
-- When you configure this descriptor, set the properties that determine the pixel format for each texture to the respective format of the texture you later assign to the scaler. For example, make sure that the format to which you set the ``colorTextureFormat`` property matches the format of the texture you later assign to the interpolator's ``MTLFXFrameInterpolatorDescriptor/colorTexture`` property.
--
-- Generated bindings for @MTLFXFrameInterpolatorDescriptor@.
module ObjC.MetalFX.MTLFXFrameInterpolatorDescriptor
  ( MTLFXFrameInterpolatorDescriptor
  , IsMTLFXFrameInterpolatorDescriptor(..)
  , newFrameInterpolatorWithDevice
  , newFrameInterpolatorWithDevice_compiler
  , supportsMetal4FX
  , supportsDevice
  , colorTextureFormat
  , setColorTextureFormat
  , outputTextureFormat
  , setOutputTextureFormat
  , depthTextureFormat
  , setDepthTextureFormat
  , motionTextureFormat
  , setMotionTextureFormat
  , uiTextureFormat
  , setUiTextureFormat
  , scaler
  , setScaler
  , inputWidth
  , setInputWidth
  , inputHeight
  , setInputHeight
  , outputWidth
  , setOutputWidth
  , outputHeight
  , setOutputHeight
  , colorTextureFormatSelector
  , depthTextureFormatSelector
  , inputHeightSelector
  , inputWidthSelector
  , motionTextureFormatSelector
  , newFrameInterpolatorWithDeviceSelector
  , newFrameInterpolatorWithDevice_compilerSelector
  , outputHeightSelector
  , outputTextureFormatSelector
  , outputWidthSelector
  , scalerSelector
  , setColorTextureFormatSelector
  , setDepthTextureFormatSelector
  , setInputHeightSelector
  , setInputWidthSelector
  , setMotionTextureFormatSelector
  , setOutputHeightSelector
  , setOutputTextureFormatSelector
  , setOutputWidthSelector
  , setScalerSelector
  , setUiTextureFormatSelector
  , supportsDeviceSelector
  , supportsMetal4FXSelector
  , uiTextureFormatSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalFX.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a frame interpolator instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the frame interpolator. - Returns:    A new frame interpolator instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newFrameInterpolatorWithDevice:@
newFrameInterpolatorWithDevice :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> IO RawId
newFrameInterpolatorWithDevice mtlfxFrameInterpolatorDescriptor device =
  sendOwnedMessage mtlfxFrameInterpolatorDescriptor newFrameInterpolatorWithDeviceSelector device

-- | Creates a frame interpolator instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the frame interpolator.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A new frame interpolator instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newFrameInterpolatorWithDevice:compiler:@
newFrameInterpolatorWithDevice_compiler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> RawId -> IO RawId
newFrameInterpolatorWithDevice_compiler mtlfxFrameInterpolatorDescriptor device compiler =
  sendOwnedMessage mtlfxFrameInterpolatorDescriptor newFrameInterpolatorWithDevice_compilerSelector device compiler

-- | Queries whether a Metal device supports frame interpolation compatible with a Metal 4 command buffer.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports frame interpolation for             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXFrameInterpolatorDescriptor"
    sendClassMessage cls' supportsMetal4FXSelector device

-- | Queries whether a Metal device supports frame interpolation.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports frame interpolation,             <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXFrameInterpolatorDescriptor"
    sendClassMessage cls' supportsDeviceSelector device

-- | The pixel format of the input color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
colorTextureFormat mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor colorTextureFormatSelector

-- | The pixel format of the input color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setColorTextureFormatSelector value

-- | The pixel format of the output color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
outputTextureFormat mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor outputTextureFormatSelector

-- | The pixel format of the output color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setOutputTextureFormatSelector value

-- | The pixel format of the input depth texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- depthTextureFormat@
depthTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
depthTextureFormat mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor depthTextureFormatSelector

-- | The pixel format of the input depth texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setDepthTextureFormat:@
setDepthTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setDepthTextureFormat mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setDepthTextureFormatSelector value

-- | The pixel format of the input motion texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- motionTextureFormat@
motionTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
motionTextureFormat mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor motionTextureFormatSelector

-- | The pixel format of the input motion texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setMotionTextureFormat:@
setMotionTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setMotionTextureFormat mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setMotionTextureFormatSelector value

-- | The pixel format for the frame interpolator of an input texture containing your game's custom UI.
--
-- ObjC selector: @- uiTextureFormat@
uiTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
uiTextureFormat mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor uiTextureFormatSelector

-- | The pixel format for the frame interpolator of an input texture containing your game's custom UI.
--
-- ObjC selector: @- setUiTextureFormat:@
setUiTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setUiTextureFormat mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setUiTextureFormatSelector value

-- | @- scaler@
scaler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO RawId
scaler mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor scalerSelector

-- | @- setScaler:@
setScaler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> IO ()
setScaler mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setScalerSelector value

-- | The width, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
inputWidth mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor inputWidthSelector

-- | The width, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setInputWidth mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setInputWidthSelector value

-- | The height, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
inputHeight mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor inputHeightSelector

-- | The height, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setInputHeight mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setInputHeightSelector value

-- | The width, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
outputWidth mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor outputWidthSelector

-- | The width, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setOutputWidth mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setOutputWidthSelector value

-- | The height, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
outputHeight mtlfxFrameInterpolatorDescriptor =
  sendMessage mtlfxFrameInterpolatorDescriptor outputHeightSelector

-- | The height, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setOutputHeight mtlfxFrameInterpolatorDescriptor value =
  sendMessage mtlfxFrameInterpolatorDescriptor setOutputHeightSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newFrameInterpolatorWithDevice:@
newFrameInterpolatorWithDeviceSelector :: Selector '[RawId] RawId
newFrameInterpolatorWithDeviceSelector = mkSelector "newFrameInterpolatorWithDevice:"

-- | @Selector@ for @newFrameInterpolatorWithDevice:compiler:@
newFrameInterpolatorWithDevice_compilerSelector :: Selector '[RawId, RawId] RawId
newFrameInterpolatorWithDevice_compilerSelector = mkSelector "newFrameInterpolatorWithDevice:compiler:"

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

-- | @Selector@ for @outputTextureFormat@
outputTextureFormatSelector :: Selector '[] CInt
outputTextureFormatSelector = mkSelector "outputTextureFormat"

-- | @Selector@ for @setOutputTextureFormat:@
setOutputTextureFormatSelector :: Selector '[CInt] ()
setOutputTextureFormatSelector = mkSelector "setOutputTextureFormat:"

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

-- | @Selector@ for @uiTextureFormat@
uiTextureFormatSelector :: Selector '[] CInt
uiTextureFormatSelector = mkSelector "uiTextureFormat"

-- | @Selector@ for @setUiTextureFormat:@
setUiTextureFormatSelector :: Selector '[CInt] ()
setUiTextureFormatSelector = mkSelector "setUiTextureFormat:"

-- | @Selector@ for @scaler@
scalerSelector :: Selector '[] RawId
scalerSelector = mkSelector "scaler"

-- | @Selector@ for @setScaler:@
setScalerSelector :: Selector '[RawId] ()
setScalerSelector = mkSelector "setScaler:"

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

