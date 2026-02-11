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
  , newFrameInterpolatorWithDeviceSelector
  , newFrameInterpolatorWithDevice_compilerSelector
  , supportsMetal4FXSelector
  , supportsDeviceSelector
  , colorTextureFormatSelector
  , setColorTextureFormatSelector
  , outputTextureFormatSelector
  , setOutputTextureFormatSelector
  , depthTextureFormatSelector
  , setDepthTextureFormatSelector
  , motionTextureFormatSelector
  , setMotionTextureFormatSelector
  , uiTextureFormatSelector
  , setUiTextureFormatSelector
  , scalerSelector
  , setScalerSelector
  , inputWidthSelector
  , setInputWidthSelector
  , inputHeightSelector
  , setInputHeightSelector
  , outputWidthSelector
  , setOutputWidthSelector
  , outputHeightSelector
  , setOutputHeightSelector


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

import ObjC.MetalFX.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a frame interpolator instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the frame interpolator. - Returns:    A new frame interpolator instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newFrameInterpolatorWithDevice:@
newFrameInterpolatorWithDevice :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> IO RawId
newFrameInterpolatorWithDevice mtlfxFrameInterpolatorDescriptor  device =
    fmap (RawId . castPtr) $ sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "newFrameInterpolatorWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Creates a frame interpolator instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the frame interpolator.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A new frame interpolator instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newFrameInterpolatorWithDevice:compiler:@
newFrameInterpolatorWithDevice_compiler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> RawId -> IO RawId
newFrameInterpolatorWithDevice_compiler mtlfxFrameInterpolatorDescriptor  device compiler =
    fmap (RawId . castPtr) $ sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "newFrameInterpolatorWithDevice:compiler:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId compiler) :: Ptr ())]

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
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsMetal4FX:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

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
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDevice:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | The pixel format of the input color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
colorTextureFormat mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "colorTextureFormat") retCInt []

-- | The pixel format of the input color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setColorTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the output color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
outputTextureFormat mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "outputTextureFormat") retCInt []

-- | The pixel format of the output color texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setOutputTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input depth texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- depthTextureFormat@
depthTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
depthTextureFormat mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "depthTextureFormat") retCInt []

-- | The pixel format of the input depth texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setDepthTextureFormat:@
setDepthTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setDepthTextureFormat mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setDepthTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input motion texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- motionTextureFormat@
motionTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
motionTextureFormat mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "motionTextureFormat") retCInt []

-- | The pixel format of the input motion texture for the frame interpolator you create with this descriptor.
--
-- ObjC selector: @- setMotionTextureFormat:@
setMotionTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setMotionTextureFormat mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setMotionTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format for the frame interpolator of an input texture containing your game's custom UI.
--
-- ObjC selector: @- uiTextureFormat@
uiTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CInt
uiTextureFormat mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "uiTextureFormat") retCInt []

-- | The pixel format for the frame interpolator of an input texture containing your game's custom UI.
--
-- ObjC selector: @- setUiTextureFormat:@
setUiTextureFormat :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CInt -> IO ()
setUiTextureFormat mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setUiTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | @- scaler@
scaler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO RawId
scaler mtlfxFrameInterpolatorDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "scaler") (retPtr retVoid) []

-- | @- setScaler:@
setScaler :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> RawId -> IO ()
setScaler mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setScaler:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The width, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
inputWidth mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "inputWidth") retCULong []

-- | The width, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setInputWidth mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setInputWidth:") retVoid [argCULong value]

-- | The height, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
inputHeight mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "inputHeight") retCULong []

-- | The height, in pixels, of the input motion and depth texture for the frame interpolator.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setInputHeight mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setInputHeight:") retVoid [argCULong value]

-- | The width, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
outputWidth mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "outputWidth") retCULong []

-- | The width, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setOutputWidth mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setOutputWidth:") retVoid [argCULong value]

-- | The height, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> IO CULong
outputHeight mtlfxFrameInterpolatorDescriptor  =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "outputHeight") retCULong []

-- | The height, in pixels, of the output color texture for the frame interpolator.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXFrameInterpolatorDescriptor mtlfxFrameInterpolatorDescriptor => mtlfxFrameInterpolatorDescriptor -> CULong -> IO ()
setOutputHeight mtlfxFrameInterpolatorDescriptor  value =
    sendMsg mtlfxFrameInterpolatorDescriptor (mkSelector "setOutputHeight:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newFrameInterpolatorWithDevice:@
newFrameInterpolatorWithDeviceSelector :: Selector
newFrameInterpolatorWithDeviceSelector = mkSelector "newFrameInterpolatorWithDevice:"

-- | @Selector@ for @newFrameInterpolatorWithDevice:compiler:@
newFrameInterpolatorWithDevice_compilerSelector :: Selector
newFrameInterpolatorWithDevice_compilerSelector = mkSelector "newFrameInterpolatorWithDevice:compiler:"

-- | @Selector@ for @supportsMetal4FX:@
supportsMetal4FXSelector :: Selector
supportsMetal4FXSelector = mkSelector "supportsMetal4FX:"

-- | @Selector@ for @supportsDevice:@
supportsDeviceSelector :: Selector
supportsDeviceSelector = mkSelector "supportsDevice:"

-- | @Selector@ for @colorTextureFormat@
colorTextureFormatSelector :: Selector
colorTextureFormatSelector = mkSelector "colorTextureFormat"

-- | @Selector@ for @setColorTextureFormat:@
setColorTextureFormatSelector :: Selector
setColorTextureFormatSelector = mkSelector "setColorTextureFormat:"

-- | @Selector@ for @outputTextureFormat@
outputTextureFormatSelector :: Selector
outputTextureFormatSelector = mkSelector "outputTextureFormat"

-- | @Selector@ for @setOutputTextureFormat:@
setOutputTextureFormatSelector :: Selector
setOutputTextureFormatSelector = mkSelector "setOutputTextureFormat:"

-- | @Selector@ for @depthTextureFormat@
depthTextureFormatSelector :: Selector
depthTextureFormatSelector = mkSelector "depthTextureFormat"

-- | @Selector@ for @setDepthTextureFormat:@
setDepthTextureFormatSelector :: Selector
setDepthTextureFormatSelector = mkSelector "setDepthTextureFormat:"

-- | @Selector@ for @motionTextureFormat@
motionTextureFormatSelector :: Selector
motionTextureFormatSelector = mkSelector "motionTextureFormat"

-- | @Selector@ for @setMotionTextureFormat:@
setMotionTextureFormatSelector :: Selector
setMotionTextureFormatSelector = mkSelector "setMotionTextureFormat:"

-- | @Selector@ for @uiTextureFormat@
uiTextureFormatSelector :: Selector
uiTextureFormatSelector = mkSelector "uiTextureFormat"

-- | @Selector@ for @setUiTextureFormat:@
setUiTextureFormatSelector :: Selector
setUiTextureFormatSelector = mkSelector "setUiTextureFormat:"

-- | @Selector@ for @scaler@
scalerSelector :: Selector
scalerSelector = mkSelector "scaler"

-- | @Selector@ for @setScaler:@
setScalerSelector :: Selector
setScalerSelector = mkSelector "setScaler:"

-- | @Selector@ for @inputWidth@
inputWidthSelector :: Selector
inputWidthSelector = mkSelector "inputWidth"

-- | @Selector@ for @setInputWidth:@
setInputWidthSelector :: Selector
setInputWidthSelector = mkSelector "setInputWidth:"

-- | @Selector@ for @inputHeight@
inputHeightSelector :: Selector
inputHeightSelector = mkSelector "inputHeight"

-- | @Selector@ for @setInputHeight:@
setInputHeightSelector :: Selector
setInputHeightSelector = mkSelector "setInputHeight:"

-- | @Selector@ for @outputWidth@
outputWidthSelector :: Selector
outputWidthSelector = mkSelector "outputWidth"

-- | @Selector@ for @setOutputWidth:@
setOutputWidthSelector :: Selector
setOutputWidthSelector = mkSelector "setOutputWidth:"

-- | @Selector@ for @outputHeight@
outputHeightSelector :: Selector
outputHeightSelector = mkSelector "outputHeight"

-- | @Selector@ for @setOutputHeight:@
setOutputHeightSelector :: Selector
setOutputHeightSelector = mkSelector "setOutputHeight:"

