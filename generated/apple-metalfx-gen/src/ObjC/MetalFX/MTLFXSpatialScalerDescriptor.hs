{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A set of properties that configure a spatial scaling effect, and a factory method that creates the effect.
--
-- Generated bindings for @MTLFXSpatialScalerDescriptor@.
module ObjC.MetalFX.MTLFXSpatialScalerDescriptor
  ( MTLFXSpatialScalerDescriptor
  , IsMTLFXSpatialScalerDescriptor(..)
  , newSpatialScalerWithDevice
  , newSpatialScalerWithDevice_compiler
  , supportsMetal4FX
  , supportsDevice
  , colorTextureFormat
  , setColorTextureFormat
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
  , colorProcessingMode
  , setColorProcessingMode
  , colorProcessingModeSelector
  , colorTextureFormatSelector
  , inputHeightSelector
  , inputWidthSelector
  , newSpatialScalerWithDeviceSelector
  , newSpatialScalerWithDevice_compilerSelector
  , outputHeightSelector
  , outputTextureFormatSelector
  , outputWidthSelector
  , setColorProcessingModeSelector
  , setColorTextureFormatSelector
  , setInputHeightSelector
  , setInputWidthSelector
  , setOutputHeightSelector
  , setOutputTextureFormatSelector
  , setOutputWidthSelector
  , supportsDeviceSelector
  , supportsMetal4FXSelector

  -- * Enum types
  , MTLFXSpatialScalerColorProcessingMode(MTLFXSpatialScalerColorProcessingMode)
  , pattern MTLFXSpatialScalerColorProcessingModePerceptual
  , pattern MTLFXSpatialScalerColorProcessingModeLinear
  , pattern MTLFXSpatialScalerColorProcessingModeHDR

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalFX.Internal.Classes
import ObjC.MetalFX.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a spatial scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the spatial scaler. - Returns:    A new spatial scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newSpatialScalerWithDevice:@
newSpatialScalerWithDevice :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> RawId -> IO RawId
newSpatialScalerWithDevice mtlfxSpatialScalerDescriptor device =
  sendOwnedMessage mtlfxSpatialScalerDescriptor newSpatialScalerWithDeviceSelector device

-- | Creates a spatial scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the spatial scaler.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A new spatial scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newSpatialScalerWithDevice:compiler:@
newSpatialScalerWithDevice_compiler :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> RawId -> RawId -> IO RawId
newSpatialScalerWithDevice_compiler mtlfxSpatialScalerDescriptor device compiler =
  sendOwnedMessage mtlfxSpatialScalerDescriptor newSpatialScalerWithDevice_compilerSelector device compiler

-- | Queries whether a Metal device supports spatial scaling compatible with Metal 4.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports spatial scaling with             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXSpatialScalerDescriptor"
    sendClassMessage cls' supportsMetal4FXSelector device

-- | Returns a Boolean value that indicates whether the spatial scaler works with a GPU.
--
-- - Parameters:    - device: An ``MTLDevice`` instance that represents a GPU.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports spatial scaling,            <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXSpatialScalerDescriptor"
    sendClassMessage cls' supportsDeviceSelector device

-- | The pixel format of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CInt
colorTextureFormat mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor colorTextureFormatSelector

-- | The pixel format of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setColorTextureFormatSelector value

-- | The pixel format of the output texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CInt
outputTextureFormat mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor outputTextureFormatSelector

-- | The pixel format of the output texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setOutputTextureFormatSelector value

-- | The width of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
inputWidth mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor inputWidthSelector

-- | The width of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setInputWidth mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setInputWidthSelector value

-- | The height of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
inputHeight mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor inputHeightSelector

-- | The height of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setInputHeight mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setInputHeightSelector value

-- | The width of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
outputWidth mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor outputWidthSelector

-- | The width of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setOutputWidth mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setOutputWidthSelector value

-- | The height of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
outputHeight mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor outputHeightSelector

-- | The height of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setOutputHeight mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setOutputHeightSelector value

-- | The color space of the input color texture for the spatial scaler you create with this descriptor.
--
-- This property's default value is ``MTLFXSpatialScalerColorProcessingMode/MTLFXSpatialScalerColorProcessingModePerceptual``.
--
-- ObjC selector: @- colorProcessingMode@
colorProcessingMode :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO MTLFXSpatialScalerColorProcessingMode
colorProcessingMode mtlfxSpatialScalerDescriptor =
  sendMessage mtlfxSpatialScalerDescriptor colorProcessingModeSelector

-- | The color space of the input color texture for the spatial scaler you create with this descriptor.
--
-- This property's default value is ``MTLFXSpatialScalerColorProcessingMode/MTLFXSpatialScalerColorProcessingModePerceptual``.
--
-- ObjC selector: @- setColorProcessingMode:@
setColorProcessingMode :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> MTLFXSpatialScalerColorProcessingMode -> IO ()
setColorProcessingMode mtlfxSpatialScalerDescriptor value =
  sendMessage mtlfxSpatialScalerDescriptor setColorProcessingModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newSpatialScalerWithDevice:@
newSpatialScalerWithDeviceSelector :: Selector '[RawId] RawId
newSpatialScalerWithDeviceSelector = mkSelector "newSpatialScalerWithDevice:"

-- | @Selector@ for @newSpatialScalerWithDevice:compiler:@
newSpatialScalerWithDevice_compilerSelector :: Selector '[RawId, RawId] RawId
newSpatialScalerWithDevice_compilerSelector = mkSelector "newSpatialScalerWithDevice:compiler:"

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

-- | @Selector@ for @colorProcessingMode@
colorProcessingModeSelector :: Selector '[] MTLFXSpatialScalerColorProcessingMode
colorProcessingModeSelector = mkSelector "colorProcessingMode"

-- | @Selector@ for @setColorProcessingMode:@
setColorProcessingModeSelector :: Selector '[MTLFXSpatialScalerColorProcessingMode] ()
setColorProcessingModeSelector = mkSelector "setColorProcessingMode:"

