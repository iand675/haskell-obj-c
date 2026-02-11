{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBinaryFullyConnected
--
-- This depends on Metal.framework
--
-- The MPSCNNBinaryFullyConnected specifies a fully connected convolution layer with binary weights              and optionally binarized input image.              See MPSCNNFullyConnected for details on the fully connected layer and              MPSCNNBinaryConvolution for binary convolutions.
--
-- The default padding policy for MPSCNNBinaryConvolution is different from most               filters. It uses MPSNNPaddingMethodSizeValidOnly instead of MPSNNPaddingMethodSizeSame.
--
-- Generated bindings for @MPSCNNBinaryFullyConnected@.
module ObjC.MetalPerformanceShaders.MPSCNNBinaryFullyConnected
  ( MPSCNNBinaryFullyConnected
  , IsMPSCNNBinaryFullyConnected(..)
  , initWithDevice_convolutionData_scaleValue_type_flags
  , initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , initWithCoder_device
  , initWithDevice
  , initWithDevice_convolutionData_scaleValue_type_flagsSelector
  , initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector

  -- * Enum types
  , MPSCNNBinaryConvolutionFlags(MPSCNNBinaryConvolutionFlags)
  , pattern MPSCNNBinaryConvolutionFlagsNone
  , pattern MPSCNNBinaryConvolutionFlagsUseBetaScaling
  , MPSCNNBinaryConvolutionType(MPSCNNBinaryConvolutionType)
  , pattern MPSCNNBinaryConvolutionTypeBinaryWeights
  , pattern MPSCNNBinaryConvolutionTypeXNOR
  , pattern MPSCNNBinaryConvolutionTypeAND

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a binary fully connected kernel with binary weights and a single scaling term.
--
-- @device@ — The MTLDevice on which this MPSCNNBinaryFullyConnected filter will be used
--
-- @convolutionData@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource protocol.                                              The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNBinaryFullyConnected uses to obtain the weights and bias terms as                                              well as the convolution descriptor.                                              Each entry in the convolutionData:weights array is a 32-bit unsigned integer value                                              and each bit represents one filter weight (given in machine byte order).                                              The featurechannel indices increase from the least significant bit within the 32-bits.                                              The number of entries is =                                              ceil( inputFeatureChannels/32.0 ) * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as a 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ ceil( inputChannels / 32.0 ) ]                                              (The ordering of the reduction from 4D tensor to 1D is per C convention. The index based on                                              inputchannels varies most rapidly, followed by kernelWidth, then kernelHeight and finally                                              outputChannels varies least rapidly.)
--
-- @scaleValue@ — A single floating point value used to scale the entire convolution.                                              Each entry is a float value. The number of entries is 'inputFeatureChannels'. If nil then 1.0 is used.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation above and documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A valid MPSCNNBinaryFullyConnected object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionData:scaleValue:type:flags:@
initWithDevice_convolutionData_scaleValue_type_flags :: IsMPSCNNBinaryFullyConnected mpscnnBinaryFullyConnected => mpscnnBinaryFullyConnected -> RawId -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnected)
initWithDevice_convolutionData_scaleValue_type_flags mpscnnBinaryFullyConnected  device convolutionData scaleValue type_ flags =
  sendMsg mpscnnBinaryFullyConnected (mkSelector "initWithDevice:convolutionData:scaleValue:type:flags:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId convolutionData) :: Ptr ()), argCFloat (fromIntegral scaleValue), argCULong (coerce type_), argCULong (coerce flags)] >>= ownedObject . castPtr

-- | Initializes a binary fully connected kernel with binary weights as well as both pre and post scaling terms.
--
-- @device@ — The MTLDevice on which this MPSCNNBinaryFullyConnected filter will be used
--
-- @convolutionData@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource protocol.                                              The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNBinaryFullyConnected uses to obtain the weights and the convolution descriptor.                                              Each entry in the convolutionData:weights array is a 32-bit unsigned integer value                                              and each bit represents one filter weight (given in machine byte order).                                              The featurechannel indices increase from the least significant bit within the 32-bits.                                              The number of entries is =                                              ceil( inputFeatureChannels/32.0 ) * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as a 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ ceil( inputChannels / 32.0 ) ]                                              (The ordering of the reduction from 4D tensor to 1D is per C convention. The index based on                                              inputchannels varies most rapidly, followed by kernelWidth, then kernelHeight and finally                                              outputChannels varies least rapidly.)
--
-- @outputBiasTerms@ — A pointer to bias terms to be applied to the convolution output.  Each entry is a float value.                                              The number of entries is = numberOfOutputFeatureMaps. If nil then 0.0 is used for bias.                                              The values stored in the pointer are copied in and the array can be freed after this function returns.
--
-- @outputScaleTerms@ — A pointer to scale terms to be applied to binary convolution results per output feature channel.                                              Each entry is a float value. The number of entries is = numberOfOutputFeatureMaps. If nil then 1.0 is used.                                              The values stored in the pointer are copied in and the array can be freed after this function returns.
--
-- @inputBiasTerms@ — A pointer to offset terms to be applied to the input before convolution and before input scaling.                                              Each entry is a float value. The number of entries is 'inputFeatureChannels'. If NULL then 0.0 is used for bias.                                              The values stored in the pointer are copied in and the array can be freed after this function returns.
--
-- @inputScaleTerms@ — A pointer to scale terms to be applied to the input before convolution, but after input biasing.                                              Each entry is a float value. The number of entries is 'inputFeatureChannels'. If nil then 1.0 is used.                                              The values stored in the pointer are copied in and the array can be freed after this function returns.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation above and documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A valid MPSCNNBinaryFullyConnected object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: IsMPSCNNBinaryFullyConnected mpscnnBinaryFullyConnected => mpscnnBinaryFullyConnected -> RawId -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnected)
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags mpscnnBinaryFullyConnected  device convolutionData outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  sendMsg mpscnnBinaryFullyConnected (mkSelector "initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId convolutionData) :: Ptr ()), argPtr (unConst outputBiasTerms), argPtr (unConst outputScaleTerms), argPtr (unConst inputBiasTerms), argPtr (unConst inputScaleTerms), argCULong (coerce type_), argCULong (coerce flags)] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNBinaryFullyConnected mpscnnBinaryFullyConnected, IsNSCoder aDecoder) => mpscnnBinaryFullyConnected -> aDecoder -> RawId -> IO (Id MPSCNNBinaryFullyConnected)
initWithCoder_device mpscnnBinaryFullyConnected  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnBinaryFullyConnected (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNBinaryFullyConnected mpscnnBinaryFullyConnected => mpscnnBinaryFullyConnected -> RawId -> IO (Id MPSCNNBinaryFullyConnected)
initWithDevice mpscnnBinaryFullyConnected  device =
  sendMsg mpscnnBinaryFullyConnected (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:convolutionData:scaleValue:type:flags:@
initWithDevice_convolutionData_scaleValue_type_flagsSelector :: Selector
initWithDevice_convolutionData_scaleValue_type_flagsSelector = mkSelector "initWithDevice:convolutionData:scaleValue:type:flags:"

-- | @Selector@ for @initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector :: Selector
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector = mkSelector "initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

