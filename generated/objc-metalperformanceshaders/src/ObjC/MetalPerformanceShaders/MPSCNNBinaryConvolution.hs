{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBinaryConvolution
--
-- This depends on Metal.framework
--
-- The MPSCNNBinaryConvolution specifies a convolution with binary weights and an input image using binary approximations.              The MPSCNNBinaryConvolution optionally first binarizes the input image and then convolves the result with a set of              binary-valued filters, each producing one feature map in the output image (which is a normal image)
--
-- The output is computed as follows:
--
-- out[i, x, y, c] = ( sum_{dx,dy,f} in[i,x+dx, y+dy, f] x B[c,dx,dy,f] )                                      * scale[c] * beta[i,x,y] + bias[c], where
--
-- the sum over dx,dy is over the spatial filter kernel window defined by 'kernelWidth' and 'KernelHeight',              sum over 'f' is over the input feature channel indices within group, 'B' contains the binary weights, interpreted as              {-1,1} or { 0, 1 } and scale[c] is the 'outputScaleTerms' array and bias is the 'outputBiasTerms' array. Above 'i' is              the image index in batch the sum over input channels 'f' runs through the group indices.
--
-- The convolution operator 'x' is defined by MPSCNNBinaryConvolutionType passed in at initialization time of the filter              (
--
-- See: initWithDevice).              In case 'type' = MPSCNNBinaryConvolutionTypeBinaryWeights, the input image is not binarized at all                  and the convolution is computed interpreting the weights as [ 0, 1 ] -> { -1, 1 } with the given scaling terms.              In case 'type' = MPSCNNBinaryConvolutionTypeXNOR the convolution is computed by first binarizing the input image                  using the sign function 'bin(x) = x < 0 ? -1 : 1' and the convolution multiplication is done with the                  XNOR-operator !(x ^ y) = delta_xy = { (x==y) ? 1 : 0 },                  and scaled according to the optional scaling operations. Note that we output the values of the bitwise convolutions                  to interval { -1, 1 }, which means that the output of the XNOR-operator is scaled implicitly as follows:                      r = 2 * ( !(x ^ y) ) - 1 = { -1, 1 }.                  This means that for a dot-product of two 32-bit words the result is:                      r = 2 * popcount(!(x ^ y) ) - 32 = 32 - 2 * popcount( x ^ y ) = { -32, -30, ..., 30, 32 }.              In case 'type' = MPSCNNBinaryConvolutionTypeAND the convolution is computed by first binarizing the input image                  using the sign function 'bin(x) = x < 0 ? -1 : 1' and the convolution multiplication is done with the                  AND-operator (x & y) = delta_xy * delta_x1 = { (x==y==1) ? 1 : 0 }.                  and scaled according to the optional scaling operations. Note that we output the values of the AND-operation is                  assumed to lie in { 0, 1 } interval and hence no more implicit scaling takes place.                  This means that for a dot-product of two 32-bit words the result is:                      r = popcount(x & y) = { 0, ..., 31, 32 }.
--
-- The input data can be pre-offset and scaled by providing the 'inputBiasTerms' and 'inputScaleTerms' parameters for the              initialization functions and this can be used for example to accomplish batch normalization of the data. The scaling of              input values happens before possible beta-image computation.
--
-- The parameter 'beta' above is an optional image which is used to compute scaling factors for each spatial position and image index.              For the XNOR-Net based networks this is computed as follows: beta[i,x,y] = sum_{dx,dy} A[i, x+dx, y+dy] / (kx * ky), where              (dx,dy) are summed over the convolution filter window [ -kx/2, (kx-1)/2], [ -ky/2, (ky-1)/2 ] and              A[i,x,y] = sum_{c} abs( in[i,x,y,c] ) / Nc, where 'in' is the original input image (in full precision) and Nc is the              number of input channels in the input image. Parameter 'beta' is not passed as input and to enable beta-scaling the user can              provide 'MPSCNNBinaryConvolutionFlagsUseBetaScaling' in the flags parameter in the initialization functions.
--
-- Finally the normal activation neuron is applied and the result is written to the output image.
--
-- NOTE: MPSCNNBinaryConvolution does not currently support groups > 1.
--
-- Generated bindings for @MPSCNNBinaryConvolution@.
module ObjC.MetalPerformanceShaders.MPSCNNBinaryConvolution
  ( MPSCNNBinaryConvolution
  , IsMPSCNNBinaryConvolution(..)
  , initWithDevice_convolutionData_scaleValue_type_flags
  , initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , initWithCoder_device
  , initWithDevice
  , inputFeatureChannels
  , outputFeatureChannels
  , initWithDevice_convolutionData_scaleValue_type_flagsSelector
  , initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , inputFeatureChannelsSelector
  , outputFeatureChannelsSelector

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

-- | Initializes a binary convolution kernel with binary weights and a single scaling term.
--
-- @device@ — The MTLDevice on which this MPSCNNBinaryConvolution filter will be used
--
-- @convolutionData@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource protocol.                                              The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNBinaryConvolution uses to obtain the weights and bias terms as                                              well as the convolution descriptor.                                              Each entry in the convolutionData:weights array is a 32-bit unsigned integer value                                              and each bit represents one filter weight (given in machine byte order).                                              The featurechannel indices increase from the least significant bit within the 32-bits.                                              The number of entries is =                                              ceil( inputFeatureChannels/32.0 ) * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as a 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ ceil( inputChannels / 32.0 ) ]                                              (The ordering of the reduction from 4D tensor to 1D is per C convention. The index based on                                              inputchannels varies most rapidly, followed by kernelWidth, then kernelHeight and finally                                              outputChannels varies least rapidly.)
--
-- @scaleValue@ — A floating point value used to scale the entire convolution.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation above and documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A valid MPSCNNBinaryConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionData:scaleValue:type:flags:@
initWithDevice_convolutionData_scaleValue_type_flags :: IsMPSCNNBinaryConvolution mpscnnBinaryConvolution => mpscnnBinaryConvolution -> RawId -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolution)
initWithDevice_convolutionData_scaleValue_type_flags mpscnnBinaryConvolution  device convolutionData scaleValue type_ flags =
  sendMsg mpscnnBinaryConvolution (mkSelector "initWithDevice:convolutionData:scaleValue:type:flags:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId convolutionData) :: Ptr ()), argCFloat (fromIntegral scaleValue), argCULong (coerce type_), argCULong (coerce flags)] >>= ownedObject . castPtr

-- | Initializes a binary convolution kernel with binary weights as well as both pre and post scaling terms.
--
-- @device@ — The MTLDevice on which this MPSCNNBinaryConvolution filter will be used
--
-- @convolutionData@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource protocol.                                              The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNBinaryConvolution uses to obtain the weights and the convolution descriptor.                                              Each entry in the convolutionData:weights array is a 32-bit unsigned integer value                                              and each bit represents one filter weight (given in machine byte order).                                              The featurechannel indices increase from the least significant bit within the 32-bits.                                              The number of entries is =                                              ceil( inputFeatureChannels/32.0 ) * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as a 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ ceil( inputChannels / 32.0 ) ]                                              (The ordering of the reduction from 4D tensor to 1D is per C convention. The index based on                                              inputchannels varies most rapidly, followed by kernelWidth, then kernelHeight and finally                                              outputChannels varies least rapidly.)
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
-- Returns: A valid MPSCNNBinaryConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: IsMPSCNNBinaryConvolution mpscnnBinaryConvolution => mpscnnBinaryConvolution -> RawId -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolution)
initWithDevice_convolutionData_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags mpscnnBinaryConvolution  device convolutionData outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  sendMsg mpscnnBinaryConvolution (mkSelector "initWithDevice:convolutionData:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId convolutionData) :: Ptr ()), argPtr (unConst outputBiasTerms), argPtr (unConst outputScaleTerms), argPtr (unConst inputBiasTerms), argPtr (unConst inputScaleTerms), argCULong (coerce type_), argCULong (coerce flags)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNBinaryConvolution mpscnnBinaryConvolution, IsNSCoder aDecoder) => mpscnnBinaryConvolution -> aDecoder -> RawId -> IO (Id MPSCNNBinaryConvolution)
initWithCoder_device mpscnnBinaryConvolution  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnBinaryConvolution (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNBinaryConvolution mpscnnBinaryConvolution => mpscnnBinaryConvolution -> RawId -> IO (Id MPSCNNBinaryConvolution)
initWithDevice mpscnnBinaryConvolution  device =
  sendMsg mpscnnBinaryConvolution (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNBinaryConvolution mpscnnBinaryConvolution => mpscnnBinaryConvolution -> IO CULong
inputFeatureChannels mpscnnBinaryConvolution  =
  sendMsg mpscnnBinaryConvolution (mkSelector "inputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNBinaryConvolution mpscnnBinaryConvolution => mpscnnBinaryConvolution -> IO CULong
outputFeatureChannels mpscnnBinaryConvolution  =
  sendMsg mpscnnBinaryConvolution (mkSelector "outputFeatureChannels") retCULong []

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

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

