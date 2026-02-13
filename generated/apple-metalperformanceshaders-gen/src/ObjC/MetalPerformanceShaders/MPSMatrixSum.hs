{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixSum
--
-- This depends on Metal.framework
--
-- MPSMatrixSum performs a pointwise summation of N MPSMatrix              objects and applies an optional bias term and neuron activation              function.
--
-- MPSMatrix A = empty matrix;              for (i = 0; i < N; ++i)                  A += alpha[i]*B[i];
--
-- if (bias)                  A += broadcast(bias);
--
-- if (neuron)                  A = applyNeuron(A);
--
-- Where B is the array of MPSMatrix objects, A is the destination              MPSMatrix, alpha is an array of scalar values, bias is a vector              which is broadcast and accumulated across each row of the intermediate              result, and applyNeuron is a neuron activation function.
--
-- Each matrix in the array may have an independent origin.
--
-- Generated bindings for @MPSMatrixSum@.
module ObjC.MetalPerformanceShaders.MPSMatrixSum
  ( MPSMatrixSum
  , IsMPSMatrixSum(..)
  , initWithDevice
  , initWithDevice_count_rows_columns_transpose
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndex
  , initWithCoder_device
  , rows
  , columns
  , count
  , transpose
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , columnsSelector
  , countSelector
  , encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_count_rows_columns_transposeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , neuronTypeSelector
  , rowsSelector
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , transposeSelector

  -- * Enum types
  , MPSCNNNeuronType(MPSCNNNeuronType)
  , pattern MPSCNNNeuronTypeNone
  , pattern MPSCNNNeuronTypeReLU
  , pattern MPSCNNNeuronTypeLinear
  , pattern MPSCNNNeuronTypeSigmoid
  , pattern MPSCNNNeuronTypeHardSigmoid
  , pattern MPSCNNNeuronTypeTanH
  , pattern MPSCNNNeuronTypeAbsolute
  , pattern MPSCNNNeuronTypeSoftPlus
  , pattern MPSCNNNeuronTypeSoftSign
  , pattern MPSCNNNeuronTypeELU
  , pattern MPSCNNNeuronTypePReLU
  , pattern MPSCNNNeuronTypeReLUN
  , pattern MPSCNNNeuronTypePower
  , pattern MPSCNNNeuronTypeExponential
  , pattern MPSCNNNeuronTypeLogarithm
  , pattern MPSCNNNeuronTypeGeLU
  , pattern MPSCNNNeuronTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> RawId -> IO (Id MPSMatrixSum)
initWithDevice mpsMatrixSum device =
  sendOwnedMessage mpsMatrixSum initWithDeviceSelector device

-- | Initialize a MPSMatrixSum kernel.
--
-- @device@ — The device on which to initialize the kernel.
--
-- @count@ — The number of matrices to be summed.
--
-- @rows@ — The number of rows to use in the input matrices.
--
-- @columns@ — The number of columns to use in the input matrices.
--
-- @transpose@ — If YES the result of the summation is to be transposed                                          prior to applying the bias and activation.
--
-- ObjC selector: @- initWithDevice:count:rows:columns:transpose:@
initWithDevice_count_rows_columns_transpose :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> RawId -> CULong -> CULong -> CULong -> Bool -> IO (Id MPSMatrixSum)
initWithDevice_count_rows_columns_transpose mpsMatrixSum device count rows columns transpose =
  sendOwnedMessage mpsMatrixSum initWithDevice_count_rows_columns_transposeSelector device count rows columns transpose

-- | Specifies a neuron activation function to be used.
--
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. An MPSMatrixSum kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
--
-- @neuronType@ — Type of neuron activation function. For full list see MPSCNNNeuronType.h
--
-- @parameterA@ — parameterA of neuron activation that is shared across all output values.
--
-- @parameterB@ — parameterB of neuron activation that is shared across all output values.
--
-- @parameterC@ — parameterC of neuron activation that is shared across all output values.
--
-- ObjC selector: @- setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixSum neuronType parameterA parameterB parameterC =
  sendMessage mpsMatrixSum setNeuronType_parameterA_parameterB_parameterCSelector neuronType parameterA parameterB parameterC

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO MPSCNNNeuronType
neuronType mpsMatrixSum =
  sendMessage mpsMatrixSum neuronTypeSelector

-- | Encode the operations to the command buffer
--
-- @buffer@ — The command buffer in which to encode the operation.
--
-- @sourceMatrices@ — A list of matrices from which the matrix data is read.
--
-- @resultMatrix@ — The result matrix.
--
-- @scaleVector@ — A MPSVector of type MPSDataTypeFloat32 containing the list of                                      scale factors, specified in single precision.
--
-- @offsetVector@ — A MPSVector of type MPSDataTypeUInt32 containing the list of                                      offsets, stored as a packed array of MPSMatrixOffset values.
--
-- @biasVector@ — A MPSVector containing the bias terms to add to the result                                      prior to applying the neuron function, if any.  May be nil.
--
-- @startIndex@ — The starting index into the scale and offset vectors.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:@
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndex :: (IsMPSMatrixSum mpsMatrixSum, IsNSArray sourceMatrices, IsMPSMatrix resultMatrix, IsMPSVector scaleVector, IsMPSVector offsetVector, IsMPSVector biasVector) => mpsMatrixSum -> RawId -> sourceMatrices -> resultMatrix -> scaleVector -> offsetVector -> biasVector -> CULong -> IO ()
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndex mpsMatrixSum buffer sourceMatrices resultMatrix scaleVector offsetVector biasVector startIndex =
  sendMessage mpsMatrixSum encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector buffer (toNSArray sourceMatrices) (toMPSMatrix resultMatrix) (toMPSVector scaleVector) (toMPSVector offsetVector) (toMPSVector biasVector) startIndex

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixSum kernel.
--
-- @device@ — The MTLDevice on which to make the MPSMatrixSum object.
--
-- Returns: A new MPSMatrixSum object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixSum mpsMatrixSum, IsNSCoder aDecoder) => mpsMatrixSum -> aDecoder -> RawId -> IO (Id MPSMatrixSum)
initWithCoder_device mpsMatrixSum aDecoder device =
  sendOwnedMessage mpsMatrixSum initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | The number of rows to sum.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
rows mpsMatrixSum =
  sendMessage mpsMatrixSum rowsSelector

-- | The number of columns to sum.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
columns mpsMatrixSum =
  sendMessage mpsMatrixSum columnsSelector

-- | The number of matrices to sum.
--
-- ObjC selector: @- count@
count :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
count mpsMatrixSum =
  sendMessage mpsMatrixSum countSelector

-- | The transposition used to initialize the kernel.
--
-- ObjC selector: @- transpose@
transpose :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO Bool
transpose mpsMatrixSum =
  sendMessage mpsMatrixSum transposeSelector

-- | Neuron parameter A.
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterA mpsMatrixSum =
  sendMessage mpsMatrixSum neuronParameterASelector

-- | Neuron parameter B.
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterB mpsMatrixSum =
  sendMessage mpsMatrixSum neuronParameterBSelector

-- | Neuron parameter C.
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterC mpsMatrixSum =
  sendMessage mpsMatrixSum neuronParameterCSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:count:rows:columns:transpose:@
initWithDevice_count_rows_columns_transposeSelector :: Selector '[RawId, CULong, CULong, CULong, Bool] (Id MPSMatrixSum)
initWithDevice_count_rows_columns_transposeSelector = mkSelector "initWithDevice:count:rows:columns:transpose:"

-- | @Selector@ for @setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterCSelector :: Selector '[MPSCNNNeuronType, CFloat, CFloat, CFloat] ()
setNeuronType_parameterA_parameterB_parameterCSelector = mkSelector "setNeuronType:parameterA:parameterB:parameterC:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:@
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector :: Selector '[RawId, Id NSArray, Id MPSMatrix, Id MPSVector, Id MPSVector, Id MPSVector, CULong] ()
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector = mkSelector "encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixSum)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] CULong
rowsSelector = mkSelector "rows"

-- | @Selector@ for @columns@
columnsSelector :: Selector '[] CULong
columnsSelector = mkSelector "columns"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @transpose@
transposeSelector :: Selector '[] Bool
transposeSelector = mkSelector "transpose"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector '[] CFloat
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector '[] CFloat
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector '[] CFloat
neuronParameterCSelector = mkSelector "neuronParameterC"

