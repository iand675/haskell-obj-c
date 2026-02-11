{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , initWithDevice_count_rows_columns_transposeSelector
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , neuronTypeSelector
  , encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector
  , initWithCoder_deviceSelector
  , rowsSelector
  , columnsSelector
  , countSelector
  , transposeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> RawId -> IO (Id MPSMatrixSum)
initWithDevice mpsMatrixSum  device =
  sendMsg mpsMatrixSum (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_count_rows_columns_transpose mpsMatrixSum  device count rows columns transpose =
  sendMsg mpsMatrixSum (mkSelector "initWithDevice:count:rows:columns:transpose:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count), argCULong (fromIntegral rows), argCULong (fromIntegral columns), argCULong (if transpose then 1 else 0)] >>= ownedObject . castPtr

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
setNeuronType_parameterA_parameterB_parameterC mpsMatrixSum  neuronType parameterA parameterB parameterC =
  sendMsg mpsMatrixSum (mkSelector "setNeuronType:parameterA:parameterB:parameterC:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB), argCFloat (fromIntegral parameterC)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO MPSCNNNeuronType
neuronType mpsMatrixSum  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpsMatrixSum (mkSelector "neuronType") retCInt []

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
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndex mpsMatrixSum  buffer sourceMatrices resultMatrix scaleVector offsetVector biasVector startIndex =
withObjCPtr sourceMatrices $ \raw_sourceMatrices ->
  withObjCPtr resultMatrix $ \raw_resultMatrix ->
    withObjCPtr scaleVector $ \raw_scaleVector ->
      withObjCPtr offsetVector $ \raw_offsetVector ->
        withObjCPtr biasVector $ \raw_biasVector ->
            sendMsg mpsMatrixSum (mkSelector "encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:") retVoid [argPtr (castPtr (unRawId buffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrices :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ()), argPtr (castPtr raw_scaleVector :: Ptr ()), argPtr (castPtr raw_offsetVector :: Ptr ()), argPtr (castPtr raw_biasVector :: Ptr ()), argCULong (fromIntegral startIndex)]

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
initWithCoder_device mpsMatrixSum  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixSum (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | The number of rows to sum.
--
-- ObjC selector: @- rows@
rows :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
rows mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "rows") retCULong []

-- | The number of columns to sum.
--
-- ObjC selector: @- columns@
columns :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
columns mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "columns") retCULong []

-- | The number of matrices to sum.
--
-- ObjC selector: @- count@
count :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CULong
count mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "count") retCULong []

-- | The transposition used to initialize the kernel.
--
-- ObjC selector: @- transpose@
transpose :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO Bool
transpose mpsMatrixSum  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsMatrixSum (mkSelector "transpose") retCULong []

-- | Neuron parameter A.
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterA mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "neuronParameterA") retCFloat []

-- | Neuron parameter B.
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterB mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "neuronParameterB") retCFloat []

-- | Neuron parameter C.
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixSum mpsMatrixSum => mpsMatrixSum -> IO CFloat
neuronParameterC mpsMatrixSum  =
  sendMsg mpsMatrixSum (mkSelector "neuronParameterC") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:count:rows:columns:transpose:@
initWithDevice_count_rows_columns_transposeSelector :: Selector
initWithDevice_count_rows_columns_transposeSelector = mkSelector "initWithDevice:count:rows:columns:transpose:"

-- | @Selector@ for @setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterCSelector :: Selector
setNeuronType_parameterA_parameterB_parameterCSelector = mkSelector "setNeuronType:parameterA:parameterB:parameterC:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:@
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector :: Selector
encodeToCommandBuffer_sourceMatrices_resultMatrix_scaleVector_offsetVector_biasVector_startIndexSelector = mkSelector "encodeToCommandBuffer:sourceMatrices:resultMatrix:scaleVector:offsetVector:biasVector:startIndex:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @columns@
columnsSelector :: Selector
columnsSelector = mkSelector "columns"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @transpose@
transposeSelector :: Selector
transposeSelector = mkSelector "transpose"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector
neuronParameterCSelector = mkSelector "neuronParameterC"

