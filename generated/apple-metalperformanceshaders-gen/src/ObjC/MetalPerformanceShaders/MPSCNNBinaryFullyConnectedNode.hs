{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNFilterNode representing a MPSCNNBinaryFullyConnected kernel
--
-- Generated bindings for @MPSCNNBinaryFullyConnectedNode@.
module ObjC.MetalPerformanceShaders.MPSCNNBinaryFullyConnectedNode
  ( MPSCNNBinaryFullyConnectedNode
  , IsMPSCNNBinaryFullyConnectedNode(..)
  , nodeWithSource_weights_scaleValue_type_flags
  , initWithSource_weights_scaleValue_type_flags
  , nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector
  , initWithSource_weights_scaleValue_type_flagsSelector
  , nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector
  , nodeWithSource_weights_scaleValue_type_flagsSelector

  -- * Enum types
  , MPSCNNBinaryConvolutionFlags(MPSCNNBinaryConvolutionFlags)
  , pattern MPSCNNBinaryConvolutionFlagsNone
  , pattern MPSCNNBinaryConvolutionFlagsUseBetaScaling
  , MPSCNNBinaryConvolutionType(MPSCNNBinaryConvolutionType)
  , pattern MPSCNNBinaryConvolutionTypeBinaryWeights
  , pattern MPSCNNBinaryConvolutionTypeXNOR
  , pattern MPSCNNBinaryConvolutionTypeAND

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

-- | Init an autoreleased node representing a MPSCNNBinaryFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- @scaleValue@ — A floating point value used to scale the entire convolution.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryFullyConnected kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:scaleValue:type:flags:@
nodeWithSource_weights_scaleValue_type_flags :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnectedNode)
nodeWithSource_weights_scaleValue_type_flags sourceNode weights scaleValue type_ flags =
  do
    cls' <- getRequiredClass "MPSCNNBinaryFullyConnectedNode"
    sendClassMessage cls' nodeWithSource_weights_scaleValue_type_flagsSelector (toMPSNNImageNode sourceNode) weights scaleValue type_ flags

-- | Init a node representing a MPSCNNBinaryFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- @scaleValue@ — A floating point value used to scale the entire convolution.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryFullyConnected kernel.
--
-- ObjC selector: @- initWithSource:weights:scaleValue:type:flags:@
initWithSource_weights_scaleValue_type_flags :: (IsMPSCNNBinaryFullyConnectedNode mpscnnBinaryFullyConnectedNode, IsMPSNNImageNode sourceNode) => mpscnnBinaryFullyConnectedNode -> sourceNode -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnectedNode)
initWithSource_weights_scaleValue_type_flags mpscnnBinaryFullyConnectedNode sourceNode weights scaleValue type_ flags =
  sendOwnedMessage mpscnnBinaryFullyConnectedNode initWithSource_weights_scaleValue_type_flagsSelector (toMPSNNImageNode sourceNode) weights scaleValue type_ flags

-- | Init an autoreleased node representing a MPSCNNBinaryFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- @outputBiasTerms@ — A pointer to bias terms to be applied to the convolution output.                                      See MPSCNNBinaryConvolution for more details.
--
-- @outputScaleTerms@ — A pointer to scale terms to be applied to binary convolution                                      results per output feature channel. See MPSCNNBinaryConvolution for more details.
--
-- @inputBiasTerms@ — A pointer to offset terms to be applied to the input before convolution and                                      before input scaling. See MPSCNNBinaryConvolution for more details.
--
-- @inputScaleTerms@ — A pointer to scale terms to be applied to the input before convolution,                                      but after input biasing. See MPSCNNBinaryConvolution for more details.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryFullyConnected kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnectedNode)
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags sourceNode weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  do
    cls' <- getRequiredClass "MPSCNNBinaryFullyConnectedNode"
    sendClassMessage cls' nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector (toMPSNNImageNode sourceNode) weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags

-- | Init a node representing a MPSCNNBinaryFullyConnected kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- @outputBiasTerms@ — A pointer to bias terms to be applied to the convolution output.                                      See MPSCNNBinaryConvolution for more details.
--
-- @outputScaleTerms@ — A pointer to scale terms to be applied to binary convolution                                      results per output feature channel. See MPSCNNBinaryConvolution for more details.
--
-- @inputBiasTerms@ — A pointer to offset terms to be applied to the input before convolution and                                      before input scaling. See MPSCNNBinaryConvolution for more details.
--
-- @inputScaleTerms@ — A pointer to scale terms to be applied to the input before convolution,                                      but after input biasing. See MPSCNNBinaryConvolution for more details.
--
-- @type@ — What kind of binarization strategy is to be used.
--
-- @flags@ — See documentation of MPSCNNBinaryConvolutionFlags.
--
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryFullyConnected kernel.
--
-- ObjC selector: @- initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: (IsMPSCNNBinaryFullyConnectedNode mpscnnBinaryFullyConnectedNode, IsMPSNNImageNode sourceNode) => mpscnnBinaryFullyConnectedNode -> sourceNode -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryFullyConnectedNode)
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags mpscnnBinaryFullyConnectedNode sourceNode weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  sendOwnedMessage mpscnnBinaryFullyConnectedNode initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector (toMPSNNImageNode sourceNode) weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:scaleValue:type:flags:@
nodeWithSource_weights_scaleValue_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, CFloat, MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryFullyConnectedNode)
nodeWithSource_weights_scaleValue_type_flagsSelector = mkSelector "nodeWithSource:weights:scaleValue:type:flags:"

-- | @Selector@ for @initWithSource:weights:scaleValue:type:flags:@
initWithSource_weights_scaleValue_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, CFloat, MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryFullyConnectedNode)
initWithSource_weights_scaleValue_type_flagsSelector = mkSelector "initWithSource:weights:scaleValue:type:flags:"

-- | @Selector@ for @nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryFullyConnectedNode)
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector = mkSelector "nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:"

-- | @Selector@ for @initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryFullyConnectedNode)
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector = mkSelector "initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:"

