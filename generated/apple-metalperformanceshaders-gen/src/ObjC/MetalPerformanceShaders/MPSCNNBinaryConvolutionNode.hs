{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNFilterNode representing a MPSCNNBinaryConvolution kernel
--
-- Generated bindings for @MPSCNNBinaryConvolutionNode@.
module ObjC.MetalPerformanceShaders.MPSCNNBinaryConvolutionNode
  ( MPSCNNBinaryConvolutionNode
  , IsMPSCNNBinaryConvolutionNode(..)
  , nodeWithSource_weights_scaleValue_type_flags
  , initWithSource_weights_scaleValue_type_flags
  , nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags
  , convolutionGradientState
  , convolutionGradientStateSelector
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

-- | Init an autoreleased node representing a MPSCNNBinaryConvolution kernel
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
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryConvolution kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:scaleValue:type:flags:@
nodeWithSource_weights_scaleValue_type_flags :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolutionNode)
nodeWithSource_weights_scaleValue_type_flags sourceNode weights scaleValue type_ flags =
  do
    cls' <- getRequiredClass "MPSCNNBinaryConvolutionNode"
    sendClassMessage cls' nodeWithSource_weights_scaleValue_type_flagsSelector (toMPSNNImageNode sourceNode) weights scaleValue type_ flags

-- | Init a node representing a MPSCNNBinaryConvolution kernel
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
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryConvolution kernel.
--
-- ObjC selector: @- initWithSource:weights:scaleValue:type:flags:@
initWithSource_weights_scaleValue_type_flags :: (IsMPSCNNBinaryConvolutionNode mpscnnBinaryConvolutionNode, IsMPSNNImageNode sourceNode) => mpscnnBinaryConvolutionNode -> sourceNode -> RawId -> CFloat -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolutionNode)
initWithSource_weights_scaleValue_type_flags mpscnnBinaryConvolutionNode sourceNode weights scaleValue type_ flags =
  sendOwnedMessage mpscnnBinaryConvolutionNode initWithSource_weights_scaleValue_type_flagsSelector (toMPSNNImageNode sourceNode) weights scaleValue type_ flags

-- | Init an autoreleased node representing a MPSCNNBinaryConvolution kernel
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
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryConvolution kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolutionNode)
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags sourceNode weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  do
    cls' <- getRequiredClass "MPSCNNBinaryConvolutionNode"
    sendClassMessage cls' nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector (toMPSNNImageNode sourceNode) weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags

-- | Init a node representing a MPSCNNBinaryConvolution kernel
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
-- Returns: A new MPSNNFilter node for a MPSCNNBinaryConvolution kernel.
--
-- ObjC selector: @- initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags :: (IsMPSCNNBinaryConvolutionNode mpscnnBinaryConvolutionNode, IsMPSNNImageNode sourceNode) => mpscnnBinaryConvolutionNode -> sourceNode -> RawId -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNBinaryConvolutionType -> MPSCNNBinaryConvolutionFlags -> IO (Id MPSCNNBinaryConvolutionNode)
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flags mpscnnBinaryConvolutionNode sourceNode weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags =
  sendOwnedMessage mpscnnBinaryConvolutionNode initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector (toMPSNNImageNode sourceNode) weights outputBiasTerms outputScaleTerms inputBiasTerms inputScaleTerms type_ flags

-- | unavailable
--
-- ObjC selector: @- convolutionGradientState@
convolutionGradientState :: IsMPSCNNBinaryConvolutionNode mpscnnBinaryConvolutionNode => mpscnnBinaryConvolutionNode -> IO RawId
convolutionGradientState mpscnnBinaryConvolutionNode =
  sendMessage mpscnnBinaryConvolutionNode convolutionGradientStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:scaleValue:type:flags:@
nodeWithSource_weights_scaleValue_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, CFloat, MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryConvolutionNode)
nodeWithSource_weights_scaleValue_type_flagsSelector = mkSelector "nodeWithSource:weights:scaleValue:type:flags:"

-- | @Selector@ for @initWithSource:weights:scaleValue:type:flags:@
initWithSource_weights_scaleValue_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, CFloat, MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryConvolutionNode)
initWithSource_weights_scaleValue_type_flagsSelector = mkSelector "initWithSource:weights:scaleValue:type:flags:"

-- | @Selector@ for @nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryConvolutionNode)
nodeWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector = mkSelector "nodeWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:"

-- | @Selector@ for @initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:@
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector :: Selector '[Id MPSNNImageNode, RawId, Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNBinaryConvolutionType, MPSCNNBinaryConvolutionFlags] (Id MPSCNNBinaryConvolutionNode)
initWithSource_weights_outputBiasTerms_outputScaleTerms_inputBiasTerms_inputScaleTerms_type_flagsSelector = mkSelector "initWithSource:weights:outputBiasTerms:outputScaleTerms:inputBiasTerms:inputScaleTerms:type:flags:"

-- | @Selector@ for @convolutionGradientState@
convolutionGradientStateSelector :: Selector '[] RawId
convolutionGradientStateSelector = mkSelector "convolutionGradientState"

