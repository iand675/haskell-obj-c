{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNFilterNode representing a MPSCNNConvolution kernel
--
-- Generated bindings for @MPSCNNConvolutionNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionNode
  ( MPSCNNConvolutionNode
  , IsMPSCNNConvolutionNode(..)
  , nodeWithSource_weights
  , initWithSource_weights
  , trainingStyle
  , setTrainingStyle
  , accumulatorPrecision
  , setAccumulatorPrecision
  , convolutionGradientState
  , accumulatorPrecisionSelector
  , convolutionGradientStateSelector
  , initWithSource_weightsSelector
  , nodeWithSource_weightsSelector
  , setAccumulatorPrecisionSelector
  , setTrainingStyleSelector
  , trainingStyleSelector

  -- * Enum types
  , MPSNNConvolutionAccumulatorPrecisionOption(MPSNNConvolutionAccumulatorPrecisionOption)
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionHalf
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionFloat
  , MPSNNTrainingStyle(MPSNNTrainingStyle)
  , pattern MPSNNTrainingStyleUpdateDeviceNone
  , pattern MPSNNTrainingStyleUpdateDeviceCPU
  , pattern MPSNNTrainingStyleUpdateDeviceGPU

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

-- | Init an autoreleased not representing a MPSCNNConvolution kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases. If it is used for training, it may not                                      have a neuron embedded in the convolution descriptor.
--
-- Returns: A new MPSNNFilter node for a MPSCNNConvolution kernel.
--
-- ObjC selector: @+ nodeWithSource:weights:@
nodeWithSource_weights :: IsMPSNNImageNode sourceNode => sourceNode -> RawId -> IO (Id MPSCNNConvolutionNode)
nodeWithSource_weights sourceNode weights =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionNode"
    sendClassMessage cls' nodeWithSource_weightsSelector (toMPSNNImageNode sourceNode) weights

-- | Init a node representing a MPSCNNConvolution kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases. If it is used for training, it may not                                      have a neuron embedded in the convolution descriptor.
--
-- Returns: A new MPSNNFilter node for a MPSCNNConvolution kernel.
--
-- ObjC selector: @- initWithSource:weights:@
initWithSource_weights :: (IsMPSCNNConvolutionNode mpscnnConvolutionNode, IsMPSNNImageNode sourceNode) => mpscnnConvolutionNode -> sourceNode -> RawId -> IO (Id MPSCNNConvolutionNode)
initWithSource_weights mpscnnConvolutionNode sourceNode weights =
  sendOwnedMessage mpscnnConvolutionNode initWithSource_weightsSelector (toMPSNNImageNode sourceNode) weights

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnConvolutionNode =
  sendMessage mpscnnConvolutionNode trainingStyleSelector

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnConvolutionNode value =
  sendMessage mpscnnConvolutionNode setTrainingStyleSelector value

-- | Set the floating-point precision used by the convolution accumulator
--
-- Default:  MPSNNConvolutionAccumulatorPrecisionOptionFloat
--
-- ObjC selector: @- accumulatorPrecision@
accumulatorPrecision :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecision mpscnnConvolutionNode =
  sendMessage mpscnnConvolutionNode accumulatorPrecisionSelector

-- | Set the floating-point precision used by the convolution accumulator
--
-- Default:  MPSNNConvolutionAccumulatorPrecisionOptionFloat
--
-- ObjC selector: @- setAccumulatorPrecision:@
setAccumulatorPrecision :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecision mpscnnConvolutionNode value =
  sendMessage mpscnnConvolutionNode setAccumulatorPrecisionSelector value

-- | A node to represent a MPSCNNConvolutionGradientState object
--
-- Use this if the convolution is mirrored by a convolution transpose node               later on in the graph to make sure that the size of the image returned               from the convolution transpose matches the size of the image passed in               to this node.
--
-- ObjC selector: @- convolutionGradientState@
convolutionGradientState :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO (Id MPSCNNConvolutionGradientStateNode)
convolutionGradientState mpscnnConvolutionNode =
  sendMessage mpscnnConvolutionNode convolutionGradientStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:@
nodeWithSource_weightsSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNConvolutionNode)
nodeWithSource_weightsSelector = mkSelector "nodeWithSource:weights:"

-- | @Selector@ for @initWithSource:weights:@
initWithSource_weightsSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNConvolutionNode)
initWithSource_weightsSelector = mkSelector "initWithSource:weights:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector '[] MPSNNTrainingStyle
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector '[MPSNNTrainingStyle] ()
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

-- | @Selector@ for @accumulatorPrecision@
accumulatorPrecisionSelector :: Selector '[] MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionSelector = mkSelector "accumulatorPrecision"

-- | @Selector@ for @setAccumulatorPrecision:@
setAccumulatorPrecisionSelector :: Selector '[MPSNNConvolutionAccumulatorPrecisionOption] ()
setAccumulatorPrecisionSelector = mkSelector "setAccumulatorPrecision:"

-- | @Selector@ for @convolutionGradientState@
convolutionGradientStateSelector :: Selector '[] (Id MPSCNNConvolutionGradientStateNode)
convolutionGradientStateSelector = mkSelector "convolutionGradientState"

