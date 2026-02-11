{-# LANGUAGE PatternSynonyms #-}
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
  , nodeWithSource_weightsSelector
  , initWithSource_weightsSelector
  , trainingStyleSelector
  , setTrainingStyleSelector
  , accumulatorPrecisionSelector
  , setAccumulatorPrecisionSelector
  , convolutionGradientStateSelector

  -- * Enum types
  , MPSNNConvolutionAccumulatorPrecisionOption(MPSNNConvolutionAccumulatorPrecisionOption)
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionHalf
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionFloat
  , MPSNNTrainingStyle(MPSNNTrainingStyle)
  , pattern MPSNNTrainingStyleUpdateDeviceNone
  , pattern MPSNNTrainingStyleUpdateDeviceCPU
  , pattern MPSNNTrainingStyleUpdateDeviceGPU

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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= retainedObject . castPtr

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
initWithSource_weights mpscnnConvolutionNode  sourceNode weights =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnConvolutionNode (mkSelector "initWithSource:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnConvolutionNode  =
  fmap (coerce :: CULong -> MPSNNTrainingStyle) $ sendMsg mpscnnConvolutionNode (mkSelector "trainingStyle") retCULong []

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnConvolutionNode  value =
  sendMsg mpscnnConvolutionNode (mkSelector "setTrainingStyle:") retVoid [argCULong (coerce value)]

-- | Set the floating-point precision used by the convolution accumulator
--
-- Default:  MPSNNConvolutionAccumulatorPrecisionOptionFloat
--
-- ObjC selector: @- accumulatorPrecision@
accumulatorPrecision :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecision mpscnnConvolutionNode  =
  fmap (coerce :: CULong -> MPSNNConvolutionAccumulatorPrecisionOption) $ sendMsg mpscnnConvolutionNode (mkSelector "accumulatorPrecision") retCULong []

-- | Set the floating-point precision used by the convolution accumulator
--
-- Default:  MPSNNConvolutionAccumulatorPrecisionOptionFloat
--
-- ObjC selector: @- setAccumulatorPrecision:@
setAccumulatorPrecision :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecision mpscnnConvolutionNode  value =
  sendMsg mpscnnConvolutionNode (mkSelector "setAccumulatorPrecision:") retVoid [argCULong (coerce value)]

-- | A node to represent a MPSCNNConvolutionGradientState object
--
-- Use this if the convolution is mirrored by a convolution transpose node               later on in the graph to make sure that the size of the image returned               from the convolution transpose matches the size of the image passed in               to this node.
--
-- ObjC selector: @- convolutionGradientState@
convolutionGradientState :: IsMPSCNNConvolutionNode mpscnnConvolutionNode => mpscnnConvolutionNode -> IO (Id MPSCNNConvolutionGradientStateNode)
convolutionGradientState mpscnnConvolutionNode  =
  sendMsg mpscnnConvolutionNode (mkSelector "convolutionGradientState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:weights:@
nodeWithSource_weightsSelector :: Selector
nodeWithSource_weightsSelector = mkSelector "nodeWithSource:weights:"

-- | @Selector@ for @initWithSource:weights:@
initWithSource_weightsSelector :: Selector
initWithSource_weightsSelector = mkSelector "initWithSource:weights:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

-- | @Selector@ for @accumulatorPrecision@
accumulatorPrecisionSelector :: Selector
accumulatorPrecisionSelector = mkSelector "accumulatorPrecision"

-- | @Selector@ for @setAccumulatorPrecision:@
setAccumulatorPrecisionSelector :: Selector
setAccumulatorPrecisionSelector = mkSelector "setAccumulatorPrecision:"

-- | @Selector@ for @convolutionGradientState@
convolutionGradientStateSelector :: Selector
convolutionGradientStateSelector = mkSelector "convolutionGradientState"

