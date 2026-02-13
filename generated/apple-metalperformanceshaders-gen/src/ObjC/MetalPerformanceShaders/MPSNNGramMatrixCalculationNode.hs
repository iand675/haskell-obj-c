{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSNNGramMatrixCalculation kernel
--
-- Generated bindings for @MPSNNGramMatrixCalculationNode@.
module ObjC.MetalPerformanceShaders.MPSNNGramMatrixCalculationNode
  ( MPSNNGramMatrixCalculationNode
  , IsMPSNNGramMatrixCalculationNode(..)
  , nodeWithSource
  , initWithSource
  , nodeWithSource_alpha
  , initWithSource_alpha
  , alpha
  , propertyCallBack
  , setPropertyCallBack
  , alphaSelector
  , initWithSourceSelector
  , initWithSource_alphaSelector
  , nodeWithSourceSelector
  , nodeWithSource_alphaSelector
  , propertyCallBackSelector
  , setPropertyCallBackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init a node representing a autoreleased MPSNNGramMatrixCalculationNode kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter.
--
-- Returns: A new MPSNNFilter node for a MPSNNGramMatrixCalculationNode kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSNNGramMatrixCalculationNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node representing a MPSNNGramMatrixCalculationNode kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter.
--
-- Returns: A new MPSNNFilter node for a MPSNNGramMatrixCalculationNode kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode, IsMPSNNImageNode sourceNode) => mpsnnGramMatrixCalculationNode -> sourceNode -> IO (Id MPSNNGramMatrixCalculationNode)
initWithSource mpsnnGramMatrixCalculationNode sourceNode =
  sendOwnedMessage mpsnnGramMatrixCalculationNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | Init a node representing a autoreleased MPSNNGramMatrixCalculationNode kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter.
--
-- @alpha@ — Scaling factor for the output.
--
-- Returns: A new MPSNNFilter node for a MPSNNGramMatrixCalculationNode kernel.
--
-- ObjC selector: @+ nodeWithSource:alpha:@
nodeWithSource_alpha :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> IO (Id MPSNNGramMatrixCalculationNode)
nodeWithSource_alpha sourceNode alpha =
  do
    cls' <- getRequiredClass "MPSNNGramMatrixCalculationNode"
    sendClassMessage cls' nodeWithSource_alphaSelector (toMPSNNImageNode sourceNode) alpha

-- | Init a node representing a MPSNNGramMatrixCalculationNode kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter.
--
-- @alpha@ — Scaling factor for the output.
--
-- Returns: A new MPSNNFilter node for a MPSNNGramMatrixCalculationNode kernel.
--
-- ObjC selector: @- initWithSource:alpha:@
initWithSource_alpha :: (IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode, IsMPSNNImageNode sourceNode) => mpsnnGramMatrixCalculationNode -> sourceNode -> CFloat -> IO (Id MPSNNGramMatrixCalculationNode)
initWithSource_alpha mpsnnGramMatrixCalculationNode sourceNode alpha =
  sendOwnedMessage mpsnnGramMatrixCalculationNode initWithSource_alphaSelector (toMPSNNImageNode sourceNode) alpha

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode => mpsnnGramMatrixCalculationNode -> IO CFloat
alpha mpsnnGramMatrixCalculationNode =
  sendMessage mpsnnGramMatrixCalculationNode alphaSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the alpha value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- propertyCallBack@
propertyCallBack :: IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode => mpsnnGramMatrixCalculationNode -> IO RawId
propertyCallBack mpsnnGramMatrixCalculationNode =
  sendMessage mpsnnGramMatrixCalculationNode propertyCallBackSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the alpha value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- setPropertyCallBack:@
setPropertyCallBack :: IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode => mpsnnGramMatrixCalculationNode -> RawId -> IO ()
setPropertyCallBack mpsnnGramMatrixCalculationNode value =
  sendMessage mpsnnGramMatrixCalculationNode setPropertyCallBackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNGramMatrixCalculationNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNGramMatrixCalculationNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:alpha:@
nodeWithSource_alphaSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSNNGramMatrixCalculationNode)
nodeWithSource_alphaSelector = mkSelector "nodeWithSource:alpha:"

-- | @Selector@ for @initWithSource:alpha:@
initWithSource_alphaSelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSNNGramMatrixCalculationNode)
initWithSource_alphaSelector = mkSelector "initWithSource:alpha:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @propertyCallBack@
propertyCallBackSelector :: Selector '[] RawId
propertyCallBackSelector = mkSelector "propertyCallBack"

-- | @Selector@ for @setPropertyCallBack:@
setPropertyCallBackSelector :: Selector '[RawId] ()
setPropertyCallBackSelector = mkSelector "setPropertyCallBack:"

