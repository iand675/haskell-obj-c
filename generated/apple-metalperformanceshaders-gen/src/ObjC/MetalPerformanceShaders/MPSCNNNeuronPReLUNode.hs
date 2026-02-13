{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ReLU node with parameter a provided independently for each feature channel
--
-- For each pixel, applies the following function:
--
-- f(x) = x                if x >= 0
-- = aData[i] * x     if x < 0,  i is the index of the feature channel
-- @param      sourceNode              The MPSNNImageNode representing the source MPSImage for the filter
-- @param      aData                   An array of single precision floating-point alpha values to use
--
-- Generated bindings for @MPSCNNNeuronPReLUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronPReLUNode
  ( MPSCNNNeuronPReLUNode
  , IsMPSCNNNeuronPReLUNode(..)
  , nodeWithSource_aData
  , initWithSource_aData
  , nodeWithSource
  , initWithSource
  , initWithSourceSelector
  , initWithSource_aDataSelector
  , nodeWithSourceSelector
  , nodeWithSource_aDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:aData:@
nodeWithSource_aData :: (IsMPSNNImageNode sourceNode, IsNSData aData) => sourceNode -> aData -> IO (Id MPSCNNNeuronPReLUNode)
nodeWithSource_aData sourceNode aData =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPReLUNode"
    sendClassMessage cls' nodeWithSource_aDataSelector (toMPSNNImageNode sourceNode) (toNSData aData)

-- | Init a node representing a MPSCNNNeuronTanH kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x                if x >= 0
-- = aData[i] * x     if x < 0,  i is the index of the feature channel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @aData@ — An array of single precision floating-point alpha values to use
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronTanH kernel.
--
-- ObjC selector: @- initWithSource:aData:@
initWithSource_aData :: (IsMPSCNNNeuronPReLUNode mpscnnNeuronPReLUNode, IsMPSNNImageNode sourceNode, IsNSData aData) => mpscnnNeuronPReLUNode -> sourceNode -> aData -> IO (Id MPSCNNNeuronPReLUNode)
initWithSource_aData mpscnnNeuronPReLUNode sourceNode aData =
  sendOwnedMessage mpscnnNeuronPReLUNode initWithSource_aDataSelector (toMPSNNImageNode sourceNode) (toNSData aData)

-- | @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronPReLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPReLUNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronPReLUNode mpscnnNeuronPReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronPReLUNode -> sourceNode -> IO (Id MPSCNNNeuronPReLUNode)
initWithSource mpscnnNeuronPReLUNode sourceNode =
  sendOwnedMessage mpscnnNeuronPReLUNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:aData:@
nodeWithSource_aDataSelector :: Selector '[Id MPSNNImageNode, Id NSData] (Id MPSCNNNeuronPReLUNode)
nodeWithSource_aDataSelector = mkSelector "nodeWithSource:aData:"

-- | @Selector@ for @initWithSource:aData:@
initWithSource_aDataSelector :: Selector '[Id MPSNNImageNode, Id NSData] (Id MPSCNNNeuronPReLUNode)
initWithSource_aDataSelector = mkSelector "initWithSource:aData:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronPReLUNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNeuronPReLUNode)
initWithSourceSelector = mkSelector "initWithSource:"

