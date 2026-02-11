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
  , nodeWithSource_aDataSelector
  , initWithSource_aDataSelector
  , nodeWithSourceSelector
  , initWithSourceSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:aData:@
nodeWithSource_aData :: (IsMPSNNImageNode sourceNode, IsNSData aData) => sourceNode -> aData -> IO (Id MPSCNNNeuronPReLUNode)
nodeWithSource_aData sourceNode aData =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPReLUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      withObjCPtr aData $ \raw_aData ->
        sendClassMsg cls' (mkSelector "nodeWithSource:aData:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr raw_aData :: Ptr ())] >>= retainedObject . castPtr

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
initWithSource_aData mpscnnNeuronPReLUNode  sourceNode aData =
withObjCPtr sourceNode $ \raw_sourceNode ->
  withObjCPtr aData $ \raw_aData ->
      sendMsg mpscnnNeuronPReLUNode (mkSelector "initWithSource:aData:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr raw_aData :: Ptr ())] >>= ownedObject . castPtr

-- | @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronPReLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronPReLUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronPReLUNode mpscnnNeuronPReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronPReLUNode -> sourceNode -> IO (Id MPSCNNNeuronPReLUNode)
initWithSource mpscnnNeuronPReLUNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronPReLUNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:aData:@
nodeWithSource_aDataSelector :: Selector
nodeWithSource_aDataSelector = mkSelector "nodeWithSource:aData:"

-- | @Selector@ for @initWithSource:aData:@
initWithSource_aDataSelector :: Selector
initWithSource_aDataSelector = mkSelector "initWithSource:aData:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

