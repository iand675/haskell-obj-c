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
  , nodeWithSourceSelector
  , initWithSourceSelector
  , nodeWithSource_alphaSelector
  , initWithSource_alphaSelector
  , alphaSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node representing a MPSNNGramMatrixCalculationNode kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter.
--
-- Returns: A new MPSNNFilter node for a MPSNNGramMatrixCalculationNode kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode, IsMPSNNImageNode sourceNode) => mpsnnGramMatrixCalculationNode -> sourceNode -> IO (Id MPSNNGramMatrixCalculationNode)
initWithSource mpsnnGramMatrixCalculationNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpsnnGramMatrixCalculationNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:alpha:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral alpha)] >>= retainedObject . castPtr

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
initWithSource_alpha mpsnnGramMatrixCalculationNode  sourceNode alpha =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpsnnGramMatrixCalculationNode (mkSelector "initWithSource:alpha:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral alpha)] >>= ownedObject . castPtr

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculationNode mpsnnGramMatrixCalculationNode => mpsnnGramMatrixCalculationNode -> IO CFloat
alpha mpsnnGramMatrixCalculationNode  =
  sendMsg mpsnnGramMatrixCalculationNode (mkSelector "alpha") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:alpha:@
nodeWithSource_alphaSelector :: Selector
nodeWithSource_alphaSelector = mkSelector "nodeWithSource:alpha:"

-- | @Selector@ for @initWithSource:alpha:@
initWithSource_alphaSelector :: Selector
initWithSource_alphaSelector = mkSelector "initWithSource:alpha:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

