{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node for a unary MPSNNReduce node.
--
-- This is an abstract base class that does not correspond with any              particular MPSCNNKernel. Please make one of the MPSNNReduction              subclasses instead.
--
-- Generated bindings for @MPSNNUnaryReductionNode@.
module ObjC.MetalPerformanceShaders.MPSNNUnaryReductionNode
  ( MPSNNUnaryReductionNode
  , IsMPSNNUnaryReductionNode(..)
  , nodeWithSource
  , initWithSource
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

-- | Create an autoreleased node representing an MPS reduction kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for an MPS reduction kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSNNUnaryReductionNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSNNUnaryReductionNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node representing an MPS reduction kernel.
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for an MPS reduction kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNUnaryReductionNode mpsnnUnaryReductionNode, IsMPSNNImageNode sourceNode) => mpsnnUnaryReductionNode -> sourceNode -> IO (Id MPSNNUnaryReductionNode)
initWithSource mpsnnUnaryReductionNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpsnnUnaryReductionNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

