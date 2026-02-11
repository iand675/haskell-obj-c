{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNSoftMax kernel
--
-- Generated bindings for @MPSCNNSoftMaxNode@.
module ObjC.MetalPerformanceShaders.MPSCNNSoftMaxNode
  ( MPSCNNSoftMaxNode
  , IsMPSCNNSoftMaxNode(..)
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

-- | Init a node representing a autoreleased MPSCNNSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNSoftMax kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNSoftMaxNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNSoftMaxNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node representing a MPSCNNSoftMax kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSCNNSoftMax kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNSoftMaxNode mpscnnSoftMaxNode, IsMPSNNImageNode sourceNode) => mpscnnSoftMaxNode -> sourceNode -> IO (Id MPSCNNSoftMaxNode)
initWithSource mpscnnSoftMaxNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnSoftMaxNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

