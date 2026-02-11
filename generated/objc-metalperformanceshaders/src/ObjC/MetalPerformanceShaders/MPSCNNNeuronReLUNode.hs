{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronReLU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x            if x >= 0
-- = a * x        if x < 0
--
-- Generated bindings for @MPSCNNNeuronReLUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUNode
  ( MPSCNNNeuronReLUNode
  , IsMPSCNNNeuronReLUNode(..)
  , nodeWithSource_a
  , nodeWithSource
  , initWithSource
  , initWithSource_a
  , nodeWithSource_aSelector
  , nodeWithSourceSelector
  , initWithSourceSelector
  , initWithSource_aSelector


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

-- | @+ nodeWithSource:a:@
nodeWithSource_a :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> IO (Id MPSCNNNeuronReLUNode)
nodeWithSource_a sourceNode a =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:a:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronReLUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronReLUNode mpscnnNeuronReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNode -> sourceNode -> IO (Id MPSCNNNeuronReLUNode)
initWithSource mpscnnNeuronReLUNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronReLUNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:a:@
initWithSource_a :: (IsMPSCNNNeuronReLUNode mpscnnNeuronReLUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNode -> sourceNode -> CFloat -> IO (Id MPSCNNNeuronReLUNode)
initWithSource_a mpscnnNeuronReLUNode  sourceNode a =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronReLUNode (mkSelector "initWithSource:a:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:@
nodeWithSource_aSelector :: Selector
nodeWithSource_aSelector = mkSelector "nodeWithSource:a:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:a:@
initWithSource_aSelector :: Selector
initWithSource_aSelector = mkSelector "initWithSource:a:"

