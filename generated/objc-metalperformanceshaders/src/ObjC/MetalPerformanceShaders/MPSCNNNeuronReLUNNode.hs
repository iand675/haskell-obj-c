{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronReLUN kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = min((x >= 0 ? x : a * x), b)
--
-- Generated bindings for @MPSCNNNeuronReLUNNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUNNode
  ( MPSCNNNeuronReLUNNode
  , IsMPSCNNNeuronReLUNNode(..)
  , nodeWithSource_a_b
  , initWithSource_a_b
  , nodeWithSource
  , initWithSource
  , nodeWithSource_a_bSelector
  , initWithSource_a_bSelector
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

-- | @+ nodeWithSource:a:b:@
nodeWithSource_a_b :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUNNode)
nodeWithSource_a_b sourceNode a b =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:a:b:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= retainedObject . castPtr

-- | @- initWithSource:a:b:@
initWithSource_a_b :: (IsMPSCNNNeuronReLUNNode mpscnnNeuronReLUNNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNNode -> sourceNode -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUNNode)
initWithSource_a_b mpscnnNeuronReLUNNode  sourceNode a b =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronReLUNNode (mkSelector "initWithSource:a:b:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= ownedObject . castPtr

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronReLUNNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronReLUNNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronReLUNNode mpscnnNeuronReLUNNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronReLUNNode -> sourceNode -> IO (Id MPSCNNNeuronReLUNNode)
initWithSource mpscnnNeuronReLUNNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronReLUNNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:@
nodeWithSource_a_bSelector :: Selector
nodeWithSource_a_bSelector = mkSelector "nodeWithSource:a:b:"

-- | @Selector@ for @initWithSource:a:b:@
initWithSource_a_bSelector :: Selector
initWithSource_a_bSelector = mkSelector "initWithSource:a:b:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

