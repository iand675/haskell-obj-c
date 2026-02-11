{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronExponential kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = c ^ (a * x + b)
--
-- Generated bindings for @MPSCNNNeuronExponentialNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronExponentialNode
  ( MPSCNNNeuronExponentialNode
  , IsMPSCNNNeuronExponentialNode(..)
  , nodeWithSource_a_b_c
  , initWithSource_a_b_c
  , nodeWithSource
  , initWithSource
  , nodeWithSource_a_b_cSelector
  , initWithSource_a_b_cSelector
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

-- | @+ nodeWithSource:a:b:c:@
nodeWithSource_a_b_c :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronExponentialNode)
nodeWithSource_a_b_c sourceNode a b c =
  do
    cls' <- getRequiredClass "MPSCNNNeuronExponentialNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:a:b:c:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b), argCFloat (fromIntegral c)] >>= retainedObject . castPtr

-- | Init a node representing a MPSCNNNeuronExponential kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = c ^ (a * x + b)
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @a@ — See discussion above.
--
-- @b@ — See discussion above.
--
-- @c@ — See discussion above.
--
-- Returns: A new MPSNNFilter node for a MPSCNNNeuronExponential kernel.
--
-- ObjC selector: @- initWithSource:a:b:c:@
initWithSource_a_b_c :: (IsMPSCNNNeuronExponentialNode mpscnnNeuronExponentialNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronExponentialNode -> sourceNode -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronExponentialNode)
initWithSource_a_b_c mpscnnNeuronExponentialNode  sourceNode a b c =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronExponentialNode (mkSelector "initWithSource:a:b:c:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b), argCFloat (fromIntegral c)] >>= ownedObject . castPtr

-- | Create an autoreleased node with default values for parameters a, b, and c
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronExponentialNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronExponentialNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node with default values for parameters a, b, and c
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronExponentialNode mpscnnNeuronExponentialNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronExponentialNode -> sourceNode -> IO (Id MPSCNNNeuronExponentialNode)
initWithSource mpscnnNeuronExponentialNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronExponentialNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:a:b:c:@
nodeWithSource_a_b_cSelector :: Selector
nodeWithSource_a_b_cSelector = mkSelector "nodeWithSource:a:b:c:"

-- | @Selector@ for @initWithSource:a:b:c:@
initWithSource_a_b_cSelector :: Selector
initWithSource_a_b_cSelector = mkSelector "initWithSource:a:b:c:"

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

