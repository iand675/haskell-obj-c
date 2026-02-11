{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronELU kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = a * exp(x) - 1, x <  0
-- x             , x >= 0
--
-- Generated bindings for @MPSCNNNeuronELUNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronELUNode
  ( MPSCNNNeuronELUNode
  , IsMPSCNNNeuronELUNode(..)
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
nodeWithSource_a :: IsMPSNNImageNode sourceNode => sourceNode -> CFloat -> IO (Id MPSCNNNeuronELUNode)
nodeWithSource_a sourceNode a =
  do
    cls' <- getRequiredClass "MPSCNNNeuronELUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:a:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronELUNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronELUNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronELUNode mpscnnNeuronELUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronELUNode -> sourceNode -> IO (Id MPSCNNNeuronELUNode)
initWithSource mpscnnNeuronELUNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronELUNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSource:a:@
initWithSource_a :: (IsMPSCNNNeuronELUNode mpscnnNeuronELUNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronELUNode -> sourceNode -> CFloat -> IO (Id MPSCNNNeuronELUNode)
initWithSource_a mpscnnNeuronELUNode  sourceNode a =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronELUNode (mkSelector "initWithSource:a:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCFloat (fromIntegral a)] >>= ownedObject . castPtr

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

