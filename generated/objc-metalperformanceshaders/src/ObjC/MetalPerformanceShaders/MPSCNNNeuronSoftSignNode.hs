{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node representing a MPSCNNNeuronSoftSign kernel
--
-- For each pixel, applies the following function:
--
-- f(x) = x / (1 + abs(x))
--
-- Generated bindings for @MPSCNNNeuronSoftSignNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSoftSignNode
  ( MPSCNNNeuronSoftSignNode
  , IsMPSCNNNeuronSoftSignNode(..)
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

-- | Create an autoreleased node with default values for parameters a & b
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNeuronSoftSignNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNeuronSoftSignNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node with default values for parameters a & b
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSCNNNeuronSoftSignNode mpscnnNeuronSoftSignNode, IsMPSNNImageNode sourceNode) => mpscnnNeuronSoftSignNode -> sourceNode -> IO (Id MPSCNNNeuronSoftSignNode)
initWithSource mpscnnNeuronSoftSignNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNeuronSoftSignNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

