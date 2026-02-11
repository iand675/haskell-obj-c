{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNInitialGradientNode
--
-- A node for a MPSNNInitialGradient kernel
--
-- This node can be used to generate a starting point for an arbitrary gradient computation.                  Simply add this node after the node for which you want to compute gradients and then                  call the function trainingGraphWithSourceGradient: of this node to automatically                  generate the nodes needed for gradient computations or add the desired nodes manually.                  This is generally used with MPSNNLossGradientNode and MPSNNForwardLossNode
--
-- Generated bindings for @MPSNNInitialGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNInitialGradientNode
  ( MPSNNInitialGradientNode
  , IsMPSNNInitialGradientNode(..)
  , nodeWithSource
  , initWithSource
  , gradientFilterWithSources
  , nodeWithSourceSelector
  , initWithSourceSelector
  , gradientFilterWithSourcesSelector


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

-- | Init a node representing a MPSNNInitialGradient MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSNNInitialGradient kernel.
--
-- ObjC selector: @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode source => source -> IO (Id MPSNNInitialGradientNode)
nodeWithSource source =
  do
    cls' <- getRequiredClass "MPSNNInitialGradientNode"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node representing a MPSNNInitialGradient MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- Returns: A new MPSNNFilter node for a MPSNNInitialGradient kernel.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsMPSNNInitialGradientNode mpsnnInitialGradientNode, IsMPSNNImageNode source) => mpsnnInitialGradientNode -> source -> IO (Id MPSNNInitialGradientNode)
initWithSource mpsnnInitialGradientNode  source =
withObjCPtr source $ \raw_source ->
    sendMsg mpsnnInitialGradientNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | The initial gradient filter is a gradient filter and we don't provide support for gradients of gradients currently.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNInitialGradientNode mpsnnInitialGradientNode, IsNSArray gradientImages) => mpsnnInitialGradientNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnInitialGradientNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpsnnInitialGradientNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

