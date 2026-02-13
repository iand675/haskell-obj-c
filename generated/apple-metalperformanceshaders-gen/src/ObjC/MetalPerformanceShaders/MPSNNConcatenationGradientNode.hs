{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNConcatenationGradientNode
--
-- A MPSNNSlice filter that operates as the conjugate computation for concatentation operators during training
--
-- As concatenation is formally just a copy and not a computation, there isn't a lot of arithmetic for              the slice operator to do, but we still need to extract out the relevant portion              of the gradient of the input signal that went into the corresponding concatenation              destination image.
--
-- Generated bindings for @MPSNNConcatenationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNConcatenationGradientNode
  ( MPSNNConcatenationGradientNode
  , IsMPSNNConcatenationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientState
  , initWithSourceGradient_sourceImage_gradientStateSelector
  , nodeWithSourceGradient_sourceImage_gradientStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | create a MPSNNConcatenationGradientNode
--
-- Generally you should use [MPSNNConcatenationNode gradientFiltersWithSources:] instead.
--
-- @gradientSourceNode@ — The gradient image functioning as input for the operator
--
-- @sourceImage@ — The particular input image to the concatentation, if any, that the slice corresponds with
--
-- @gradientState@ — The gradient state produced by the concatenation filter, consumed by this filter
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode gradientSourceNode, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => gradientSourceNode -> sourceImage -> gradientState -> IO (Id MPSNNConcatenationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState gradientSourceNode sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSNNConcatenationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode gradientSourceNode) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | Init a MPSNNConcatenationGradientNode
--
-- Generally you should use [MPSNNConcatenationNode gradientFiltersWithSources:] instead.
--
-- @gradientSourceNode@ — The gradient image functioning as input for the operator
--
-- @sourceImage@ — The particular input image to the concatentation, if any, that the slice corresponds with
--
-- @gradientState@ — The gradient state produced by the concatenation filter, consumed by this filter
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSNNConcatenationGradientNode mpsnnConcatenationGradientNode, IsMPSNNImageNode gradientSourceNode, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnConcatenationGradientNode -> gradientSourceNode -> sourceImage -> gradientState -> IO (Id MPSNNConcatenationGradientNode)
initWithSourceGradient_sourceImage_gradientState mpsnnConcatenationGradientNode gradientSourceNode sourceImage gradientState =
  sendOwnedMessage mpsnnConcatenationGradientNode initWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode gradientSourceNode) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNConcatenationGradientNode)
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNConcatenationGradientNode)
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

