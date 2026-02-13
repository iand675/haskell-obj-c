{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNReshapeGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNReshapeGradientNode
  ( MPSNNReshapeGradientNode
  , IsMPSNNReshapeGradientNode(..)
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

-- | A node to represent the gradient of a reshape node.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward reshape node.
--
-- Returns: A MPSNNReshapeGradientNode
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientState :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNReshapeGradientNode)
nodeWithSourceGradient_sourceImage_gradientState sourceGradient sourceImage gradientState =
  do
    cls' <- getRequiredClass "MPSNNReshapeGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- | A node to represent the gradient of a reshape node.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward reshape node.
--
-- Returns: A MPSCNNConvolutionGradientNode
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientState :: (IsMPSNNReshapeGradientNode mpsnnReshapeGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpsnnReshapeGradientNode -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSNNReshapeGradientNode)
initWithSourceGradient_sourceImage_gradientState mpsnnReshapeGradientNode sourceGradient sourceImage gradientState =
  sendOwnedMessage mpsnnReshapeGradientNode initWithSourceGradient_sourceImage_gradientStateSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:@
nodeWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNReshapeGradientNode)
nodeWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:@
initWithSourceGradient_sourceImage_gradientStateSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode] (Id MPSNNReshapeGradientNode)
initWithSourceGradient_sourceImage_gradientStateSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:"

