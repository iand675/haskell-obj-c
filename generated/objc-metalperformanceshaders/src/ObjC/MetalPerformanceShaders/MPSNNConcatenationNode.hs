{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a the concatenation (in the feature channel dimension) of the results from one or more kernels
--
-- Generated bindings for @MPSNNConcatenationNode@.
module ObjC.MetalPerformanceShaders.MPSNNConcatenationNode
  ( MPSNNConcatenationNode
  , IsMPSNNConcatenationNode(..)
  , nodeWithSources
  , initWithSources
  , gradientFilterWithSources
  , nodeWithSourcesSelector
  , initWithSourcesSelector
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

-- | Init a autoreleased node that concatenates feature channels from multiple images
--
-- In some neural network designs, it is necessary to append feature channels              from one neural network filter to the results of another. If we have three              image nodes with M, N and O feature channels in them, passed to -initWithSources:              as \@[imageM, imageN, imageO], then feature channels [0,M-1] will be drawn from              image M,  feature channels [M, M+N-1] will be drawn from image N and feature channels              [M+N, M+N+O-1] will be drawn from image O.
--
-- As all images are padded out to a multiple of four feature channels,              M, N and O here are also multiples of four, even when the MPSImages              are not. That is, if the image is 23 feature channels and one channel              of padding, it takes up 24 feature channels worth of space in the              concatenated result.
--
-- Performance Note:  Generally, concatenation is free as long as all              of the sourceNodes are produced by filters in the same MPSNNGraph.              Most MPSCNNKernels have the ability to write their results  at a              feature channel offset within a target MPSImage. However, if the              MPSNNImageNode source nodes come from images external to the MPSNNGraph,              then we have to do a copy operation to assemble the concatenated node.              As a result, when deciding where to break a large logical graph into              multiple smaller MPSNNGraphs, it is better for concatenations to              appear at the ends of subgraphs when possible rather than at the start,              to the extent that all the images used in the concatenation are              produced by that subgraph.
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter
--
-- Returns: A new MPSNNFilter node that concatenates its inputs.
--
-- ObjC selector: @+ nodeWithSources:@
nodeWithSources :: IsNSArray sourceNodes => sourceNodes -> IO (Id MPSNNConcatenationNode)
nodeWithSources sourceNodes =
  do
    cls' <- getRequiredClass "MPSNNConcatenationNode"
    withObjCPtr sourceNodes $ \raw_sourceNodes ->
      sendClassMsg cls' (mkSelector "nodeWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ())] >>= retainedObject . castPtr

-- | Init a node that concatenates feature channels from multiple images
--
-- In some neural network designs, it is necessary to append feature channels              from one neural network filter to the results of another. If we have three              image nodes with M, N and O feature channels in them, passed to -initWithSources:              as \@[imageM, imageN, imageO], then feature channels [0,M-1] will be drawn from              image M,  feature channels [M, M+N-1] will be drawn from image N and feature channels              [M+N, M+N+O-1] will be drawn from image O.
--
-- As all images are padded out to a multiple of four feature channels,              M, N and O here are also multiples of four, even when the MPSImages              are not. That is, if the image is 23 feature channels and one channel              of padding, it takes up 24 feature channels worth of space in the              concatenated result.
--
-- Performance Note:  Generally, concatenation is free as long as all              of the sourceNodes are produced by filters in the same MPSNNGraph.              Most MPSCNNKernels have the ability to write their results  at a              feature channel offset within a target MPSImage. However, if the              MPSNNImageNode source nodes come from images external to the MPSNNGraph,              then we have to do a copy operation to assemble the concatenated node.               As a result, when deciding where to break a large logical graph into               multiple smaller MPSNNGraphs, it is better for concatenations to               appear at the ends of subgraphs when possible rather than at the start,              to the extent that all the images used in the concatenation are               produced by that subgraph.
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter
--
-- Returns: A new MPSNNFilter node that concatenates its inputs.
--
-- ObjC selector: @- initWithSources:@
initWithSources :: (IsMPSNNConcatenationNode mpsnnConcatenationNode, IsNSArray sourceNodes) => mpsnnConcatenationNode -> sourceNodes -> IO (Id MPSNNConcatenationNode)
initWithSources mpsnnConcatenationNode  sourceNodes =
withObjCPtr sourceNodes $ \raw_sourceNodes ->
    sendMsg mpsnnConcatenationNode (mkSelector "initWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ())] >>= ownedObject . castPtr

-- | Concatenation returns multiple gradient filters. Use -gradientFiltersWithSources: instead.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNConcatenationNode mpsnnConcatenationNode, IsNSArray gradientImages) => mpsnnConcatenationNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnConcatenationNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpsnnConcatenationNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSources:@
nodeWithSourcesSelector :: Selector
nodeWithSourcesSelector = mkSelector "nodeWithSources:"

-- | @Selector@ for @initWithSources:@
initWithSourcesSelector :: Selector
initWithSourcesSelector = mkSelector "initWithSources:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

