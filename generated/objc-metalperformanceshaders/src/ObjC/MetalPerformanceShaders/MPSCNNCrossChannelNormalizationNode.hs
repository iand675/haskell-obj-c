{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing MPSCNNCrossChannelNormalization
--
-- The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,               where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where               N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 5.0f
-- delta = 1.0f
-- kernelHeight = kernelWidth = kernelSize
--
-- Generated bindings for @MPSCNNCrossChannelNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNCrossChannelNormalizationNode
  ( MPSCNNCrossChannelNormalizationNode
  , IsMPSCNNCrossChannelNormalizationNode(..)
  , nodeWithSource_kernelSize
  , initWithSource_kernelSize
  , initWithSource
  , kernelSizeInFeatureChannels
  , setKernelSizeInFeatureChannels
  , nodeWithSource_kernelSizeSelector
  , initWithSource_kernelSizeSelector
  , initWithSourceSelector
  , kernelSizeInFeatureChannelsSelector
  , setKernelSizeInFeatureChannelsSelector


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

-- | @+ nodeWithSource:kernelSize:@
nodeWithSource_kernelSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNCrossChannelNormalizationNode)
nodeWithSource_kernelSize sourceNode kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNCrossChannelNormalizationNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= retainedObject . castPtr

-- | @- initWithSource:kernelSize:@
initWithSource_kernelSize :: (IsMPSCNNCrossChannelNormalizationNode mpscnnCrossChannelNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnCrossChannelNormalizationNode -> sourceNode -> CULong -> IO (Id MPSCNNCrossChannelNormalizationNode)
initWithSource_kernelSize mpscnnCrossChannelNormalizationNode  sourceNode kernelSize =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnCrossChannelNormalizationNode (mkSelector "initWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNCrossChannelNormalizationNode mpscnnCrossChannelNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnCrossChannelNormalizationNode -> sourceNode -> IO (Id MPSCNNCrossChannelNormalizationNode)
initWithSource mpscnnCrossChannelNormalizationNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnCrossChannelNormalizationNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | @- kernelSizeInFeatureChannels@
kernelSizeInFeatureChannels :: IsMPSCNNCrossChannelNormalizationNode mpscnnCrossChannelNormalizationNode => mpscnnCrossChannelNormalizationNode -> IO CULong
kernelSizeInFeatureChannels mpscnnCrossChannelNormalizationNode  =
  sendMsg mpscnnCrossChannelNormalizationNode (mkSelector "kernelSizeInFeatureChannels") retCULong []

-- | @- setKernelSizeInFeatureChannels:@
setKernelSizeInFeatureChannels :: IsMPSCNNCrossChannelNormalizationNode mpscnnCrossChannelNormalizationNode => mpscnnCrossChannelNormalizationNode -> CULong -> IO ()
setKernelSizeInFeatureChannels mpscnnCrossChannelNormalizationNode  value =
  sendMsg mpscnnCrossChannelNormalizationNode (mkSelector "setKernelSizeInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:kernelSize:@
nodeWithSource_kernelSizeSelector :: Selector
nodeWithSource_kernelSizeSelector = mkSelector "nodeWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:kernelSize:@
initWithSource_kernelSizeSelector :: Selector
initWithSource_kernelSizeSelector = mkSelector "initWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @kernelSizeInFeatureChannels@
kernelSizeInFeatureChannelsSelector :: Selector
kernelSizeInFeatureChannelsSelector = mkSelector "kernelSizeInFeatureChannels"

-- | @Selector@ for @setKernelSizeInFeatureChannels:@
setKernelSizeInFeatureChannelsSelector :: Selector
setKernelSizeInFeatureChannelsSelector = mkSelector "setKernelSizeInFeatureChannels:"

