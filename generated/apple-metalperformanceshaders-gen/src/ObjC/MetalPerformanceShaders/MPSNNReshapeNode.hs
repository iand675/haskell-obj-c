{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node for a MPSNNReshape kernel
--
-- Generated bindings for @MPSNNReshapeNode@.
module ObjC.MetalPerformanceShaders.MPSNNReshapeNode
  ( MPSNNReshapeNode
  , IsMPSNNReshapeNode(..)
  , nodeWithSource_resultWidth_resultHeight_resultFeatureChannels
  , initWithSource_resultWidth_resultHeight_resultFeatureChannels
  , initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector
  , nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init a node representing a autoreleased MPSNNReshape kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @resultWidth@ — The width of the reshaped image.
--
-- @resultHeight@ — The height of the reshaped image.
--
-- @resultFeatureChannels@ — The number of feature channels in the reshaped image.
--
-- Returns: A new MPSNNFilter node for a MPSNNReshape kernel.
--
-- ObjC selector: @+ nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:@
nodeWithSource_resultWidth_resultHeight_resultFeatureChannels :: IsMPSNNImageNode source => source -> CULong -> CULong -> CULong -> IO (Id MPSNNReshapeNode)
nodeWithSource_resultWidth_resultHeight_resultFeatureChannels source resultWidth resultHeight resultFeatureChannels =
  do
    cls' <- getRequiredClass "MPSNNReshapeNode"
    sendClassMessage cls' nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector (toMPSNNImageNode source) resultWidth resultHeight resultFeatureChannels

-- | Init a node representing a MPSNNReshape kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @resultWidth@ — The width of the reshaped image.
--
-- @resultHeight@ — The height of the reshaped image.
--
-- @resultFeatureChannels@ — The number of feature channels in the reshaped image.
--
-- Returns: A new MPSNNFilter node for a MPSNNReshape kernel.
--
-- ObjC selector: @- initWithSource:resultWidth:resultHeight:resultFeatureChannels:@
initWithSource_resultWidth_resultHeight_resultFeatureChannels :: (IsMPSNNReshapeNode mpsnnReshapeNode, IsMPSNNImageNode source) => mpsnnReshapeNode -> source -> CULong -> CULong -> CULong -> IO (Id MPSNNReshapeNode)
initWithSource_resultWidth_resultHeight_resultFeatureChannels mpsnnReshapeNode source resultWidth resultHeight resultFeatureChannels =
  sendOwnedMessage mpsnnReshapeNode initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector (toMPSNNImageNode source) resultWidth resultHeight resultFeatureChannels

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:@
nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong] (Id MPSNNReshapeNode)
nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector = mkSelector "nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:"

-- | @Selector@ for @initWithSource:resultWidth:resultHeight:resultFeatureChannels:@
initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong] (Id MPSNNReshapeNode)
initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector = mkSelector "initWithSource:resultWidth:resultHeight:resultFeatureChannels:"

