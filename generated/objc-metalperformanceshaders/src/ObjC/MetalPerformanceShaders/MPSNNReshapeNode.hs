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
  , nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector
  , initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector


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
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCULong (fromIntegral resultWidth), argCULong (fromIntegral resultHeight), argCULong (fromIntegral resultFeatureChannels)] >>= retainedObject . castPtr

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
initWithSource_resultWidth_resultHeight_resultFeatureChannels mpsnnReshapeNode  source resultWidth resultHeight resultFeatureChannels =
withObjCPtr source $ \raw_source ->
    sendMsg mpsnnReshapeNode (mkSelector "initWithSource:resultWidth:resultHeight:resultFeatureChannels:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCULong (fromIntegral resultWidth), argCULong (fromIntegral resultHeight), argCULong (fromIntegral resultFeatureChannels)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:@
nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector :: Selector
nodeWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector = mkSelector "nodeWithSource:resultWidth:resultHeight:resultFeatureChannels:"

-- | @Selector@ for @initWithSource:resultWidth:resultHeight:resultFeatureChannels:@
initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector :: Selector
initWithSource_resultWidth_resultHeight_resultFeatureChannelsSelector = mkSelector "initWithSource:resultWidth:resultHeight:resultFeatureChannels:"

