{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNReductionFeatureChannelsSumNode@.
module ObjC.MetalPerformanceShaders.MPSNNReductionFeatureChannelsSumNode
  ( MPSNNReductionFeatureChannelsSumNode
  , IsMPSNNReductionFeatureChannelsSumNode(..)
  , weight
  , setWeight
  , weightSelector
  , setWeightSelector


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

-- | A scale factor to apply to each feature channel sum.
--
-- ObjC selector: @- weight@
weight :: IsMPSNNReductionFeatureChannelsSumNode mpsnnReductionFeatureChannelsSumNode => mpsnnReductionFeatureChannelsSumNode -> IO CFloat
weight mpsnnReductionFeatureChannelsSumNode  =
  sendMsg mpsnnReductionFeatureChannelsSumNode (mkSelector "weight") retCFloat []

-- | A scale factor to apply to each feature channel sum.
--
-- ObjC selector: @- setWeight:@
setWeight :: IsMPSNNReductionFeatureChannelsSumNode mpsnnReductionFeatureChannelsSumNode => mpsnnReductionFeatureChannelsSumNode -> CFloat -> IO ()
setWeight mpsnnReductionFeatureChannelsSumNode  value =
  sendMsg mpsnnReductionFeatureChannelsSumNode (mkSelector "setWeight:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector
setWeightSelector = mkSelector "setWeight:"

