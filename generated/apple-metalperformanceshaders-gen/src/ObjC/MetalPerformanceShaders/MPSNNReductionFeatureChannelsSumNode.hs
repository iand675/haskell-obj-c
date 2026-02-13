{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNReductionFeatureChannelsSumNode@.
module ObjC.MetalPerformanceShaders.MPSNNReductionFeatureChannelsSumNode
  ( MPSNNReductionFeatureChannelsSumNode
  , IsMPSNNReductionFeatureChannelsSumNode(..)
  , weight
  , setWeight
  , setWeightSelector
  , weightSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A scale factor to apply to each feature channel sum.
--
-- ObjC selector: @- weight@
weight :: IsMPSNNReductionFeatureChannelsSumNode mpsnnReductionFeatureChannelsSumNode => mpsnnReductionFeatureChannelsSumNode -> IO CFloat
weight mpsnnReductionFeatureChannelsSumNode =
  sendMessage mpsnnReductionFeatureChannelsSumNode weightSelector

-- | A scale factor to apply to each feature channel sum.
--
-- ObjC selector: @- setWeight:@
setWeight :: IsMPSNNReductionFeatureChannelsSumNode mpsnnReductionFeatureChannelsSumNode => mpsnnReductionFeatureChannelsSumNode -> CFloat -> IO ()
setWeight mpsnnReductionFeatureChannelsSumNode value =
  sendMessage mpsnnReductionFeatureChannelsSumNode setWeightSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector '[CFloat] ()
setWeightSelector = mkSelector "setWeight:"

