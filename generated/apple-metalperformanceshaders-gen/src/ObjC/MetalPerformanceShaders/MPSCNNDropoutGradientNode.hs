{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNDropoutGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNDropoutGradientNode
  ( MPSCNNDropoutGradientNode
  , IsMPSCNNDropoutGradientNode(..)
  , keepProbability
  , seed
  , keepProbabilitySelector
  , seedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- keepProbability@
keepProbability :: IsMPSCNNDropoutGradientNode mpscnnDropoutGradientNode => mpscnnDropoutGradientNode -> IO CFloat
keepProbability mpscnnDropoutGradientNode =
  sendMessage mpscnnDropoutGradientNode keepProbabilitySelector

-- | @- seed@
seed :: IsMPSCNNDropoutGradientNode mpscnnDropoutGradientNode => mpscnnDropoutGradientNode -> IO CULong
seed mpscnnDropoutGradientNode =
  sendMessage mpscnnDropoutGradientNode seedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector '[] CFloat
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

