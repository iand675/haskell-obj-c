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

-- | @- keepProbability@
keepProbability :: IsMPSCNNDropoutGradientNode mpscnnDropoutGradientNode => mpscnnDropoutGradientNode -> IO CFloat
keepProbability mpscnnDropoutGradientNode  =
  sendMsg mpscnnDropoutGradientNode (mkSelector "keepProbability") retCFloat []

-- | @- seed@
seed :: IsMPSCNNDropoutGradientNode mpscnnDropoutGradientNode => mpscnnDropoutGradientNode -> IO CULong
seed mpscnnDropoutGradientNode  =
  sendMsg mpscnnDropoutGradientNode (mkSelector "seed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

