{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNArithmeticGradientState
--
-- This depends on Metal.framework.
--
-- The MPSCNNArithmeticGradientState is used to hold the clamp mask used by both              MPSCNNArithmetic forward filter and MPSCNNArithmeticGradient backward filter.              The MPSCNNArithmetic forward filter populates the MPSCNNArithmeticGradientState              object and the MPSCNNArithmeticGradient backward filter consumes the state              object.
--
-- The clamp mask is stored internally and is not accessible by the user.
--
-- Generated bindings for @MPSCNNArithmeticGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNArithmeticGradientState
  ( MPSCNNArithmeticGradientState
  , IsMPSCNNArithmeticGradientState(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSCNNArithmeticGradientState mpscnnArithmeticGradientState => mpscnnArithmeticGradientState -> IO (Id MPSCNNArithmeticGradientState)
init_ mpscnnArithmeticGradientState =
  sendOwnedMessage mpscnnArithmeticGradientState initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNArithmeticGradientState)
initSelector = mkSelector "init"

