{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A state created to record a MPSNDArrayGather kernel properties
--
-- at the time an -encode call was made.
--
-- Must be created with the appropriate MPSNDArray kernel method, for example:
--
-- MPSNDArrayGather* gather = [[MPSNDArrayGather alloc] initWithDevice: device];          MPSNDArrayGatherGradientState* state = [gather resultStateForSourceArrays:...];
--
-- Generated bindings for @MPSNDArrayGatherGradientState@.
module ObjC.MetalPerformanceShaders.MPSNDArrayGatherGradientState
  ( MPSNDArrayGatherGradientState
  , IsMPSNDArrayGatherGradientState(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

