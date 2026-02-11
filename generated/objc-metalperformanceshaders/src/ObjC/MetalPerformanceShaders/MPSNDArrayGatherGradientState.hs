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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

