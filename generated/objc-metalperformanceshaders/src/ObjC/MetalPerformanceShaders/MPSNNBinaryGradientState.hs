{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A state created to record MPSCNNBinaryKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNBinaryKernel resultStateForPrimaryImage:secondaryImage:sourceStates:destinationImage:]          or analogous interfaces.
--
-- Generated bindings for @MPSNNBinaryGradientState@.
module ObjC.MetalPerformanceShaders.MPSNNBinaryGradientState
  ( MPSNNBinaryGradientState
  , IsMPSNNBinaryGradientState(..)


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

