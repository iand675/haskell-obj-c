{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A state created to record a MPSCNNKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage:]          or analogous interfaces.
--
-- Generated bindings for @MPSNNGradientState@.
module ObjC.MetalPerformanceShaders.MPSNNGradientState
  ( MPSNNGradientState
  , IsMPSNNGradientState(..)


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

