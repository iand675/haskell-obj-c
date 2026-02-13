{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A state created to record a MPSCNNKernel properties
--
-- at the time an -encode call was made. The contents are opaque.
--
-- Gradient states must be created with [MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage:]          or analogous interfaces.
--
-- Generated bindings for @MPSNDArrayGradientState@.
module ObjC.MetalPerformanceShaders.MPSNDArrayGradientState
  ( MPSNDArrayGradientState
  , IsMPSNDArrayGradientState(..)


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

