{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDStridedSliceGradient
--
-- This depends on Metal.framework.
--
-- Perform the gradient operation corresponding to a strided slice.
--
-- Generated bindings for @MPSNDArrayStridedSliceGradient@.
module ObjC.MetalPerformanceShaders.MPSNDArrayStridedSliceGradient
  ( MPSNDArrayStridedSliceGradient
  , IsMPSNDArrayStridedSliceGradient(..)


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

