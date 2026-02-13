{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDStridedSlice
--
-- This depends on Metal.framework.
--
-- Extracts a subset of the source array using the specified slice strides.
--
-- Generated bindings for @MPSNDArrayStridedSlice@.
module ObjC.MetalPerformanceShaders.MPSNDArrayStridedSlice
  ( MPSNDArrayStridedSlice
  , IsMPSNDArrayStridedSlice(..)


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

