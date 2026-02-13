{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayGatherGradient
--
-- This depends on Metal.framework.
--
-- Applies the gradient operation corresponding to a forward gather operation.
--
-- Generated bindings for @MPSNDArrayGatherGradient@.
module ObjC.MetalPerformanceShaders.MPSNDArrayGatherGradient
  ( MPSNDArrayGatherGradient
  , IsMPSNDArrayGatherGradient(..)


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

