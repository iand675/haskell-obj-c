{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNInstanceNormalizationGradient
--
-- This depends on Metal.framework
--
-- This kernel executes a gradient pass corresponding to MPSCNNInstanceNormalization.
--
-- Generated bindings for @MPSCNNInstanceNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNInstanceNormalizationGradient
  ( MPSCNNInstanceNormalizationGradient
  , IsMPSCNNInstanceNormalizationGradient(..)


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

