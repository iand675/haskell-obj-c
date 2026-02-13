{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNMatrixTrainingState
--
-- This depends on Metal.framework
--
-- This class holds the data that is passed from the forward pass needed in the backward pass.
--
-- Generated bindings for @MPSRNNMatrixTrainingState@.
module ObjC.MetalPerformanceShaders.MPSRNNMatrixTrainingState
  ( MPSRNNMatrixTrainingState
  , IsMPSRNNMatrixTrainingState(..)


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

