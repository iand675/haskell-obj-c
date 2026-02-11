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

