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

