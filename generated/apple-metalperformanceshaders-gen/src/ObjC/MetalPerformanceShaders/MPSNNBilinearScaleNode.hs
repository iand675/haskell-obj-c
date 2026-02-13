{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNScale object that uses bilinear interpolation for resampling
--
-- Caution: bilinear downscaling by more than a factor of                    two in any dimension causes loss of information if a                    low pass filter is not run over the image first. Details                    may be omitted.
--
-- Generated bindings for @MPSNNBilinearScaleNode@.
module ObjC.MetalPerformanceShaders.MPSNNBilinearScaleNode
  ( MPSNNBilinearScaleNode
  , IsMPSNNBilinearScaleNode(..)


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

