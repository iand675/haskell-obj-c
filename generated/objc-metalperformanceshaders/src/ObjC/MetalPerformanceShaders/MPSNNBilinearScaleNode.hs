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

