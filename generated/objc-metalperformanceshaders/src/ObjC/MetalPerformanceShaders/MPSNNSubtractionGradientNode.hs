{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | returns gradient for either primary or secondary source image from the inference pass.  Use the isSecondarySourceFilter property to indicate whether this filter is computing the gradient  for the primary or secondary source image from the inference pass.
--
-- Generated bindings for @MPSNNSubtractionGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNSubtractionGradientNode
  ( MPSNNSubtractionGradientNode
  , IsMPSNNSubtractionGradientNode(..)


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

