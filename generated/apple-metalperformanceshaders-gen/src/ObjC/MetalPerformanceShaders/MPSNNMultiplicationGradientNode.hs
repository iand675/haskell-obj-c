{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | returns gradient for either primary or secondary source image from the inference pass.  Use the isSecondarySourceFilter property to indicate whether this filter is computing the gradient  for the primary or secondary source image from the inference pass.
--
-- Generated bindings for @MPSNNMultiplicationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNMultiplicationGradientNode
  ( MPSNNMultiplicationGradientNode
  , IsMPSNNMultiplicationGradientNode(..)


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

