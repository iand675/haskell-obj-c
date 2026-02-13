{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNScale object that uses the Lanczos resampling filter
--
-- This method does not require a low pass filter for downsampling                    by more than a factor of two. Caution: may cause ringing, which                    could prove distracting to a neural network unused to seeing it.                    You should use the resampling method that was used to train the                    network.
--
-- Generated bindings for @MPSNNLanczosScaleNode@.
module ObjC.MetalPerformanceShaders.MPSNNLanczosScaleNode
  ( MPSNNLanczosScaleNode
  , IsMPSNNLanczosScaleNode(..)


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

