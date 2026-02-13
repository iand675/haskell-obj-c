{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNSubtract
--
-- This depends on Metal.framework.
--
-- Specifies the subtraction operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) - (secondaryScale * y)) + bias.
--
-- Generated bindings for @MPSCNNSubtract@.
module ObjC.MetalPerformanceShaders.MPSCNNSubtract
  ( MPSCNNSubtract
  , IsMPSCNNSubtract(..)
  , initWithDevice
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the subtraction operator
--
-- @device@ — The device the filter will run on.
--
-- Returns: A valid MPSCNNSubtract object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNSubtract mpscnnSubtract => mpscnnSubtract -> RawId -> IO (Id MPSCNNSubtract)
initWithDevice mpscnnSubtract device =
  sendOwnedMessage mpscnnSubtract initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNSubtract)
initWithDeviceSelector = mkSelector "initWithDevice:"

