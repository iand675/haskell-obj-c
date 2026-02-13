{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageSubtract
--
-- This depends on Metal.framework.
--
-- Specifies the subtraction operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) - (secondaryScale * y)) + bias.
--
-- Generated bindings for @MPSImageSubtract@.
module ObjC.MetalPerformanceShaders.MPSImageSubtract
  ( MPSImageSubtract
  , IsMPSImageSubtract(..)
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
-- Returns: A valid MPSImageSubtract object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageSubtract mpsImageSubtract => mpsImageSubtract -> RawId -> IO (Id MPSImageSubtract)
initWithDevice mpsImageSubtract device =
  sendOwnedMessage mpsImageSubtract initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageSubtract)
initWithDeviceSelector = mkSelector "initWithDevice:"

