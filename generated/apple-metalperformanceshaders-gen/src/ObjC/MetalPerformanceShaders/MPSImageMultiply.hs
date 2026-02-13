{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageMultiply
--
-- This depends on Metal.framework.
--
-- Specifies the multiplication operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) * (secondaryScale * y)) + bias.
--
-- Generated bindings for @MPSImageMultiply@.
module ObjC.MetalPerformanceShaders.MPSImageMultiply
  ( MPSImageMultiply
  , IsMPSImageMultiply(..)
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

-- | Initialize the multiplication operator
--
-- @device@ — The device the filter will run on.
--
-- Returns: A valid MPSImageMultiply object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageMultiply mpsImageMultiply => mpsImageMultiply -> RawId -> IO (Id MPSImageMultiply)
initWithDevice mpsImageMultiply device =
  sendOwnedMessage mpsImageMultiply initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageMultiply)
initWithDeviceSelector = mkSelector "initWithDevice:"

