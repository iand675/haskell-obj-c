{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceColumnMax
--
-- The MPSImageReduceColumnMax performs a reduction operation returning the maximum value for each column of an image
--
-- Generated bindings for @MPSImageReduceColumnMax@.
module ObjC.MetalPerformanceShaders.MPSImageReduceColumnMax
  ( MPSImageReduceColumnMax
  , IsMPSImageReduceColumnMax(..)
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

-- | Specifies information to apply the reduction operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageReduce object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageReduceColumnMax mpsImageReduceColumnMax => mpsImageReduceColumnMax -> RawId -> IO (Id MPSImageReduceColumnMax)
initWithDevice mpsImageReduceColumnMax device =
  sendOwnedMessage mpsImageReduceColumnMax initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceColumnMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

