{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceColumnSum
--
-- The MPSImageReduceColumnSum performs a reduction operation returning the sum for each column of an image
--
-- Generated bindings for @MPSImageReduceColumnSum@.
module ObjC.MetalPerformanceShaders.MPSImageReduceColumnSum
  ( MPSImageReduceColumnSum
  , IsMPSImageReduceColumnSum(..)
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
initWithDevice :: IsMPSImageReduceColumnSum mpsImageReduceColumnSum => mpsImageReduceColumnSum -> RawId -> IO (Id MPSImageReduceColumnSum)
initWithDevice mpsImageReduceColumnSum device =
  sendOwnedMessage mpsImageReduceColumnSum initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceColumnSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

