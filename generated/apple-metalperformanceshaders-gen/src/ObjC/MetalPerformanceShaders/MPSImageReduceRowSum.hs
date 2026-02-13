{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceRowSum
--
-- The MPSImageReduceRowSum performs a reduction operation returning the sum for each row of an image
--
-- Generated bindings for @MPSImageReduceRowSum@.
module ObjC.MetalPerformanceShaders.MPSImageReduceRowSum
  ( MPSImageReduceRowSum
  , IsMPSImageReduceRowSum(..)
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
initWithDevice :: IsMPSImageReduceRowSum mpsImageReduceRowSum => mpsImageReduceRowSum -> RawId -> IO (Id MPSImageReduceRowSum)
initWithDevice mpsImageReduceRowSum device =
  sendOwnedMessage mpsImageReduceRowSum initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceRowSum)
initWithDeviceSelector = mkSelector "initWithDevice:"

