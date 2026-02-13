{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceColumnMin
--
-- The MPSImageReduceColumnMin performs a reduction operation returning the mininmum value for each column of an image
--
-- Generated bindings for @MPSImageReduceColumnMin@.
module ObjC.MetalPerformanceShaders.MPSImageReduceColumnMin
  ( MPSImageReduceColumnMin
  , IsMPSImageReduceColumnMin(..)
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
initWithDevice :: IsMPSImageReduceColumnMin mpsImageReduceColumnMin => mpsImageReduceColumnMin -> RawId -> IO (Id MPSImageReduceColumnMin)
initWithDevice mpsImageReduceColumnMin device =
  sendOwnedMessage mpsImageReduceColumnMin initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceColumnMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

