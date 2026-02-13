{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceRowMin
--
-- The MPSImageReduceRowMin performs a reduction operation returning the mininmum value for each row of an image
--
-- Generated bindings for @MPSImageReduceRowMin@.
module ObjC.MetalPerformanceShaders.MPSImageReduceRowMin
  ( MPSImageReduceRowMin
  , IsMPSImageReduceRowMin(..)
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
initWithDevice :: IsMPSImageReduceRowMin mpsImageReduceRowMin => mpsImageReduceRowMin -> RawId -> IO (Id MPSImageReduceRowMin)
initWithDevice mpsImageReduceRowMin device =
  sendOwnedMessage mpsImageReduceRowMin initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceRowMin)
initWithDeviceSelector = mkSelector "initWithDevice:"

