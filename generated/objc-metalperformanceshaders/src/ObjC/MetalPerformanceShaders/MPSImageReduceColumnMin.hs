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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithDevice mpsImageReduceColumnMin  device =
  sendMsg mpsImageReduceColumnMin (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

