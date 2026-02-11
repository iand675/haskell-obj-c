{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceRowMax
--
-- The MPSImageReduceRowMax performs a reduction operation returning the maximum value for each row of an image
--
-- Generated bindings for @MPSImageReduceRowMax@.
module ObjC.MetalPerformanceShaders.MPSImageReduceRowMax
  ( MPSImageReduceRowMax
  , IsMPSImageReduceRowMax(..)
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
initWithDevice :: IsMPSImageReduceRowMax mpsImageReduceRowMax => mpsImageReduceRowMax -> RawId -> IO (Id MPSImageReduceRowMax)
initWithDevice mpsImageReduceRowMax  device =
  sendMsg mpsImageReduceRowMax (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

