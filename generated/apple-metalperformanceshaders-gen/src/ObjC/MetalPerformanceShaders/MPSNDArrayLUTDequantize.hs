{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayLUTDequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes a lookup-table based NDArray.
--
-- The kernel works with 2 inputs: 1) The quantized input, 2) The LookUp table array.
--
-- Generated bindings for @MPSNDArrayLUTDequantize@.
module ObjC.MetalPerformanceShaders.MPSNDArrayLUTDequantize
  ( MPSNDArrayLUTDequantize
  , IsMPSNDArrayLUTDequantize(..)
  , initWithDevice
  , initWithDevice_sourceCount
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayLUTDequantize mpsndArrayLUTDequantize => mpsndArrayLUTDequantize -> RawId -> IO (Id MPSNDArrayLUTDequantize)
initWithDevice mpsndArrayLUTDequantize device =
  sendOwnedMessage mpsndArrayLUTDequantize initWithDeviceSelector device

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayLUTDequantize mpsndArrayLUTDequantize => mpsndArrayLUTDequantize -> RawId -> CULong -> IO (Id MPSNDArrayLUTDequantize)
initWithDevice_sourceCount mpsndArrayLUTDequantize device sourceCount =
  sendOwnedMessage mpsndArrayLUTDequantize initWithDevice_sourceCountSelector device sourceCount

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayLUTDequantize)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayLUTDequantize)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

