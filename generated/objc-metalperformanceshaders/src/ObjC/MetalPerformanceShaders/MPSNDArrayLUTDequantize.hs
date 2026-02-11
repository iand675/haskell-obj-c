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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayLUTDequantize mpsndArrayLUTDequantize => mpsndArrayLUTDequantize -> RawId -> IO (Id MPSNDArrayLUTDequantize)
initWithDevice mpsndArrayLUTDequantize  device =
  sendMsg mpsndArrayLUTDequantize (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayLUTDequantize mpsndArrayLUTDequantize => mpsndArrayLUTDequantize -> RawId -> CULong -> IO (Id MPSNDArrayLUTDequantize)
initWithDevice_sourceCount mpsndArrayLUTDequantize  device sourceCount =
  sendMsg mpsndArrayLUTDequantize (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral sourceCount)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

