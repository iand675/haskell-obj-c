{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayGather
--
-- This depends on Metal.framework.
--
-- Applies a gather operation along a given axis.  The encoded primary source array              contains the data and the secondary array is a 1-D MPSNDArray containing the              indices.
--
-- For each dimension other than axis                      result[i] = source[i]; 0 <= i < array slice length along dimension                  Along the specified axis                      result[i] = source[indices[i]]; 0 <= i < number of indices
--
-- Generated bindings for @MPSNDArrayGather@.
module ObjC.MetalPerformanceShaders.MPSNDArrayGather
  ( MPSNDArrayGather
  , IsMPSNDArrayGather(..)
  , axis
  , setAxis
  , axisSelector
  , setAxisSelector


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

-- | axis
--
-- The axis along which to apply the gather operation.              Defaults to zero.
--
-- ObjC selector: @- axis@
axis :: IsMPSNDArrayGather mpsndArrayGather => mpsndArrayGather -> IO CULong
axis mpsndArrayGather  =
  sendMsg mpsndArrayGather (mkSelector "axis") retCULong []

-- | axis
--
-- The axis along which to apply the gather operation.              Defaults to zero.
--
-- ObjC selector: @- setAxis:@
setAxis :: IsMPSNDArrayGather mpsndArrayGather => mpsndArrayGather -> CULong -> IO ()
setAxis mpsndArrayGather  value =
  sendMsg mpsndArrayGather (mkSelector "setAxis:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @axis@
axisSelector :: Selector
axisSelector = mkSelector "axis"

-- | @Selector@ for @setAxis:@
setAxisSelector :: Selector
setAxisSelector = mkSelector "setAxis:"

