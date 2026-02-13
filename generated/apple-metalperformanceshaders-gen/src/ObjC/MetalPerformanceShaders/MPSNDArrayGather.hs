{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
axis mpsndArrayGather =
  sendMessage mpsndArrayGather axisSelector

-- | axis
--
-- The axis along which to apply the gather operation.              Defaults to zero.
--
-- ObjC selector: @- setAxis:@
setAxis :: IsMPSNDArrayGather mpsndArrayGather => mpsndArrayGather -> CULong -> IO ()
setAxis mpsndArrayGather value =
  sendMessage mpsndArrayGather setAxisSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @axis@
axisSelector :: Selector '[] CULong
axisSelector = mkSelector "axis"

-- | @Selector@ for @setAxis:@
setAxisSelector :: Selector '[CULong] ()
setAxisSelector = mkSelector "setAxis:"

