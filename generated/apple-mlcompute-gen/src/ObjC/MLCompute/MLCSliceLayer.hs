{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Slice layer is used to slice a given source.
--
-- Slicing should not decrease the tensor dimension.              The start, end and stride vectors must have the same number of dimension as the source tensor.              Only positive stride is supported.
--
-- Generated bindings for @MLCSliceLayer@.
module ObjC.MLCompute.MLCSliceLayer
  ( MLCSliceLayer
  , IsMLCSliceLayer(..)
  , sliceLayerWithStart_end_stride
  , start
  , end
  , stride
  , endSelector
  , sliceLayerWithStart_end_strideSelector
  , startSelector
  , strideSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a slice layer
--
-- @stride@ — If set to nil, it will be set to 1.
--
-- Returns: A new layer for slicing tensors.
--
-- ObjC selector: @+ sliceLayerWithStart:end:stride:@
sliceLayerWithStart_end_stride :: (IsNSArray start, IsNSArray end, IsNSArray stride) => start -> end -> stride -> IO (Id MLCSliceLayer)
sliceLayerWithStart_end_stride start end stride =
  do
    cls' <- getRequiredClass "MLCSliceLayer"
    sendClassMessage cls' sliceLayerWithStart_end_strideSelector (toNSArray start) (toNSArray end) (toNSArray stride)

-- | start
--
-- A vector of length equal to that of source. The element at index i specifies the beginning of slice in dimension i.
--
-- ObjC selector: @- start@
start :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
start mlcSliceLayer =
  sendMessage mlcSliceLayer startSelector

-- | end
--
-- A vector of length equal to that of source. The element at index i specifies the end of slice in dimension i.
--
-- ObjC selector: @- end@
end :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
end mlcSliceLayer =
  sendMessage mlcSliceLayer endSelector

-- | stride
--
-- A vector of length equal to that of source. The element at index i specifies the stride of slice in dimension i.
--
-- ObjC selector: @- stride@
stride :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
stride mlcSliceLayer =
  sendMessage mlcSliceLayer strideSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sliceLayerWithStart:end:stride:@
sliceLayerWithStart_end_strideSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id MLCSliceLayer)
sliceLayerWithStart_end_strideSelector = mkSelector "sliceLayerWithStart:end:stride:"

-- | @Selector@ for @start@
startSelector :: Selector '[] (Id NSArray)
startSelector = mkSelector "start"

-- | @Selector@ for @end@
endSelector :: Selector '[] (Id NSArray)
endSelector = mkSelector "end"

-- | @Selector@ for @stride@
strideSelector :: Selector '[] (Id NSArray)
strideSelector = mkSelector "stride"

