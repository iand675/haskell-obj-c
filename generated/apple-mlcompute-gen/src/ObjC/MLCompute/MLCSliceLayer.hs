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
  , sliceLayerWithStart_end_strideSelector
  , startSelector
  , endSelector
  , strideSelector


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
    withObjCPtr start $ \raw_start ->
      withObjCPtr end $ \raw_end ->
        withObjCPtr stride $ \raw_stride ->
          sendClassMsg cls' (mkSelector "sliceLayerWithStart:end:stride:") (retPtr retVoid) [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr raw_end :: Ptr ()), argPtr (castPtr raw_stride :: Ptr ())] >>= retainedObject . castPtr

-- | start
--
-- A vector of length equal to that of source. The element at index i specifies the beginning of slice in dimension i.
--
-- ObjC selector: @- start@
start :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
start mlcSliceLayer  =
    sendMsg mlcSliceLayer (mkSelector "start") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | end
--
-- A vector of length equal to that of source. The element at index i specifies the end of slice in dimension i.
--
-- ObjC selector: @- end@
end :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
end mlcSliceLayer  =
    sendMsg mlcSliceLayer (mkSelector "end") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | stride
--
-- A vector of length equal to that of source. The element at index i specifies the stride of slice in dimension i.
--
-- ObjC selector: @- stride@
stride :: IsMLCSliceLayer mlcSliceLayer => mlcSliceLayer -> IO (Id NSArray)
stride mlcSliceLayer  =
    sendMsg mlcSliceLayer (mkSelector "stride") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sliceLayerWithStart:end:stride:@
sliceLayerWithStart_end_strideSelector :: Selector
sliceLayerWithStart_end_strideSelector = mkSelector "sliceLayerWithStart:end:stride:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @stride@
strideSelector :: Selector
strideSelector = mkSelector "stride"

