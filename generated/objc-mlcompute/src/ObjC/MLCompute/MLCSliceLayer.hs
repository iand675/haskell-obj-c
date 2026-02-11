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
  , sliceLayerWithStart_end_strideSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sliceLayerWithStart:end:stride:@
sliceLayerWithStart_end_strideSelector :: Selector
sliceLayerWithStart_end_strideSelector = mkSelector "sliceLayerWithStart:end:stride:"

