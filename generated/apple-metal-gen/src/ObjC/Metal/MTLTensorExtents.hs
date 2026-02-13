{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An array of length matching the rank, holding the dimensions of a tensor.
--
-- Supports rank up to ``MTL_TENSOR_MAX_RANK``.
--
-- Generated bindings for @MTLTensorExtents@.
module ObjC.Metal.MTLTensorExtents
  ( MTLTensorExtents
  , IsMTLTensorExtents(..)
  , initWithRank_values
  , extentAtDimensionIndex
  , rank
  , extentAtDimensionIndexSelector
  , initWithRank_valuesSelector
  , rankSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new tensor extents with the rank and extent values you provide.
--
-- Zero rank extents represent scalars. @values@ can only be @nil@if @rank@ is 0. - Parameters:   - rank: the number of dimensions.   - values: an array of length @rank@ that specifies the size of each dimension. The first dimension is the innermost dimension. - Returns: Tensor extents with the rank and extent values you provide. Returns @nil@ if @rank@ exceeds 0 and @values@ is nil or if @rank@ exceeds ``MTL_TENSOR_MAX_RANK``.
--
-- ObjC selector: @- initWithRank:values:@
initWithRank_values :: IsMTLTensorExtents mtlTensorExtents => mtlTensorExtents -> CULong -> Const (Ptr CLong) -> IO (Id MTLTensorExtents)
initWithRank_values mtlTensorExtents rank values =
  sendOwnedMessage mtlTensorExtents initWithRank_valuesSelector rank values

-- | Returns the extent at an index.
--
-- - Parameters:   - dimensionIndex: the index of the dimension. The first dimension is the innermost dimension. - Returns: the extent at @dimensionIndex@. This method returns -1 if @dimensionIndex@ is greater than or equal to @rank@.
--
-- ObjC selector: @- extentAtDimensionIndex:@
extentAtDimensionIndex :: IsMTLTensorExtents mtlTensorExtents => mtlTensorExtents -> CULong -> IO CLong
extentAtDimensionIndex mtlTensorExtents dimensionIndex =
  sendMessage mtlTensorExtents extentAtDimensionIndexSelector dimensionIndex

-- | Obtains the rank of the tensor.
--
-- The rank represents the number of dimensions.
--
-- ObjC selector: @- rank@
rank :: IsMTLTensorExtents mtlTensorExtents => mtlTensorExtents -> IO CULong
rank mtlTensorExtents =
  sendMessage mtlTensorExtents rankSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRank:values:@
initWithRank_valuesSelector :: Selector '[CULong, Const (Ptr CLong)] (Id MTLTensorExtents)
initWithRank_valuesSelector = mkSelector "initWithRank:values:"

-- | @Selector@ for @extentAtDimensionIndex:@
extentAtDimensionIndexSelector :: Selector '[CULong] CLong
extentAtDimensionIndexSelector = mkSelector "extentAtDimensionIndex:"

-- | @Selector@ for @rank@
rankSelector :: Selector '[] CULong
rankSelector = mkSelector "rank"

