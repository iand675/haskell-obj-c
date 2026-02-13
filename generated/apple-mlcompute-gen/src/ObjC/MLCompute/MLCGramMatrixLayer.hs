{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCGramMatrixLayer
--
-- A gram matrix layer
--
-- The MLComputeGramMatrix  specifies a layer which computes the uncentered cross-correlation                 values between the spatial planes of each feature channel of a tensor. If the input tensor batch is                 x = x[b, y, x, c], where 'b' is batch index, 'y' and 'x' are the spatial coordinates and 'c' is the feature channel                 index then this layer computes the values:
--
-- y = y[b, 1, f, c] = alpha * sum_{x,y} x[b,y,x,f] * x[b,y,x,c], where 'alpha' is a scaling factor.
--
-- This operation can be interpreted to be computing all combinations of fully connected layers                 between the different spatial planes of the input tensor. The results are stored in the feature channel and                 'x'-coordinate indices of the output batch.
--
-- The operation is performed independently for each tensor in a batch.
--
-- Generated bindings for @MLCGramMatrixLayer@.
module ObjC.MLCompute.MLCGramMatrixLayer
  ( MLCGramMatrixLayer
  , IsMLCGramMatrixLayer(..)
  , layerWithScale
  , scale
  , layerWithScaleSelector
  , scaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a GramMatrix layer
--
-- @scale@ — The scaling factor for the output.
--
-- Returns: A new GramMatrix layer
--
-- ObjC selector: @+ layerWithScale:@
layerWithScale :: CFloat -> IO (Id MLCGramMatrixLayer)
layerWithScale scale =
  do
    cls' <- getRequiredClass "MLCGramMatrixLayer"
    sendClassMessage cls' layerWithScaleSelector scale

-- | scale
--
-- The scale factor
--
-- ObjC selector: @- scale@
scale :: IsMLCGramMatrixLayer mlcGramMatrixLayer => mlcGramMatrixLayer -> IO CFloat
scale mlcGramMatrixLayer =
  sendMessage mlcGramMatrixLayer scaleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithScale:@
layerWithScaleSelector :: Selector '[CFloat] (Id MLCGramMatrixLayer)
layerWithScaleSelector = mkSelector "layerWithScale:"

-- | @Selector@ for @scale@
scaleSelector :: Selector '[] CFloat
scaleSelector = mkSelector "scale"

