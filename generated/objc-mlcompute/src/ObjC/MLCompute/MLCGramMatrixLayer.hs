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
    sendClassMsg cls' (mkSelector "layerWithScale:") (retPtr retVoid) [argCFloat (fromIntegral scale)] >>= retainedObject . castPtr

-- | scale
--
-- The scale factor
--
-- ObjC selector: @- scale@
scale :: IsMLCGramMatrixLayer mlcGramMatrixLayer => mlcGramMatrixLayer -> IO CFloat
scale mlcGramMatrixLayer  =
  sendMsg mlcGramMatrixLayer (mkSelector "scale") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithScale:@
layerWithScaleSelector :: Selector
layerWithScaleSelector = mkSelector "layerWithScale:"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

