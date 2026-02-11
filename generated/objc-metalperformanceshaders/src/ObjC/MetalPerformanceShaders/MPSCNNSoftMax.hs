{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNSoftMax
--
-- This depends on Metal.framework
--
-- The softMax filter is a neural transfer function and is useful for classification tasks.              The softMax filter is applied across feature channels and in a convolutional manner at all              spatial locations. The softMax filter can be seen as the combination of an              activation function (exponential) and a normalization operator.              For each feature channel per pixel in an image in a feature map, the softMax filter computes the following:                  result channel in pixel = exp(pixel(x,y,k))/sum(exp(pixel(x,y,0)) ... exp(pixel(x,y,N-1))                      where N is the number of feature channels
--
-- Generated bindings for @MPSCNNSoftMax@.
module ObjC.MetalPerformanceShaders.MPSCNNSoftMax
  ( MPSCNNSoftMax
  , IsMPSCNNSoftMax(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

