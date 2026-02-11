{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLogSoftMax
--
-- This depends on Metal.framework
--
-- The logarithmic softMax filter can be achieved by taking the natural logarithm of the              the result of the softMax filter. The results are often used to construct a loss function to be              minimized when training neural networks.              For each feature channel per pixel in an image in a feature map, the logarithmic softMax filter              computes the following:                  result channel in pixel = pixel(x,y,k)) - ln{sum(exp(pixel(x,y,0)) ... exp(pixel(x,y,N-1))}                      where N is the number of feature channels and y = ln{x} satisfies e^y = x.
--
-- Generated bindings for @MPSCNNLogSoftMax@.
module ObjC.MetalPerformanceShaders.MPSCNNLogSoftMax
  ( MPSCNNLogSoftMax
  , IsMPSCNNLogSoftMax(..)


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

