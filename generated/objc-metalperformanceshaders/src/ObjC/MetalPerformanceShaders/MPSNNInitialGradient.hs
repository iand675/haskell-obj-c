{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNInitialGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNInitialGradient filter specifies a layer which computes the initial gradient for              an aribitrary input image. The operation itself is extremely simple: it computes the gradient of the input image              with itself, resulting in an output image which is filled with '1's for all the inputs that were used.              This serves as the starting point for the computation of gradients between arbitrary images in a network.              Example:                  Suppose that we want to compute gradients for a function that multiplies together two losses:                      f = f(L1, L2) = L1 * L2                  The losses themselves are computed from some inputs x1,x2:                      L1 = L1(x1),                      L2 = L2(x2)                  The filters to compute f, L1, L2 are:                      f = MPSCNNMultiply(L1, L2), where                      L1 = MPSNNForwardLoss1(x1) and                      L2 = MPSNNForwardLoss1(x2)
--
-- To compute df/dx1 we apply the chain rule:
--
-- df/dx1 = d(L1 * L2)/dx1 = d(L1 * L2)/dL1 * dL1/dx1 + d(L1 * L2)/dL2 * dL2/dx1                             = d(L1 * L2)/dL1 * dL1/dx1 = L2 * dL1/dx1
--
-- The MPSCNNMultiplyGradient filter computes for f = L1 * L2 forward op:                      dL/dL1 = dL/df * df/dL1 = dL/df * L2 and                      dL/dL2 = dL/df * df/dL2 = dL/df * L1 where                  dL/df is the input gradient of the chain rule / backpropagation algorithm.                  But in our case we want MPSCNNMultiplyGradient to compute the gradient:                      df/dL1 = d(L1 * L2)/dL1 = L2,                  which shows that                      L = f, which means that dL/dL1 = df/df * df/dL1 = 1 * L2, which                  shows that we get the correct gradient by providing unit input as input gradient to                  the MPSCNNMultiplyGradient.
--
-- Generated bindings for @MPSNNInitialGradient@.
module ObjC.MetalPerformanceShaders.MPSNNInitialGradient
  ( MPSNNInitialGradient
  , IsMPSNNInitialGradient(..)
  , initWithDevice
  , initWithDeviceSelector


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

-- | Initializes a MPSNNInitialGradient kernel.
--
-- @device@ — The MTLDevice on which this MPSNNInitialGradient filter will be used.
--
-- Returns: A valid MPSNNInitialGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNInitialGradient mpsnnInitialGradient => mpsnnInitialGradient -> RawId -> IO (Id MPSNNInitialGradient)
initWithDevice mpsnnInitialGradient  device =
  sendMsg mpsnnInitialGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

