{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A matrix multiplication kernel operating on MPSNDArray objects.
--
-- A MPSNDArrayMatrixMultiplication object computes, for each 2-D matrix within              a 4-D MPSNDArray object:
--
-- D = alpha * A * B + beta * C
--
-- A, B, C, and D are matrices which are represented by objects stored              in the two most major dimensions of the MPSNDArray. alpha and beta              are scalar values (of the same data type as values of D and C) which              are applied as shown above.
--
-- If an input's 3rd or 4th dimension is 1 its data will be broadcast as              appropriate to the remaining input's 3rd or 4th dimension respectively.
--
-- Generated bindings for @MPSNDArrayMatrixMultiplication@.
module ObjC.MetalPerformanceShaders.MPSNDArrayMatrixMultiplication
  ( MPSNDArrayMatrixMultiplication
  , IsMPSNDArrayMatrixMultiplication(..)
  , alpha
  , setAlpha
  , beta
  , setBeta
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector


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

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNDArrayMatrixMultiplication mpsndArrayMatrixMultiplication => mpsndArrayMatrixMultiplication -> IO CDouble
alpha mpsndArrayMatrixMultiplication  =
  sendMsg mpsndArrayMatrixMultiplication (mkSelector "alpha") retCDouble []

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSNDArrayMatrixMultiplication mpsndArrayMatrixMultiplication => mpsndArrayMatrixMultiplication -> CDouble -> IO ()
setAlpha mpsndArrayMatrixMultiplication  value =
  sendMsg mpsndArrayMatrixMultiplication (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- | beta
--
-- The scale factor to apply to the addend if available.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- beta@
beta :: IsMPSNDArrayMatrixMultiplication mpsndArrayMatrixMultiplication => mpsndArrayMatrixMultiplication -> IO CDouble
beta mpsndArrayMatrixMultiplication  =
  sendMsg mpsndArrayMatrixMultiplication (mkSelector "beta") retCDouble []

-- | beta
--
-- The scale factor to apply to the addend if available.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSNDArrayMatrixMultiplication mpsndArrayMatrixMultiplication => mpsndArrayMatrixMultiplication -> CDouble -> IO ()
setBeta mpsndArrayMatrixMultiplication  value =
  sendMsg mpsndArrayMatrixMultiplication (mkSelector "setBeta:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector
setBetaSelector = mkSelector "setBeta:"

