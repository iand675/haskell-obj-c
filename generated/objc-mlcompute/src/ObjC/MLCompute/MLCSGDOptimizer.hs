{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCSGDOptimizer
--
-- The MLCSGDOptimizer specifies a stochastic gradient descent optimizer.
--
-- Generated bindings for @MLCSGDOptimizer@.
module ObjC.MLCompute.MLCSGDOptimizer
  ( MLCSGDOptimizer
  , IsMLCSGDOptimizer(..)
  , optimizerWithDescriptor
  , optimizerWithDescriptor_momentumScale_usesNesterovMomentum
  , momentumScale
  , usesNesterovMomentum
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector
  , momentumScaleSelector
  , usesNesterovMomentumSelector


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

-- | Create an MLCSGDOptimizer object with defaults
--
-- Returns: A new MLCSGDOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCSGDOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCSGDOptimizer"
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Create an MLCSGDOptimizer object
--
-- @optimizerDescriptor@ — The optimizer descriptor object
--
-- @momentumScale@ — The momentum scale
--
-- @usesNesterovMomentum@ — A boolean to enable / disable nesterov momentum
--
-- Returns: A new MLCSGDOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:momentumScale:usesNesterovMomentum:@
optimizerWithDescriptor_momentumScale_usesNesterovMomentum :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> CFloat -> Bool -> IO (Id MLCSGDOptimizer)
optimizerWithDescriptor_momentumScale_usesNesterovMomentum optimizerDescriptor momentumScale usesNesterovMomentum =
  do
    cls' <- getRequiredClass "MLCSGDOptimizer"
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:momentumScale:usesNesterovMomentum:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ()), argCFloat (fromIntegral momentumScale), argCULong (if usesNesterovMomentum then 1 else 0)] >>= retainedObject . castPtr

-- | momentumScale
--
-- The momentum factor.  A hyper-parameter.
--
-- The default is 0.0.
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMLCSGDOptimizer mlcsgdOptimizer => mlcsgdOptimizer -> IO CFloat
momentumScale mlcsgdOptimizer  =
  sendMsg mlcsgdOptimizer (mkSelector "momentumScale") retCFloat []

-- | usesNesterovMomentum
--
-- A boolean that specifies whether to apply nesterov momentum or not.
--
-- The default is false.
--
-- ObjC selector: @- usesNesterovMomentum@
usesNesterovMomentum :: IsMLCSGDOptimizer mlcsgdOptimizer => mlcsgdOptimizer -> IO Bool
usesNesterovMomentum mlcsgdOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcsgdOptimizer (mkSelector "usesNesterovMomentum") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:momentumScale:usesNesterovMomentum:@
optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector :: Selector
optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector = mkSelector "optimizerWithDescriptor:momentumScale:usesNesterovMomentum:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @usesNesterovMomentum@
usesNesterovMomentumSelector :: Selector
usesNesterovMomentumSelector = mkSelector "usesNesterovMomentum"

