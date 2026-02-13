{-# LANGUAGE DataKinds #-}
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
  , momentumScaleSelector
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector
  , usesNesterovMomentumSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' optimizerWithDescriptorSelector (toMLCOptimizerDescriptor optimizerDescriptor)

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
    sendClassMessage cls' optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector (toMLCOptimizerDescriptor optimizerDescriptor) momentumScale usesNesterovMomentum

-- | momentumScale
--
-- The momentum factor.  A hyper-parameter.
--
-- The default is 0.0.
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMLCSGDOptimizer mlcsgdOptimizer => mlcsgdOptimizer -> IO CFloat
momentumScale mlcsgdOptimizer =
  sendMessage mlcsgdOptimizer momentumScaleSelector

-- | usesNesterovMomentum
--
-- A boolean that specifies whether to apply nesterov momentum or not.
--
-- The default is false.
--
-- ObjC selector: @- usesNesterovMomentum@
usesNesterovMomentum :: IsMLCSGDOptimizer mlcsgdOptimizer => mlcsgdOptimizer -> IO Bool
usesNesterovMomentum mlcsgdOptimizer =
  sendMessage mlcsgdOptimizer usesNesterovMomentumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector '[Id MLCOptimizerDescriptor] (Id MLCSGDOptimizer)
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:momentumScale:usesNesterovMomentum:@
optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector :: Selector '[Id MLCOptimizerDescriptor, CFloat, Bool] (Id MLCSGDOptimizer)
optimizerWithDescriptor_momentumScale_usesNesterovMomentumSelector = mkSelector "optimizerWithDescriptor:momentumScale:usesNesterovMomentum:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector '[] CFloat
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @usesNesterovMomentum@
usesNesterovMomentumSelector :: Selector '[] Bool
usesNesterovMomentumSelector = mkSelector "usesNesterovMomentum"

