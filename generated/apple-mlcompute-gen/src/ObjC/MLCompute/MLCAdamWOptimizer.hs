{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCAdamWOptimizer
--
-- The MLCAdamWOptimizer specifies the AdamW optimizer.
--
-- Generated bindings for @MLCAdamWOptimizer@.
module ObjC.MLCompute.MLCAdamWOptimizer
  ( MLCAdamWOptimizer
  , IsMLCAdamWOptimizer(..)
  , optimizerWithDescriptor
  , optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStep
  , beta1
  , beta2
  , epsilon
  , usesAMSGrad
  , timeStep
  , beta1Selector
  , beta2Selector
  , epsilonSelector
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector
  , timeStepSelector
  , usesAMSGradSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create an MLCAdamWOptimizer object with defaults
--
-- Returns: A new MLCAdamWOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCAdamWOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCAdamWOptimizer"
    sendClassMessage cls' optimizerWithDescriptorSelector (toMLCOptimizerDescriptor optimizerDescriptor)

-- | Create an MLCAdamWOptimizer object
--
-- @optimizerDescriptor@ — The optimizer descriptor object
--
-- @beta1@ — The beta1 value
--
-- @beta2@ — The beta2 value
--
-- @epsilon@ — The epsilon value to use to improve numerical stability
--
-- @usesAMSGrad@ — Whether to use the AMSGrad variant of this algorithm from the paper (https://arxiv.org/abs/1904.09237)
--
-- @timeStep@ — The initial timestep to use for the update
--
-- Returns: A new MLCAdamWOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStep :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> CFloat -> CFloat -> CFloat -> Bool -> CULong -> IO (Id MLCAdamWOptimizer)
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStep optimizerDescriptor beta1 beta2 epsilon usesAMSGrad timeStep =
  do
    cls' <- getRequiredClass "MLCAdamWOptimizer"
    sendClassMessage cls' optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector (toMLCOptimizerDescriptor optimizerDescriptor) beta1 beta2 epsilon usesAMSGrad timeStep

-- | beta1
--
-- Coefficent used for computing running averages of gradient.
--
-- The default is 0.9.
--
-- ObjC selector: @- beta1@
beta1 :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
beta1 mlcAdamWOptimizer =
  sendMessage mlcAdamWOptimizer beta1Selector

-- | beta2
--
-- Coefficent used for computing running averages of square of gradient.
--
-- The default is 0.999.
--
-- ObjC selector: @- beta2@
beta2 :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
beta2 mlcAdamWOptimizer =
  sendMessage mlcAdamWOptimizer beta2Selector

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
epsilon mlcAdamWOptimizer =
  sendMessage mlcAdamWOptimizer epsilonSelector

-- | usesAMSGrad
--
-- Whether to use the AMSGrad variant of this algorithm
--
-- The default is false
--
-- ObjC selector: @- usesAMSGrad@
usesAMSGrad :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO Bool
usesAMSGrad mlcAdamWOptimizer =
  sendMessage mlcAdamWOptimizer usesAMSGradSelector

-- | timeStep
--
-- The current timestep used for the update.
--
-- The default is 1.
--
-- ObjC selector: @- timeStep@
timeStep :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CULong
timeStep mlcAdamWOptimizer =
  sendMessage mlcAdamWOptimizer timeStepSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector '[Id MLCOptimizerDescriptor] (Id MLCAdamWOptimizer)
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector :: Selector '[Id MLCOptimizerDescriptor, CFloat, CFloat, CFloat, Bool, CULong] (Id MLCAdamWOptimizer)
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector = mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:"

-- | @Selector@ for @beta1@
beta1Selector :: Selector '[] CFloat
beta1Selector = mkSelector "beta1"

-- | @Selector@ for @beta2@
beta2Selector :: Selector '[] CFloat
beta2Selector = mkSelector "beta2"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @usesAMSGrad@
usesAMSGradSelector :: Selector '[] Bool
usesAMSGradSelector = mkSelector "usesAMSGrad"

-- | @Selector@ for @timeStep@
timeStepSelector :: Selector '[] CULong
timeStepSelector = mkSelector "timeStep"

