{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCAdamOptimizer
--
-- The MLCAdamOptimizer specifies the Adam optimizer.
--
-- Generated bindings for @MLCAdamOptimizer@.
module ObjC.MLCompute.MLCAdamOptimizer
  ( MLCAdamOptimizer
  , IsMLCAdamOptimizer(..)
  , optimizerWithDescriptor
  , optimizerWithDescriptor_beta1_beta2_epsilon_timeStep
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
  , optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector
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

-- | Create a MLCAdamOptimizer object with defaults
--
-- Returns: A new MLCAdamOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCAdamOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCAdamOptimizer"
    sendClassMessage cls' optimizerWithDescriptorSelector (toMLCOptimizerDescriptor optimizerDescriptor)

-- | Create a MLCAdamOptimizer object
--
-- @optimizerDescriptor@ — The optimizer descriptor object
--
-- @beta1@ — The beta1 value
--
-- @beta2@ — The beta2 value
--
-- @epsilon@ — The epsilon value to use to improve numerical stability
--
-- @timeStep@ — The initial timestep to use for the update
--
-- Returns: A new MLCAdamOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_timeStep :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> CFloat -> CFloat -> CFloat -> CULong -> IO (Id MLCAdamOptimizer)
optimizerWithDescriptor_beta1_beta2_epsilon_timeStep optimizerDescriptor beta1 beta2 epsilon timeStep =
  do
    cls' <- getRequiredClass "MLCAdamOptimizer"
    sendClassMessage cls' optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector (toMLCOptimizerDescriptor optimizerDescriptor) beta1 beta2 epsilon timeStep

-- | Create a MLCAdamOptimizer object
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
-- Returns: A new MLCAdamOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStep :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> CFloat -> CFloat -> CFloat -> Bool -> CULong -> IO (Id MLCAdamOptimizer)
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStep optimizerDescriptor beta1 beta2 epsilon usesAMSGrad timeStep =
  do
    cls' <- getRequiredClass "MLCAdamOptimizer"
    sendClassMessage cls' optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector (toMLCOptimizerDescriptor optimizerDescriptor) beta1 beta2 epsilon usesAMSGrad timeStep

-- | beta1
--
-- Coefficent used for computing running averages of gradient.
--
-- The default is 0.9.
--
-- ObjC selector: @- beta1@
beta1 :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
beta1 mlcAdamOptimizer =
  sendMessage mlcAdamOptimizer beta1Selector

-- | beta2
--
-- Coefficent used for computing running averages of square of gradient.
--
-- The default is 0.999.
--
-- ObjC selector: @- beta2@
beta2 :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
beta2 mlcAdamOptimizer =
  sendMessage mlcAdamOptimizer beta2Selector

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
epsilon mlcAdamOptimizer =
  sendMessage mlcAdamOptimizer epsilonSelector

-- | usesAMSGrad
--
-- Whether to use the AMSGrad variant of this algorithm
--
-- The default is false
--
-- ObjC selector: @- usesAMSGrad@
usesAMSGrad :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO Bool
usesAMSGrad mlcAdamOptimizer =
  sendMessage mlcAdamOptimizer usesAMSGradSelector

-- | timeStep
--
-- The current timestep used for the update.
--
-- The default is 1.
--
-- ObjC selector: @- timeStep@
timeStep :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CULong
timeStep mlcAdamOptimizer =
  sendMessage mlcAdamOptimizer timeStepSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector '[Id MLCOptimizerDescriptor] (Id MLCAdamOptimizer)
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector :: Selector '[Id MLCOptimizerDescriptor, CFloat, CFloat, CFloat, CULong] (Id MLCAdamOptimizer)
optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector = mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:"

-- | @Selector@ for @optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector :: Selector '[Id MLCOptimizerDescriptor, CFloat, CFloat, CFloat, Bool, CULong] (Id MLCAdamOptimizer)
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

