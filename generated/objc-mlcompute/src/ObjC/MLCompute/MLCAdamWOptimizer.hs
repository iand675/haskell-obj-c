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
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector
  , beta1Selector
  , beta2Selector
  , epsilonSelector
  , usesAMSGradSelector
  , timeStepSelector


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

-- | Create an MLCAdamWOptimizer object with defaults
--
-- Returns: A new MLCAdamWOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCAdamWOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCAdamWOptimizer"
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ()), argCFloat (fromIntegral beta1), argCFloat (fromIntegral beta2), argCFloat (fromIntegral epsilon), argCULong (if usesAMSGrad then 1 else 0), argCULong (fromIntegral timeStep)] >>= retainedObject . castPtr

-- | beta1
--
-- Coefficent used for computing running averages of gradient.
--
-- The default is 0.9.
--
-- ObjC selector: @- beta1@
beta1 :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
beta1 mlcAdamWOptimizer  =
  sendMsg mlcAdamWOptimizer (mkSelector "beta1") retCFloat []

-- | beta2
--
-- Coefficent used for computing running averages of square of gradient.
--
-- The default is 0.999.
--
-- ObjC selector: @- beta2@
beta2 :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
beta2 mlcAdamWOptimizer  =
  sendMsg mlcAdamWOptimizer (mkSelector "beta2") retCFloat []

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CFloat
epsilon mlcAdamWOptimizer  =
  sendMsg mlcAdamWOptimizer (mkSelector "epsilon") retCFloat []

-- | usesAMSGrad
--
-- Whether to use the AMSGrad variant of this algorithm
--
-- The default is false
--
-- ObjC selector: @- usesAMSGrad@
usesAMSGrad :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO Bool
usesAMSGrad mlcAdamWOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcAdamWOptimizer (mkSelector "usesAMSGrad") retCULong []

-- | timeStep
--
-- The current timestep used for the update.
--
-- The default is 1.
--
-- ObjC selector: @- timeStep@
timeStep :: IsMLCAdamWOptimizer mlcAdamWOptimizer => mlcAdamWOptimizer -> IO CULong
timeStep mlcAdamWOptimizer  =
  sendMsg mlcAdamWOptimizer (mkSelector "timeStep") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector :: Selector
optimizerWithDescriptor_beta1_beta2_epsilon_usesAMSGrad_timeStepSelector = mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:"

-- | @Selector@ for @beta1@
beta1Selector :: Selector
beta1Selector = mkSelector "beta1"

-- | @Selector@ for @beta2@
beta2Selector :: Selector
beta2Selector = mkSelector "beta2"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @usesAMSGrad@
usesAMSGradSelector :: Selector
usesAMSGradSelector = mkSelector "usesAMSGrad"

-- | @Selector@ for @timeStep@
timeStepSelector :: Selector
timeStepSelector = mkSelector "timeStep"

