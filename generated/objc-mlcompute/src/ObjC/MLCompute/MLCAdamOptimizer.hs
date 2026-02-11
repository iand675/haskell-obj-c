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
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector
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

-- | Create a MLCAdamOptimizer object with defaults
--
-- Returns: A new MLCAdamOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCAdamOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCAdamOptimizer"
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ()), argCFloat (fromIntegral beta1), argCFloat (fromIntegral beta2), argCFloat (fromIntegral epsilon), argCULong (fromIntegral timeStep)] >>= retainedObject . castPtr

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
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:usesAMSGrad:timeStep:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ()), argCFloat (fromIntegral beta1), argCFloat (fromIntegral beta2), argCFloat (fromIntegral epsilon), argCULong (if usesAMSGrad then 1 else 0), argCULong (fromIntegral timeStep)] >>= retainedObject . castPtr

-- | beta1
--
-- Coefficent used for computing running averages of gradient.
--
-- The default is 0.9.
--
-- ObjC selector: @- beta1@
beta1 :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
beta1 mlcAdamOptimizer  =
  sendMsg mlcAdamOptimizer (mkSelector "beta1") retCFloat []

-- | beta2
--
-- Coefficent used for computing running averages of square of gradient.
--
-- The default is 0.999.
--
-- ObjC selector: @- beta2@
beta2 :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
beta2 mlcAdamOptimizer  =
  sendMsg mlcAdamOptimizer (mkSelector "beta2") retCFloat []

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CFloat
epsilon mlcAdamOptimizer  =
  sendMsg mlcAdamOptimizer (mkSelector "epsilon") retCFloat []

-- | usesAMSGrad
--
-- Whether to use the AMSGrad variant of this algorithm
--
-- The default is false
--
-- ObjC selector: @- usesAMSGrad@
usesAMSGrad :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO Bool
usesAMSGrad mlcAdamOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcAdamOptimizer (mkSelector "usesAMSGrad") retCULong []

-- | timeStep
--
-- The current timestep used for the update.
--
-- The default is 1.
--
-- ObjC selector: @- timeStep@
timeStep :: IsMLCAdamOptimizer mlcAdamOptimizer => mlcAdamOptimizer -> IO CULong
timeStep mlcAdamOptimizer  =
  sendMsg mlcAdamOptimizer (mkSelector "timeStep") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:@
optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector :: Selector
optimizerWithDescriptor_beta1_beta2_epsilon_timeStepSelector = mkSelector "optimizerWithDescriptor:beta1:beta2:epsilon:timeStep:"

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

