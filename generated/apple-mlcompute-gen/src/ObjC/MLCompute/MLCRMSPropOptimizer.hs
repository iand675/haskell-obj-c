{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCRMSPropOptimizer
--
-- The MLCRMSPropOptimizer specifies the RMSProp optimizer.
--
-- Generated bindings for @MLCRMSPropOptimizer@.
module ObjC.MLCompute.MLCRMSPropOptimizer
  ( MLCRMSPropOptimizer
  , IsMLCRMSPropOptimizer(..)
  , optimizerWithDescriptor
  , optimizerWithDescriptor_momentumScale_alpha_epsilon_isCentered
  , momentumScale
  , alpha
  , epsilon
  , isCentered
  , alphaSelector
  , epsilonSelector
  , isCenteredSelector
  , momentumScaleSelector
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a MLCRMSPropOptimizer object with defaults
--
-- Returns: A new MLCRMSPropOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCRMSPropOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCRMSPropOptimizer"
    sendClassMessage cls' optimizerWithDescriptorSelector (toMLCOptimizerDescriptor optimizerDescriptor)

-- | Create a MLCRMSPropOptimizer object
--
-- @optimizerDescriptor@ — The optimizer descriptor object
--
-- @momentumScale@ — The momentum scale
--
-- @alpha@ — The smoothing constant value
--
-- @epsilon@ — The epsilon value to use to improve numerical stability
--
-- @isCentered@ — A boolean to specify whether to compute the centered RMSProp or not
--
-- Returns: A new MLCRMSPropOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:@
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCentered :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> CFloat -> CFloat -> CFloat -> Bool -> IO (Id MLCRMSPropOptimizer)
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCentered optimizerDescriptor momentumScale alpha epsilon isCentered =
  do
    cls' <- getRequiredClass "MLCRMSPropOptimizer"
    sendClassMessage cls' optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector (toMLCOptimizerDescriptor optimizerDescriptor) momentumScale alpha epsilon isCentered

-- | momentumScale
--
-- The momentum factor.  A hyper-parameter.
--
-- The default is 0.0.
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
momentumScale mlcrmsPropOptimizer =
  sendMessage mlcrmsPropOptimizer momentumScaleSelector

-- | alpha
--
-- The smoothing constant.
--
-- The default is 0.99.
--
-- ObjC selector: @- alpha@
alpha :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
alpha mlcrmsPropOptimizer =
  sendMessage mlcrmsPropOptimizer alphaSelector

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
epsilon mlcrmsPropOptimizer =
  sendMessage mlcrmsPropOptimizer epsilonSelector

-- | isCentered
--
-- If True, compute the centered RMSProp, the gradient is normalized by an estimation of its variance.
--
-- The default is false.
--
-- ObjC selector: @- isCentered@
isCentered :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO Bool
isCentered mlcrmsPropOptimizer =
  sendMessage mlcrmsPropOptimizer isCenteredSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector '[Id MLCOptimizerDescriptor] (Id MLCRMSPropOptimizer)
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:@
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector :: Selector '[Id MLCOptimizerDescriptor, CFloat, CFloat, CFloat, Bool] (Id MLCRMSPropOptimizer)
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector = mkSelector "optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector '[] CFloat
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @isCentered@
isCenteredSelector :: Selector '[] Bool
isCenteredSelector = mkSelector "isCentered"

