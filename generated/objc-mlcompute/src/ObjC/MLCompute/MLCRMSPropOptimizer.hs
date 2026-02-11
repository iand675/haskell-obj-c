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
  , optimizerWithDescriptorSelector
  , optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector
  , momentumScaleSelector
  , alphaSelector
  , epsilonSelector
  , isCenteredSelector


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

-- | Create a MLCRMSPropOptimizer object with defaults
--
-- Returns: A new MLCRMSPropOptimizer object.
--
-- ObjC selector: @+ optimizerWithDescriptor:@
optimizerWithDescriptor :: IsMLCOptimizerDescriptor optimizerDescriptor => optimizerDescriptor -> IO (Id MLCRMSPropOptimizer)
optimizerWithDescriptor optimizerDescriptor =
  do
    cls' <- getRequiredClass "MLCRMSPropOptimizer"
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
      sendClassMsg cls' (mkSelector "optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:") (retPtr retVoid) [argPtr (castPtr raw_optimizerDescriptor :: Ptr ()), argCFloat (fromIntegral momentumScale), argCFloat (fromIntegral alpha), argCFloat (fromIntegral epsilon), argCULong (if isCentered then 1 else 0)] >>= retainedObject . castPtr

-- | momentumScale
--
-- The momentum factor.  A hyper-parameter.
--
-- The default is 0.0.
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
momentumScale mlcrmsPropOptimizer  =
  sendMsg mlcrmsPropOptimizer (mkSelector "momentumScale") retCFloat []

-- | alpha
--
-- The smoothing constant.
--
-- The default is 0.99.
--
-- ObjC selector: @- alpha@
alpha :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
alpha mlcrmsPropOptimizer  =
  sendMsg mlcrmsPropOptimizer (mkSelector "alpha") retCFloat []

-- | epsilon
--
-- A term added to improve numerical stability.
--
-- The default is 1e-8.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO CFloat
epsilon mlcrmsPropOptimizer  =
  sendMsg mlcrmsPropOptimizer (mkSelector "epsilon") retCFloat []

-- | isCentered
--
-- If True, compute the centered RMSProp, the gradient is normalized by an estimation of its variance.
--
-- The default is false.
--
-- ObjC selector: @- isCentered@
isCentered :: IsMLCRMSPropOptimizer mlcrmsPropOptimizer => mlcrmsPropOptimizer -> IO Bool
isCentered mlcrmsPropOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcrmsPropOptimizer (mkSelector "isCentered") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optimizerWithDescriptor:@
optimizerWithDescriptorSelector :: Selector
optimizerWithDescriptorSelector = mkSelector "optimizerWithDescriptor:"

-- | @Selector@ for @optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:@
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector :: Selector
optimizerWithDescriptor_momentumScale_alpha_epsilon_isCenteredSelector = mkSelector "optimizerWithDescriptor:momentumScale:alpha:epsilon:isCentered:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @isCentered@
isCenteredSelector :: Selector
isCenteredSelector = mkSelector "isCentered"

