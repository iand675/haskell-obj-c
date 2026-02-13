{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | virtual base class for CNN normalization nodes
--
-- Generated bindings for @MPSCNNNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNNormalizationNode
  ( MPSCNNNormalizationNode
  , IsMPSCNNNormalizationNode(..)
  , nodeWithSource
  , initWithSource
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithSourceSelector
  , nodeWithSourceSelector
  , setAlphaSelector
  , setBetaSelector
  , setDeltaSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNormalizationNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNormalizationNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode sourceNode)

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNNormalizationNode mpscnnNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnNormalizationNode -> sourceNode -> IO (Id MPSCNNNormalizationNode)
initWithSource mpscnnNormalizationNode sourceNode =
  sendOwnedMessage mpscnnNormalizationNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
alpha mpscnnNormalizationNode =
  sendMessage mpscnnNormalizationNode alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setAlpha mpscnnNormalizationNode value =
  sendMessage mpscnnNormalizationNode setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
beta mpscnnNormalizationNode =
  sendMessage mpscnnNormalizationNode betaSelector

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setBeta mpscnnNormalizationNode value =
  sendMessage mpscnnNormalizationNode setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
delta mpscnnNormalizationNode =
  sendMessage mpscnnNormalizationNode deltaSelector

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setDelta mpscnnNormalizationNode value =
  sendMessage mpscnnNormalizationNode setDeltaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNormalizationNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNNormalizationNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CFloat] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] CFloat
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector '[CFloat] ()
setBetaSelector = mkSelector "setBeta:"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector '[CFloat] ()
setDeltaSelector = mkSelector "setDelta:"

