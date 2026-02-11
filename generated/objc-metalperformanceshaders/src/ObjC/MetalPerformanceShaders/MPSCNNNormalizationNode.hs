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
  , nodeWithSourceSelector
  , initWithSourceSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector


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

-- | @+ nodeWithSource:@
nodeWithSource :: IsMPSNNImageNode sourceNode => sourceNode -> IO (Id MPSCNNNormalizationNode)
nodeWithSource sourceNode =
  do
    cls' <- getRequiredClass "MPSCNNNormalizationNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNNormalizationNode mpscnnNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnNormalizationNode -> sourceNode -> IO (Id MPSCNNNormalizationNode)
initWithSource mpscnnNormalizationNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnNormalizationNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
alpha mpscnnNormalizationNode  =
  sendMsg mpscnnNormalizationNode (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setAlpha mpscnnNormalizationNode  value =
  sendMsg mpscnnNormalizationNode (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
beta mpscnnNormalizationNode  =
  sendMsg mpscnnNormalizationNode (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setBeta mpscnnNormalizationNode  value =
  sendMsg mpscnnNormalizationNode (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> IO CFloat
delta mpscnnNormalizationNode  =
  sendMsg mpscnnNormalizationNode (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNNormalizationNode mpscnnNormalizationNode => mpscnnNormalizationNode -> CFloat -> IO ()
setDelta mpscnnNormalizationNode  value =
  sendMsg mpscnnNormalizationNode (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

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

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector
setDeltaSelector = mkSelector "setDelta:"

