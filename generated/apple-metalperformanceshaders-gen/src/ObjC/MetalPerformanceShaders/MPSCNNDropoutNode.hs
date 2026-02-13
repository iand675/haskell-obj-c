{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNDropoutNode@.
module ObjC.MetalPerformanceShaders.MPSCNNDropoutNode
  ( MPSCNNDropoutNode
  , IsMPSCNNDropoutNode(..)
  , nodeWithSource
  , initWithSource
  , nodeWithSource_keepProbability
  , initWithSource_keepProbability
  , keepProbability
  , seed
  , initWithSourceSelector
  , initWithSource_keepProbabilitySelector
  , keepProbabilitySelector
  , nodeWithSourceSelector
  , nodeWithSource_keepProbabilitySelector
  , seedSelector


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
nodeWithSource :: IsMPSNNImageNode source => source -> IO (Id MPSCNNDropoutNode)
nodeWithSource source =
  do
    cls' <- getRequiredClass "MPSCNNDropoutNode"
    sendClassMessage cls' nodeWithSourceSelector (toMPSNNImageNode source)

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNDropoutNode mpscnnDropoutNode, IsMPSNNImageNode source) => mpscnnDropoutNode -> source -> IO (Id MPSCNNDropoutNode)
initWithSource mpscnnDropoutNode source =
  sendOwnedMessage mpscnnDropoutNode initWithSourceSelector (toMPSNNImageNode source)

-- | @+ nodeWithSource:keepProbability:@
nodeWithSource_keepProbability :: IsMPSNNImageNode source => source -> CFloat -> IO (Id MPSCNNDropoutNode)
nodeWithSource_keepProbability source keepProbability =
  do
    cls' <- getRequiredClass "MPSCNNDropoutNode"
    sendClassMessage cls' nodeWithSource_keepProbabilitySelector (toMPSNNImageNode source) keepProbability

-- | @- initWithSource:keepProbability:@
initWithSource_keepProbability :: (IsMPSCNNDropoutNode mpscnnDropoutNode, IsMPSNNImageNode source) => mpscnnDropoutNode -> source -> CFloat -> IO (Id MPSCNNDropoutNode)
initWithSource_keepProbability mpscnnDropoutNode source keepProbability =
  sendOwnedMessage mpscnnDropoutNode initWithSource_keepProbabilitySelector (toMPSNNImageNode source) keepProbability

-- | @- keepProbability@
keepProbability :: IsMPSCNNDropoutNode mpscnnDropoutNode => mpscnnDropoutNode -> IO CFloat
keepProbability mpscnnDropoutNode =
  sendMessage mpscnnDropoutNode keepProbabilitySelector

-- | @- seed@
seed :: IsMPSCNNDropoutNode mpscnnDropoutNode => mpscnnDropoutNode -> IO CULong
seed mpscnnDropoutNode =
  sendMessage mpscnnDropoutNode seedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNDropoutNode)
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNDropoutNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:keepProbability:@
nodeWithSource_keepProbabilitySelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNDropoutNode)
nodeWithSource_keepProbabilitySelector = mkSelector "nodeWithSource:keepProbability:"

-- | @Selector@ for @initWithSource:keepProbability:@
initWithSource_keepProbabilitySelector :: Selector '[Id MPSNNImageNode, CFloat] (Id MPSCNNDropoutNode)
initWithSource_keepProbabilitySelector = mkSelector "initWithSource:keepProbability:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector '[] CFloat
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

