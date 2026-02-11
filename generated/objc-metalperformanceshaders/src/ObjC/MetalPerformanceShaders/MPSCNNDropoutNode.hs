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
  , nodeWithSourceSelector
  , initWithSourceSelector
  , nodeWithSource_keepProbabilitySelector
  , initWithSource_keepProbabilitySelector
  , keepProbabilitySelector
  , seedSelector


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
nodeWithSource :: IsMPSNNImageNode source => source -> IO (Id MPSCNNDropoutNode)
nodeWithSource source =
  do
    cls' <- getRequiredClass "MPSCNNDropoutNode"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNDropoutNode mpscnnDropoutNode, IsMPSNNImageNode source) => mpscnnDropoutNode -> source -> IO (Id MPSCNNDropoutNode)
initWithSource mpscnnDropoutNode  source =
withObjCPtr source $ \raw_source ->
    sendMsg mpscnnDropoutNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | @+ nodeWithSource:keepProbability:@
nodeWithSource_keepProbability :: IsMPSNNImageNode source => source -> CFloat -> IO (Id MPSCNNDropoutNode)
nodeWithSource_keepProbability source keepProbability =
  do
    cls' <- getRequiredClass "MPSCNNDropoutNode"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:keepProbability:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCFloat (fromIntegral keepProbability)] >>= retainedObject . castPtr

-- | @- initWithSource:keepProbability:@
initWithSource_keepProbability :: (IsMPSCNNDropoutNode mpscnnDropoutNode, IsMPSNNImageNode source) => mpscnnDropoutNode -> source -> CFloat -> IO (Id MPSCNNDropoutNode)
initWithSource_keepProbability mpscnnDropoutNode  source keepProbability =
withObjCPtr source $ \raw_source ->
    sendMsg mpscnnDropoutNode (mkSelector "initWithSource:keepProbability:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCFloat (fromIntegral keepProbability)] >>= ownedObject . castPtr

-- | @- keepProbability@
keepProbability :: IsMPSCNNDropoutNode mpscnnDropoutNode => mpscnnDropoutNode -> IO CFloat
keepProbability mpscnnDropoutNode  =
  sendMsg mpscnnDropoutNode (mkSelector "keepProbability") retCFloat []

-- | @- seed@
seed :: IsMPSCNNDropoutNode mpscnnDropoutNode => mpscnnDropoutNode -> IO CULong
seed mpscnnDropoutNode  =
  sendMsg mpscnnDropoutNode (mkSelector "seed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:@
nodeWithSourceSelector :: Selector
nodeWithSourceSelector = mkSelector "nodeWithSource:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @nodeWithSource:keepProbability:@
nodeWithSource_keepProbabilitySelector :: Selector
nodeWithSource_keepProbabilitySelector = mkSelector "nodeWithSource:keepProbability:"

-- | @Selector@ for @initWithSource:keepProbability:@
initWithSource_keepProbabilitySelector :: Selector
initWithSource_keepProbabilitySelector = mkSelector "initWithSource:keepProbability:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

