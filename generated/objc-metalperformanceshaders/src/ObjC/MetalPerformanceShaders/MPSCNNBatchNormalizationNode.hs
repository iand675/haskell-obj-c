{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationNode
--
-- A node representing batch normalization for inference or training
--
-- Batch normalization operates differently for inference and training.              For inference, the normalization is done according to a static statistical              representation of data saved during training. For training, this representation              is ever evolving.  In the low level MPS batch normalization interface,              during training, the batch normalization is broken up into two steps:              calculation of the statistical representation of input data, followed              by normalization once the statistics are known for the entire batch.              These are MPSCNNBatchNormalizationStatistics and MPSCNNBatchNormalization,              respectively.
--
-- When this node appears in a graph and is not required to produce a              MPSCNNBatchNormalizationState -- that is, MPSCNNBatchNormalizationNode.resultState              is not used within the graph -- then it operates in inference mode              and new batch-only statistics are not calculated. When this state node              is consumed, then the node is assumed to be in training mode and              new statistics will be calculated and written to the MPSCNNBatchNormalizationState              and passed along to the MPSCNNBatchNormalizationGradient and              MPSCNNBatchNormalizationStatisticsGradient as necessary. This should              allow you to construct an identical sequence of nodes for inference              and training and expect the right thing to happen.
--
-- Generated bindings for @MPSCNNBatchNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationNode
  ( MPSCNNBatchNormalizationNode
  , IsMPSCNNBatchNormalizationNode(..)
  , nodeWithSource_dataSource
  , initWithSource_dataSource
  , flags
  , setFlags
  , trainingStyle
  , setTrainingStyle
  , nodeWithSource_dataSourceSelector
  , initWithSource_dataSourceSelector
  , flagsSelector
  , setFlagsSelector
  , trainingStyleSelector
  , setTrainingStyleSelector

  -- * Enum types
  , MPSCNNBatchNormalizationFlags(MPSCNNBatchNormalizationFlags)
  , pattern MPSCNNBatchNormalizationFlagsDefault
  , pattern MPSCNNBatchNormalizationFlagsCalculateStatisticsAutomatic
  , pattern MPSCNNBatchNormalizationFlagsCalculateStatisticsAlways
  , pattern MPSCNNBatchNormalizationFlagsCalculateStatisticsNever
  , pattern MPSCNNBatchNormalizationFlagsCalculateStatisticsMask
  , MPSNNTrainingStyle(MPSNNTrainingStyle)
  , pattern MPSNNTrainingStyleUpdateDeviceNone
  , pattern MPSNNTrainingStyleUpdateDeviceCPU
  , pattern MPSNNTrainingStyleUpdateDeviceGPU

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:dataSource:@
nodeWithSource_dataSource :: IsMPSNNImageNode source => source -> RawId -> IO (Id MPSCNNBatchNormalizationNode)
nodeWithSource_dataSource source dataSource =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationNode"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:dataSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:dataSource:@
initWithSource_dataSource :: (IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode, IsMPSNNImageNode source) => mpscnnBatchNormalizationNode -> source -> RawId -> IO (Id MPSCNNBatchNormalizationNode)
initWithSource_dataSource mpscnnBatchNormalizationNode  source dataSource =
withObjCPtr source $ \raw_source ->
    sendMsg mpscnnBatchNormalizationNode (mkSelector "initWithSource:dataSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= ownedObject . castPtr

-- | Options controlling how batch normalization is calculated
--
-- Default: MPSCNNBatchNormalizationFlagsDefault
--
-- ObjC selector: @- flags@
flags :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> IO MPSCNNBatchNormalizationFlags
flags mpscnnBatchNormalizationNode  =
  fmap (coerce :: CULong -> MPSCNNBatchNormalizationFlags) $ sendMsg mpscnnBatchNormalizationNode (mkSelector "flags") retCULong []

-- | Options controlling how batch normalization is calculated
--
-- Default: MPSCNNBatchNormalizationFlagsDefault
--
-- ObjC selector: @- setFlags:@
setFlags :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> MPSCNNBatchNormalizationFlags -> IO ()
setFlags mpscnnBatchNormalizationNode  value =
  sendMsg mpscnnBatchNormalizationNode (mkSelector "setFlags:") retVoid [argCULong (coerce value)]

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnBatchNormalizationNode  =
  fmap (coerce :: CULong -> MPSNNTrainingStyle) $ sendMsg mpscnnBatchNormalizationNode (mkSelector "trainingStyle") retCULong []

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnBatchNormalizationNode  value =
  sendMsg mpscnnBatchNormalizationNode (mkSelector "setTrainingStyle:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:dataSource:@
nodeWithSource_dataSourceSelector :: Selector
nodeWithSource_dataSourceSelector = mkSelector "nodeWithSource:dataSource:"

-- | @Selector@ for @initWithSource:dataSource:@
initWithSource_dataSourceSelector :: Selector
initWithSource_dataSourceSelector = mkSelector "initWithSource:dataSource:"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

