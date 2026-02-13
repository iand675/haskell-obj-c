{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , flagsSelector
  , initWithSource_dataSourceSelector
  , nodeWithSource_dataSourceSelector
  , setFlagsSelector
  , setTrainingStyleSelector
  , trainingStyleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' nodeWithSource_dataSourceSelector (toMPSNNImageNode source) dataSource

-- | @- initWithSource:dataSource:@
initWithSource_dataSource :: (IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode, IsMPSNNImageNode source) => mpscnnBatchNormalizationNode -> source -> RawId -> IO (Id MPSCNNBatchNormalizationNode)
initWithSource_dataSource mpscnnBatchNormalizationNode source dataSource =
  sendOwnedMessage mpscnnBatchNormalizationNode initWithSource_dataSourceSelector (toMPSNNImageNode source) dataSource

-- | Options controlling how batch normalization is calculated
--
-- Default: MPSCNNBatchNormalizationFlagsDefault
--
-- ObjC selector: @- flags@
flags :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> IO MPSCNNBatchNormalizationFlags
flags mpscnnBatchNormalizationNode =
  sendMessage mpscnnBatchNormalizationNode flagsSelector

-- | Options controlling how batch normalization is calculated
--
-- Default: MPSCNNBatchNormalizationFlagsDefault
--
-- ObjC selector: @- setFlags:@
setFlags :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> MPSCNNBatchNormalizationFlags -> IO ()
setFlags mpscnnBatchNormalizationNode value =
  sendMessage mpscnnBatchNormalizationNode setFlagsSelector value

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnBatchNormalizationNode =
  sendMessage mpscnnBatchNormalizationNode trainingStyleSelector

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNBatchNormalizationNode mpscnnBatchNormalizationNode => mpscnnBatchNormalizationNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnBatchNormalizationNode value =
  sendMessage mpscnnBatchNormalizationNode setTrainingStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:dataSource:@
nodeWithSource_dataSourceSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNBatchNormalizationNode)
nodeWithSource_dataSourceSelector = mkSelector "nodeWithSource:dataSource:"

-- | @Selector@ for @initWithSource:dataSource:@
initWithSource_dataSourceSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNBatchNormalizationNode)
initWithSource_dataSourceSelector = mkSelector "initWithSource:dataSource:"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] MPSCNNBatchNormalizationFlags
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector '[MPSCNNBatchNormalizationFlags] ()
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector '[] MPSNNTrainingStyle
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector '[MPSNNTrainingStyle] ()
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

