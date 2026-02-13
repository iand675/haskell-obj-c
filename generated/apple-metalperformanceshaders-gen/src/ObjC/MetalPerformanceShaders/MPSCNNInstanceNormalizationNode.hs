{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNInstanceNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNInstanceNormalizationNode
  ( MPSCNNInstanceNormalizationNode
  , IsMPSCNNInstanceNormalizationNode(..)
  , nodeWithSource_dataSource
  , initWithSource_dataSource
  , trainingStyle
  , setTrainingStyle
  , initWithSource_dataSourceSelector
  , nodeWithSource_dataSourceSelector
  , setTrainingStyleSelector
  , trainingStyleSelector

  -- * Enum types
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
nodeWithSource_dataSource :: IsMPSNNImageNode source => source -> RawId -> IO (Id MPSCNNInstanceNormalizationNode)
nodeWithSource_dataSource source dataSource =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationNode"
    sendClassMessage cls' nodeWithSource_dataSourceSelector (toMPSNNImageNode source) dataSource

-- | @- initWithSource:dataSource:@
initWithSource_dataSource :: (IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode, IsMPSNNImageNode source) => mpscnnInstanceNormalizationNode -> source -> RawId -> IO (Id MPSCNNInstanceNormalizationNode)
initWithSource_dataSource mpscnnInstanceNormalizationNode source dataSource =
  sendOwnedMessage mpscnnInstanceNormalizationNode initWithSource_dataSourceSelector (toMPSNNImageNode source) dataSource

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode => mpscnnInstanceNormalizationNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnInstanceNormalizationNode =
  sendMessage mpscnnInstanceNormalizationNode trainingStyleSelector

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode => mpscnnInstanceNormalizationNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnInstanceNormalizationNode value =
  sendMessage mpscnnInstanceNormalizationNode setTrainingStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:dataSource:@
nodeWithSource_dataSourceSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNInstanceNormalizationNode)
nodeWithSource_dataSourceSelector = mkSelector "nodeWithSource:dataSource:"

-- | @Selector@ for @initWithSource:dataSource:@
initWithSource_dataSourceSelector :: Selector '[Id MPSNNImageNode, RawId] (Id MPSCNNInstanceNormalizationNode)
initWithSource_dataSourceSelector = mkSelector "initWithSource:dataSource:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector '[] MPSNNTrainingStyle
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector '[MPSNNTrainingStyle] ()
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

