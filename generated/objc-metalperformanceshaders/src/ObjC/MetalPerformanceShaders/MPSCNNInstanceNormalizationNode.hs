{-# LANGUAGE PatternSynonyms #-}
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
  , nodeWithSource_dataSourceSelector
  , initWithSource_dataSourceSelector
  , trainingStyleSelector
  , setTrainingStyleSelector

  -- * Enum types
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
nodeWithSource_dataSource :: IsMPSNNImageNode source => source -> RawId -> IO (Id MPSCNNInstanceNormalizationNode)
nodeWithSource_dataSource source dataSource =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationNode"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "nodeWithSource:dataSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:dataSource:@
initWithSource_dataSource :: (IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode, IsMPSNNImageNode source) => mpscnnInstanceNormalizationNode -> source -> RawId -> IO (Id MPSCNNInstanceNormalizationNode)
initWithSource_dataSource mpscnnInstanceNormalizationNode  source dataSource =
withObjCPtr source $ \raw_source ->
    sendMsg mpscnnInstanceNormalizationNode (mkSelector "initWithSource:dataSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= ownedObject . castPtr

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- trainingStyle@
trainingStyle :: IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode => mpscnnInstanceNormalizationNode -> IO MPSNNTrainingStyle
trainingStyle mpscnnInstanceNormalizationNode  =
  fmap (coerce :: CULong -> MPSNNTrainingStyle) $ sendMsg mpscnnInstanceNormalizationNode (mkSelector "trainingStyle") retCULong []

-- | The training style of the forward node will be propagated to gradient nodes made from it
--
-- ObjC selector: @- setTrainingStyle:@
setTrainingStyle :: IsMPSCNNInstanceNormalizationNode mpscnnInstanceNormalizationNode => mpscnnInstanceNormalizationNode -> MPSNNTrainingStyle -> IO ()
setTrainingStyle mpscnnInstanceNormalizationNode  value =
  sendMsg mpscnnInstanceNormalizationNode (mkSelector "setTrainingStyle:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:dataSource:@
nodeWithSource_dataSourceSelector :: Selector
nodeWithSource_dataSourceSelector = mkSelector "nodeWithSource:dataSource:"

-- | @Selector@ for @initWithSource:dataSource:@
initWithSource_dataSourceSelector :: Selector
initWithSource_dataSourceSelector = mkSelector "initWithSource:dataSource:"

-- | @Selector@ for @trainingStyle@
trainingStyleSelector :: Selector
trainingStyleSelector = mkSelector "trainingStyle"

-- | @Selector@ for @setTrainingStyle:@
setTrainingStyleSelector :: Selector
setTrainingStyleSelector = mkSelector "setTrainingStyle:"

