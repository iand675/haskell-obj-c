{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNImageNode
--
-- A placeholder node denoting the position of a MPSImage in a graph
--
-- MPS neural network graphs are made up of filter nodes connected by              image (or state) nodes. An image node is produced by one filter but              may be consumed by more than one filter.
--
-- Most image nodes will be created by MPS and made available through              MPSNNFilterNode.resultImage. Image nodes that are not created by MPS              (i.e. "the graph inputs") must be created by you.
--
-- Generated bindings for @MPSNNImageNode@.
module ObjC.MetalPerformanceShaders.MPSNNImageNode
  ( MPSNNImageNode
  , IsMPSNNImageNode(..)
  , initWithHandle
  , nodeWithHandle
  , exportedNodeWithHandle
  , init_
  , handle
  , setHandle
  , format
  , setFormat
  , imageAllocator
  , setImageAllocator
  , exportFromGraph
  , setExportFromGraph
  , synchronizeResource
  , setSynchronizeResource
  , stopGradient
  , setStopGradient
  , initWithHandleSelector
  , nodeWithHandleSelector
  , exportedNodeWithHandleSelector
  , initSelector
  , handleSelector
  , setHandleSelector
  , formatSelector
  , setFormatSelector
  , imageAllocatorSelector
  , setImageAllocatorSelector
  , exportFromGraphSelector
  , setExportFromGraphSelector
  , synchronizeResourceSelector
  , setSynchronizeResourceSelector
  , stopGradientSelector
  , setStopGradientSelector

  -- * Enum types
  , MPSImageFeatureChannelFormat(MPSImageFeatureChannelFormat)
  , pattern MPSImageFeatureChannelFormatNone
  , pattern MPSImageFeatureChannelFormatUnorm8
  , pattern MPSImageFeatureChannelFormatUnorm16
  , pattern MPSImageFeatureChannelFormatFloat16
  , pattern MPSImageFeatureChannelFormatFloat32
  , pattern MPSImageFeatureChannelFormat_reserved0
  , pattern MPSImageFeatureChannelFormatCount

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

-- | @- initWithHandle:@
initWithHandle :: (IsMPSNNImageNode mpsnnImageNode, IsNSObject handle) => mpsnnImageNode -> handle -> IO (Id MPSNNImageNode)
initWithHandle mpsnnImageNode  handle =
  withObjCPtr handle $ \raw_handle ->
      sendMsg mpsnnImageNode (mkSelector "initWithHandle:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ())] >>= ownedObject . castPtr

-- | @+ nodeWithHandle:@
nodeWithHandle :: IsNSObject handle => handle -> IO (Id MPSNNImageNode)
nodeWithHandle handle =
  do
    cls' <- getRequiredClass "MPSNNImageNode"
    withObjCPtr handle $ \raw_handle ->
      sendClassMsg cls' (mkSelector "nodeWithHandle:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ())] >>= retainedObject . castPtr

-- | Create a autoreleased MPSNNImageNode with exportFromGraph = YES.
--
-- Note: image is still temporary. See MPSNNImageNode.imageAllocator parameter.
--
-- ObjC selector: @+ exportedNodeWithHandle:@
exportedNodeWithHandle :: IsNSObject handle => handle -> IO (Id MPSNNImageNode)
exportedNodeWithHandle handle =
  do
    cls' <- getRequiredClass "MPSNNImageNode"
    withObjCPtr handle $ \raw_handle ->
      sendClassMsg cls' (mkSelector "exportedNodeWithHandle:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO (Id MPSNNImageNode)
init_ mpsnnImageNode  =
    sendMsg mpsnnImageNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | MPS resource identifier
--
-- See MPSHandle protocol description.  Default: nil
--
-- ObjC selector: @- handle@
handle :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO RawId
handle mpsnnImageNode  =
    fmap (RawId . castPtr) $ sendMsg mpsnnImageNode (mkSelector "handle") (retPtr retVoid) []

-- | MPS resource identifier
--
-- See MPSHandle protocol description.  Default: nil
--
-- ObjC selector: @- setHandle:@
setHandle :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> RawId -> IO ()
setHandle mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setHandle:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The preferred precision for the image
--
-- Default: MPSImageFeatureChannelFormatNone, meaning MPS should pick a format                       Typically, this is 16-bit floating-point.
--
-- ObjC selector: @- format@
format :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO MPSImageFeatureChannelFormat
format mpsnnImageNode  =
    fmap (coerce :: CULong -> MPSImageFeatureChannelFormat) $ sendMsg mpsnnImageNode (mkSelector "format") retCULong []

-- | The preferred precision for the image
--
-- Default: MPSImageFeatureChannelFormatNone, meaning MPS should pick a format                       Typically, this is 16-bit floating-point.
--
-- ObjC selector: @- setFormat:@
setFormat :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> MPSImageFeatureChannelFormat -> IO ()
setFormat mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setFormat:") retVoid [argCULong (coerce value)]

-- | Configurability for image allocation
--
-- Allows you to influence how the image is allocated              Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- imageAllocator@
imageAllocator :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO RawId
imageAllocator mpsnnImageNode  =
    fmap (RawId . castPtr) $ sendMsg mpsnnImageNode (mkSelector "imageAllocator") (retPtr retVoid) []

-- | Configurability for image allocation
--
-- Allows you to influence how the image is allocated              Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- setImageAllocator:@
setImageAllocator :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> RawId -> IO ()
setImageAllocator mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setImageAllocator:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Tag a image node for view later
--
-- Most image nodes are private to the graph. These alias memory heavily and              consequently generally have invalid state when the graph exits.  When              exportFromGraph = YES, the image is preserved and made available through              the [MPSNNGraph encode... intermediateImages:... list.
--
-- CAUTION: exporting an image from a graph prevents MPS from                        recycling memory. It will nearly always cause the                       amount of memory used by the graph to increase by the size                       of the image. There will probably be a performance                       regression accordingly.  This feature should generally                       be used only when the node is needed as an input for                       further work and recomputing it is prohibitively costly.
--
-- Default: NO
--
-- ObjC selector: @- exportFromGraph@
exportFromGraph :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO Bool
exportFromGraph mpsnnImageNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnImageNode (mkSelector "exportFromGraph") retCULong []

-- | Tag a image node for view later
--
-- Most image nodes are private to the graph. These alias memory heavily and              consequently generally have invalid state when the graph exits.  When              exportFromGraph = YES, the image is preserved and made available through              the [MPSNNGraph encode... intermediateImages:... list.
--
-- CAUTION: exporting an image from a graph prevents MPS from                        recycling memory. It will nearly always cause the                       amount of memory used by the graph to increase by the size                       of the image. There will probably be a performance                       regression accordingly.  This feature should generally                       be used only when the node is needed as an input for                       further work and recomputing it is prohibitively costly.
--
-- Default: NO
--
-- ObjC selector: @- setExportFromGraph:@
setExportFromGraph :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> Bool -> IO ()
setExportFromGraph mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setExportFromGraph:") retVoid [argCULong (if value then 1 else 0)]

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- It is not needed on iOS/tvOS devices, where it does nothing.
--
-- ObjC selector: @- synchronizeResource@
synchronizeResource :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO Bool
synchronizeResource mpsnnImageNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnImageNode (mkSelector "synchronizeResource") retCULong []

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- It is not needed on iOS/tvOS devices, where it does nothing.
--
-- ObjC selector: @- setSynchronizeResource:@
setSynchronizeResource :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> Bool -> IO ()
setSynchronizeResource mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setSynchronizeResource:") retVoid [argCULong (if value then 1 else 0)]

-- | Stop training graph automatic creation at this node.
--
-- An inference graph of MPSNNFilterNodes, MPSNNStateNodes and MPSNNImageNodes can be automatically              converted to a training graph using -[MPSNNFilterNode trainingGraphWithSourceGradient:nodeHandler:].              Sometimes, an inference graph may contain extra nodes at start to do operations like resampling or range              adjustment that should not be part of the training graph. To prevent gradient operations for these extra              nodes from being included in the training graph, set <undesired node>.resultImage.stopGradient = YES.              This will prevent gradient propagation beyond this MPSNNImageNode.              Default: NO
--
-- ObjC selector: @- stopGradient@
stopGradient :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> IO Bool
stopGradient mpsnnImageNode  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnImageNode (mkSelector "stopGradient") retCULong []

-- | Stop training graph automatic creation at this node.
--
-- An inference graph of MPSNNFilterNodes, MPSNNStateNodes and MPSNNImageNodes can be automatically              converted to a training graph using -[MPSNNFilterNode trainingGraphWithSourceGradient:nodeHandler:].              Sometimes, an inference graph may contain extra nodes at start to do operations like resampling or range              adjustment that should not be part of the training graph. To prevent gradient operations for these extra              nodes from being included in the training graph, set <undesired node>.resultImage.stopGradient = YES.              This will prevent gradient propagation beyond this MPSNNImageNode.              Default: NO
--
-- ObjC selector: @- setStopGradient:@
setStopGradient :: IsMPSNNImageNode mpsnnImageNode => mpsnnImageNode -> Bool -> IO ()
setStopGradient mpsnnImageNode  value =
    sendMsg mpsnnImageNode (mkSelector "setStopGradient:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHandle:@
initWithHandleSelector :: Selector
initWithHandleSelector = mkSelector "initWithHandle:"

-- | @Selector@ for @nodeWithHandle:@
nodeWithHandleSelector :: Selector
nodeWithHandleSelector = mkSelector "nodeWithHandle:"

-- | @Selector@ for @exportedNodeWithHandle:@
exportedNodeWithHandleSelector :: Selector
exportedNodeWithHandleSelector = mkSelector "exportedNodeWithHandle:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @handle@
handleSelector :: Selector
handleSelector = mkSelector "handle"

-- | @Selector@ for @setHandle:@
setHandleSelector :: Selector
setHandleSelector = mkSelector "setHandle:"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @imageAllocator@
imageAllocatorSelector :: Selector
imageAllocatorSelector = mkSelector "imageAllocator"

-- | @Selector@ for @setImageAllocator:@
setImageAllocatorSelector :: Selector
setImageAllocatorSelector = mkSelector "setImageAllocator:"

-- | @Selector@ for @exportFromGraph@
exportFromGraphSelector :: Selector
exportFromGraphSelector = mkSelector "exportFromGraph"

-- | @Selector@ for @setExportFromGraph:@
setExportFromGraphSelector :: Selector
setExportFromGraphSelector = mkSelector "setExportFromGraph:"

-- | @Selector@ for @synchronizeResource@
synchronizeResourceSelector :: Selector
synchronizeResourceSelector = mkSelector "synchronizeResource"

-- | @Selector@ for @setSynchronizeResource:@
setSynchronizeResourceSelector :: Selector
setSynchronizeResourceSelector = mkSelector "setSynchronizeResource:"

-- | @Selector@ for @stopGradient@
stopGradientSelector :: Selector
stopGradientSelector = mkSelector "stopGradient"

-- | @Selector@ for @setStopGradient:@
setStopGradientSelector :: Selector
setStopGradientSelector = mkSelector "setStopGradient:"

