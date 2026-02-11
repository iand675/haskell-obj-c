{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNStateNode
--
-- A placeholder node denoting the position in the graph of a MPSState object
--
-- Some filters need additional information about an image in order to function. For example              a max-pooling gradient filter needs to know which position the max result came from in the              original pooling filter in order to select the right data for gradient computation.  In other cases,              state may be moved into a MPSState object in order to keep the filter itself immutable.              The MPSState object typically encapsulates one or more MTLResource objects.
--
-- Generated bindings for @MPSNNStateNode@.
module ObjC.MetalPerformanceShaders.MPSNNStateNode
  ( MPSNNStateNode
  , IsMPSNNStateNode(..)
  , init_
  , exportFromGraph
  , setExportFromGraph
  , synchronizeResource
  , setSynchronizeResource
  , initSelector
  , exportFromGraphSelector
  , setExportFromGraphSelector
  , synchronizeResourceSelector
  , setSynchronizeResourceSelector


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

-- | @- init@
init_ :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO (Id MPSNNStateNode)
init_ mpsnnStateNode  =
  sendMsg mpsnnStateNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Tag a state node for view later
--
-- Most state nodes are private to the graph. These alias memory heavily and              consequently generally have invalid state when the graph exits.  When              exportFromGraph = YES, the image is preserved and made available through              the [MPSNNGraph encode... resultStates:... list.
--
-- CAUTION: exporting an state from a graph prevents MPS from                       recycling memory. It will nearly always cause the                       amount of memory used by the graph to increase by the size                       of the state. There will probably be a performance                       regression accordingly.  This feature should generally                       be used only when the node is needed as an input for                       further work and recomputing it is prohibitively costly.
--
-- Default: NO
--
-- ObjC selector: @- exportFromGraph@
exportFromGraph :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO Bool
exportFromGraph mpsnnStateNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnStateNode (mkSelector "exportFromGraph") retCULong []

-- | Tag a state node for view later
--
-- Most state nodes are private to the graph. These alias memory heavily and              consequently generally have invalid state when the graph exits.  When              exportFromGraph = YES, the image is preserved and made available through              the [MPSNNGraph encode... resultStates:... list.
--
-- CAUTION: exporting an state from a graph prevents MPS from                       recycling memory. It will nearly always cause the                       amount of memory used by the graph to increase by the size                       of the state. There will probably be a performance                       regression accordingly.  This feature should generally                       be used only when the node is needed as an input for                       further work and recomputing it is prohibitively costly.
--
-- Default: NO
--
-- ObjC selector: @- setExportFromGraph:@
setExportFromGraph :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> Bool -> IO ()
setExportFromGraph mpsnnStateNode  value =
  sendMsg mpsnnStateNode (mkSelector "setExportFromGraph:") retVoid [argCULong (if value then 1 else 0)]

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- Ignored on non-MacOS.
--
-- ObjC selector: @- synchronizeResource@
synchronizeResource :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO Bool
synchronizeResource mpsnnStateNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnStateNode (mkSelector "synchronizeResource") retCULong []

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- Ignored on non-MacOS.
--
-- ObjC selector: @- setSynchronizeResource:@
setSynchronizeResource :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> Bool -> IO ()
setSynchronizeResource mpsnnStateNode  value =
  sendMsg mpsnnStateNode (mkSelector "setSynchronizeResource:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

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

