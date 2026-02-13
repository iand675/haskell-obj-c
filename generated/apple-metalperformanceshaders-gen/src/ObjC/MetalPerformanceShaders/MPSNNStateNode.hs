{-# LANGUAGE DataKinds #-}
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
  , handle
  , setHandle
  , exportFromGraph
  , setExportFromGraph
  , synchronizeResource
  , setSynchronizeResource
  , exportFromGraphSelector
  , handleSelector
  , initSelector
  , setExportFromGraphSelector
  , setHandleSelector
  , setSynchronizeResourceSelector
  , synchronizeResourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO (Id MPSNNStateNode)
init_ mpsnnStateNode =
  sendOwnedMessage mpsnnStateNode initSelector

-- | MPS resource identification
--
-- See MPSHandle protocol reference.  Default: nil
--
-- ObjC selector: @- handle@
handle :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO RawId
handle mpsnnStateNode =
  sendMessage mpsnnStateNode handleSelector

-- | MPS resource identification
--
-- See MPSHandle protocol reference.  Default: nil
--
-- ObjC selector: @- setHandle:@
setHandle :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> RawId -> IO ()
setHandle mpsnnStateNode value =
  sendMessage mpsnnStateNode setHandleSelector value

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
exportFromGraph mpsnnStateNode =
  sendMessage mpsnnStateNode exportFromGraphSelector

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
setExportFromGraph mpsnnStateNode value =
  sendMessage mpsnnStateNode setExportFromGraphSelector value

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- Ignored on non-MacOS.
--
-- ObjC selector: @- synchronizeResource@
synchronizeResource :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> IO Bool
synchronizeResource mpsnnStateNode =
  sendMessage mpsnnStateNode synchronizeResourceSelector

-- | Set to true to cause the resource to be synchronized with the CPU
--
-- Ignored on non-MacOS.
--
-- ObjC selector: @- setSynchronizeResource:@
setSynchronizeResource :: IsMPSNNStateNode mpsnnStateNode => mpsnnStateNode -> Bool -> IO ()
setSynchronizeResource mpsnnStateNode value =
  sendMessage mpsnnStateNode setSynchronizeResourceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNNStateNode)
initSelector = mkSelector "init"

-- | @Selector@ for @handle@
handleSelector :: Selector '[] RawId
handleSelector = mkSelector "handle"

-- | @Selector@ for @setHandle:@
setHandleSelector :: Selector '[RawId] ()
setHandleSelector = mkSelector "setHandle:"

-- | @Selector@ for @exportFromGraph@
exportFromGraphSelector :: Selector '[] Bool
exportFromGraphSelector = mkSelector "exportFromGraph"

-- | @Selector@ for @setExportFromGraph:@
setExportFromGraphSelector :: Selector '[Bool] ()
setExportFromGraphSelector = mkSelector "setExportFromGraph:"

-- | @Selector@ for @synchronizeResource@
synchronizeResourceSelector :: Selector '[] Bool
synchronizeResourceSelector = mkSelector "synchronizeResource"

-- | @Selector@ for @setSynchronizeResource:@
setSynchronizeResourceSelector :: Selector '[Bool] ()
setSynchronizeResourceSelector = mkSelector "setSynchronizeResource:"

