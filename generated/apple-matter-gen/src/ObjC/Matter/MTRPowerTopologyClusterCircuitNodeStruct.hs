{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerTopologyClusterCircuitNodeStruct@.
module ObjC.Matter.MTRPowerTopologyClusterCircuitNodeStruct
  ( MTRPowerTopologyClusterCircuitNodeStruct
  , IsMTRPowerTopologyClusterCircuitNodeStruct(..)
  , node
  , setNode
  , endpoint
  , setEndpoint
  , label
  , setLabel
  , fabricIndex
  , setFabricIndex
  , endpointSelector
  , fabricIndexSelector
  , labelSelector
  , nodeSelector
  , setEndpointSelector
  , setFabricIndexSelector
  , setLabelSelector
  , setNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- node@
node :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
node mtrPowerTopologyClusterCircuitNodeStruct =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct nodeSelector

-- | @- setNode:@
setNode :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setNode mtrPowerTopologyClusterCircuitNodeStruct value =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct setNodeSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
endpoint mtrPowerTopologyClusterCircuitNodeStruct =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setEndpoint mtrPowerTopologyClusterCircuitNodeStruct value =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct setEndpointSelector (toNSNumber value)

-- | @- label@
label :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSString)
label mtrPowerTopologyClusterCircuitNodeStruct =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSString value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setLabel mtrPowerTopologyClusterCircuitNodeStruct value =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct setLabelSelector (toNSString value)

-- | @- fabricIndex@
fabricIndex :: IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct => mtrPowerTopologyClusterCircuitNodeStruct -> IO (Id NSNumber)
fabricIndex mtrPowerTopologyClusterCircuitNodeStruct =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRPowerTopologyClusterCircuitNodeStruct mtrPowerTopologyClusterCircuitNodeStruct, IsNSNumber value) => mtrPowerTopologyClusterCircuitNodeStruct -> value -> IO ()
setFabricIndex mtrPowerTopologyClusterCircuitNodeStruct value =
  sendMessage mtrPowerTopologyClusterCircuitNodeStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id NSNumber)
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector '[Id NSNumber] ()
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

