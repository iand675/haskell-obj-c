{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockUserChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterLockUserChangeEvent
  ( MTRDoorLockClusterLockUserChangeEvent
  , IsMTRDoorLockClusterLockUserChangeEvent(..)
  , lockDataType
  , setLockDataType
  , dataOperationType
  , setDataOperationType
  , operationSource
  , setOperationSource
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , sourceNode
  , setSourceNode
  , dataIndex
  , setDataIndex
  , dataIndexSelector
  , dataOperationTypeSelector
  , fabricIndexSelector
  , lockDataTypeSelector
  , operationSourceSelector
  , setDataIndexSelector
  , setDataOperationTypeSelector
  , setFabricIndexSelector
  , setLockDataTypeSelector
  , setOperationSourceSelector
  , setSourceNodeSelector
  , setUserIndexSelector
  , sourceNodeSelector
  , userIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- lockDataType@
lockDataType :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
lockDataType mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent lockDataTypeSelector

-- | @- setLockDataType:@
setLockDataType :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setLockDataType mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setLockDataTypeSelector (toNSNumber value)

-- | @- dataOperationType@
dataOperationType :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
dataOperationType mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent dataOperationTypeSelector

-- | @- setDataOperationType:@
setDataOperationType :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setDataOperationType mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setDataOperationTypeSelector (toNSNumber value)

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent operationSourceSelector

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setOperationSourceSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setFabricIndexSelector (toNSNumber value)

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent sourceNodeSelector

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setSourceNodeSelector (toNSNumber value)

-- | @- dataIndex@
dataIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
dataIndex mtrDoorLockClusterLockUserChangeEvent =
  sendMessage mtrDoorLockClusterLockUserChangeEvent dataIndexSelector

-- | @- setDataIndex:@
setDataIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setDataIndex mtrDoorLockClusterLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterLockUserChangeEvent setDataIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockDataType@
lockDataTypeSelector :: Selector '[] (Id NSNumber)
lockDataTypeSelector = mkSelector "lockDataType"

-- | @Selector@ for @setLockDataType:@
setLockDataTypeSelector :: Selector '[Id NSNumber] ()
setLockDataTypeSelector = mkSelector "setLockDataType:"

-- | @Selector@ for @dataOperationType@
dataOperationTypeSelector :: Selector '[] (Id NSNumber)
dataOperationTypeSelector = mkSelector "dataOperationType"

-- | @Selector@ for @setDataOperationType:@
setDataOperationTypeSelector :: Selector '[Id NSNumber] ()
setDataOperationTypeSelector = mkSelector "setDataOperationType:"

-- | @Selector@ for @operationSource@
operationSourceSelector :: Selector '[] (Id NSNumber)
operationSourceSelector = mkSelector "operationSource"

-- | @Selector@ for @setOperationSource:@
setOperationSourceSelector :: Selector '[Id NSNumber] ()
setOperationSourceSelector = mkSelector "setOperationSource:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @sourceNode@
sourceNodeSelector :: Selector '[] (Id NSNumber)
sourceNodeSelector = mkSelector "sourceNode"

-- | @Selector@ for @setSourceNode:@
setSourceNodeSelector :: Selector '[Id NSNumber] ()
setSourceNodeSelector = mkSelector "setSourceNode:"

-- | @Selector@ for @dataIndex@
dataIndexSelector :: Selector '[] (Id NSNumber)
dataIndexSelector = mkSelector "dataIndex"

-- | @Selector@ for @setDataIndex:@
setDataIndexSelector :: Selector '[Id NSNumber] ()
setDataIndexSelector = mkSelector "setDataIndex:"

