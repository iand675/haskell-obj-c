{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockUserChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockUserChangeEvent
  ( MTRDoorLockClusterAppleAliroLockUserChangeEvent
  , IsMTRDoorLockClusterAppleAliroLockUserChangeEvent(..)
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
lockDataType :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
lockDataType mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent lockDataTypeSelector

-- | @- setLockDataType:@
setLockDataType :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setLockDataType mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setLockDataTypeSelector (toNSNumber value)

-- | @- dataOperationType@
dataOperationType :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
dataOperationType mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent dataOperationTypeSelector

-- | @- setDataOperationType:@
setDataOperationType :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setDataOperationType mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setDataOperationTypeSelector (toNSNumber value)

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent operationSourceSelector

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setOperationSourceSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setFabricIndexSelector (toNSNumber value)

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent sourceNodeSelector

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setSourceNodeSelector (toNSNumber value)

-- | @- dataIndex@
dataIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
dataIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent dataIndexSelector

-- | @- setDataIndex:@
setDataIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setDataIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockUserChangeEvent setDataIndexSelector (toNSNumber value)

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

