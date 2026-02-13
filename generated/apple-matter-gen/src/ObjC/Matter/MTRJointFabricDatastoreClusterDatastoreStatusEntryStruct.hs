{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct(..)
  , state
  , setState
  , updateTimestamp
  , setUpdateTimestamp
  , failureCode
  , setFailureCode
  , failureCodeSelector
  , setFailureCodeSelector
  , setStateSelector
  , setUpdateTimestampSelector
  , stateSelector
  , updateTimestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- state@
state :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
state mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct stateSelector

-- | @- setState:@
setState :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setState mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct setStateSelector (toNSNumber value)

-- | @- updateTimestamp@
updateTimestamp :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
updateTimestamp mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct updateTimestampSelector

-- | @- setUpdateTimestamp:@
setUpdateTimestamp :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setUpdateTimestamp mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct setUpdateTimestampSelector (toNSNumber value)

-- | @- failureCode@
failureCode :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
failureCode mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct failureCodeSelector

-- | @- setFailureCode:@
setFailureCode :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setFailureCode mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct setFailureCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSNumber)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSNumber] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @updateTimestamp@
updateTimestampSelector :: Selector '[] (Id NSNumber)
updateTimestampSelector = mkSelector "updateTimestamp"

-- | @Selector@ for @setUpdateTimestamp:@
setUpdateTimestampSelector :: Selector '[Id NSNumber] ()
setUpdateTimestampSelector = mkSelector "setUpdateTimestamp:"

-- | @Selector@ for @failureCode@
failureCodeSelector :: Selector '[] (Id NSNumber)
failureCodeSelector = mkSelector "failureCode"

-- | @Selector@ for @setFailureCode:@
setFailureCodeSelector :: Selector '[Id NSNumber] ()
setFailureCodeSelector = mkSelector "setFailureCode:"

