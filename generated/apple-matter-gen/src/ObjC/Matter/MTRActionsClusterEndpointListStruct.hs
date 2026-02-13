{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterEndpointListStruct@.
module ObjC.Matter.MTRActionsClusterEndpointListStruct
  ( MTRActionsClusterEndpointListStruct
  , IsMTRActionsClusterEndpointListStruct(..)
  , endpointListID
  , setEndpointListID
  , name
  , setName
  , type_
  , setType
  , endpoints
  , setEndpoints
  , endpointListIDSelector
  , endpointsSelector
  , nameSelector
  , setEndpointListIDSelector
  , setEndpointsSelector
  , setNameSelector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- endpointListID@
endpointListID :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSNumber)
endpointListID mtrActionsClusterEndpointListStruct =
  sendMessage mtrActionsClusterEndpointListStruct endpointListIDSelector

-- | @- setEndpointListID:@
setEndpointListID :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSNumber value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setEndpointListID mtrActionsClusterEndpointListStruct value =
  sendMessage mtrActionsClusterEndpointListStruct setEndpointListIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSString)
name mtrActionsClusterEndpointListStruct =
  sendMessage mtrActionsClusterEndpointListStruct nameSelector

-- | @- setName:@
setName :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSString value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setName mtrActionsClusterEndpointListStruct value =
  sendMessage mtrActionsClusterEndpointListStruct setNameSelector (toNSString value)

-- | @- type@
type_ :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSNumber)
type_ mtrActionsClusterEndpointListStruct =
  sendMessage mtrActionsClusterEndpointListStruct typeSelector

-- | @- setType:@
setType :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSNumber value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setType mtrActionsClusterEndpointListStruct value =
  sendMessage mtrActionsClusterEndpointListStruct setTypeSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSArray)
endpoints mtrActionsClusterEndpointListStruct =
  sendMessage mtrActionsClusterEndpointListStruct endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSArray value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setEndpoints mtrActionsClusterEndpointListStruct value =
  sendMessage mtrActionsClusterEndpointListStruct setEndpointsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointListID@
endpointListIDSelector :: Selector '[] (Id NSNumber)
endpointListIDSelector = mkSelector "endpointListID"

-- | @Selector@ for @setEndpointListID:@
setEndpointListIDSelector :: Selector '[Id NSNumber] ()
setEndpointListIDSelector = mkSelector "setEndpointListID:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

