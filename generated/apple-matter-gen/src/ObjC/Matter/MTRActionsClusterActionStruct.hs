{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterActionStruct@.
module ObjC.Matter.MTRActionsClusterActionStruct
  ( MTRActionsClusterActionStruct
  , IsMTRActionsClusterActionStruct(..)
  , actionID
  , setActionID
  , name
  , setName
  , type_
  , setType
  , endpointListID
  , setEndpointListID
  , supportedCommands
  , setSupportedCommands
  , state
  , setState
  , actionIDSelector
  , endpointListIDSelector
  , nameSelector
  , setActionIDSelector
  , setEndpointListIDSelector
  , setNameSelector
  , setStateSelector
  , setSupportedCommandsSelector
  , setTypeSelector
  , stateSelector
  , supportedCommandsSelector
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

-- | @- actionID@
actionID :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
actionID mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setActionID mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setActionIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSString)
name mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct nameSelector

-- | @- setName:@
setName :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSString value) => mtrActionsClusterActionStruct -> value -> IO ()
setName mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setNameSelector (toNSString value)

-- | @- type@
type_ :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
type_ mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct typeSelector

-- | @- setType:@
setType :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setType mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setTypeSelector (toNSNumber value)

-- | @- endpointListID@
endpointListID :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
endpointListID mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct endpointListIDSelector

-- | @- setEndpointListID:@
setEndpointListID :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setEndpointListID mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setEndpointListIDSelector (toNSNumber value)

-- | @- supportedCommands@
supportedCommands :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
supportedCommands mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct supportedCommandsSelector

-- | @- setSupportedCommands:@
setSupportedCommands :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setSupportedCommands mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setSupportedCommandsSelector (toNSNumber value)

-- | @- state@
state :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
state mtrActionsClusterActionStruct =
  sendMessage mtrActionsClusterActionStruct stateSelector

-- | @- setState:@
setState :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setState mtrActionsClusterActionStruct value =
  sendMessage mtrActionsClusterActionStruct setStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector '[] (Id NSNumber)
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector '[Id NSNumber] ()
setActionIDSelector = mkSelector "setActionID:"

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

-- | @Selector@ for @endpointListID@
endpointListIDSelector :: Selector '[] (Id NSNumber)
endpointListIDSelector = mkSelector "endpointListID"

-- | @Selector@ for @setEndpointListID:@
setEndpointListIDSelector :: Selector '[Id NSNumber] ()
setEndpointListIDSelector = mkSelector "setEndpointListID:"

-- | @Selector@ for @supportedCommands@
supportedCommandsSelector :: Selector '[] (Id NSNumber)
supportedCommandsSelector = mkSelector "supportedCommands"

-- | @Selector@ for @setSupportedCommands:@
setSupportedCommandsSelector :: Selector '[Id NSNumber] ()
setSupportedCommandsSelector = mkSelector "setSupportedCommands:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSNumber)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSNumber] ()
setStateSelector = mkSelector "setState:"

