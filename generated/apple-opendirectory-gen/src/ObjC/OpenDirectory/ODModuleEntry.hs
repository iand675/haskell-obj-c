{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ODModuleEntry@.
module ObjC.OpenDirectory.ODModuleEntry
  ( ODModuleEntry
  , IsODModuleEntry(..)
  , moduleEntryWithName_xpcServiceName
  , setOption_value
  , option
  , mappings
  , setMappings
  , supportedOptions
  , name
  , setName
  , xpcServiceName
  , setXpcServiceName
  , uuidString
  , setUuidString
  , mappingsSelector
  , moduleEntryWithName_xpcServiceNameSelector
  , nameSelector
  , optionSelector
  , setMappingsSelector
  , setNameSelector
  , setOption_valueSelector
  , setUuidStringSelector
  , setXpcServiceNameSelector
  , supportedOptionsSelector
  , uuidStringSelector
  , xpcServiceNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | moduleEntryWithName:xpcServiceName:
--
-- Creates a new module entry with a given name and service.
--
-- Creates a new module entry with a given name and service.
--
-- ObjC selector: @+ moduleEntryWithName:xpcServiceName:@
moduleEntryWithName_xpcServiceName :: (IsNSString name, IsNSString xpcServiceName) => name -> xpcServiceName -> IO (Id ODModuleEntry)
moduleEntryWithName_xpcServiceName name xpcServiceName =
  do
    cls' <- getRequiredClass "ODModuleEntry"
    sendClassMessage cls' moduleEntryWithName_xpcServiceNameSelector (toNSString name) (toNSString xpcServiceName)

-- | setOption:value:
--
-- Assigns a particular option for this module.
--
-- Options are dictated by the module and can be queried via [module supportedOptions].
--
-- ObjC selector: @- setOption:value:@
setOption_value :: (IsODModuleEntry odModuleEntry, IsNSString optionName) => odModuleEntry -> optionName -> RawId -> IO ()
setOption_value odModuleEntry optionName value =
  sendMessage odModuleEntry setOption_valueSelector (toNSString optionName) value

-- | option:
--
-- Fetches the current setting for the requested option.
--
-- Fetches the current setting for the requested option.
--
-- ObjC selector: @- option:@
option :: (IsODModuleEntry odModuleEntry, IsNSString optionName) => odModuleEntry -> optionName -> IO RawId
option odModuleEntry optionName =
  sendMessage odModuleEntry optionSelector (toNSString optionName)

-- | @- mappings@
mappings :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id ODMappings)
mappings odModuleEntry =
  sendMessage odModuleEntry mappingsSelector

-- | @- setMappings:@
setMappings :: (IsODModuleEntry odModuleEntry, IsODMappings value) => odModuleEntry -> value -> IO ()
setMappings odModuleEntry value =
  sendMessage odModuleEntry setMappingsSelector (toODMappings value)

-- | @- supportedOptions@
supportedOptions :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSArray)
supportedOptions odModuleEntry =
  sendMessage odModuleEntry supportedOptionsSelector

-- | @- name@
name :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
name odModuleEntry =
  sendMessage odModuleEntry nameSelector

-- | @- setName:@
setName :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setName odModuleEntry value =
  sendMessage odModuleEntry setNameSelector (toNSString value)

-- | @- xpcServiceName@
xpcServiceName :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
xpcServiceName odModuleEntry =
  sendMessage odModuleEntry xpcServiceNameSelector

-- | @- setXpcServiceName:@
setXpcServiceName :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setXpcServiceName odModuleEntry value =
  sendMessage odModuleEntry setXpcServiceNameSelector (toNSString value)

-- | @- uuidString@
uuidString :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
uuidString odModuleEntry =
  sendMessage odModuleEntry uuidStringSelector

-- | @- setUuidString:@
setUuidString :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setUuidString odModuleEntry value =
  sendMessage odModuleEntry setUuidStringSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moduleEntryWithName:xpcServiceName:@
moduleEntryWithName_xpcServiceNameSelector :: Selector '[Id NSString, Id NSString] (Id ODModuleEntry)
moduleEntryWithName_xpcServiceNameSelector = mkSelector "moduleEntryWithName:xpcServiceName:"

-- | @Selector@ for @setOption:value:@
setOption_valueSelector :: Selector '[Id NSString, RawId] ()
setOption_valueSelector = mkSelector "setOption:value:"

-- | @Selector@ for @option:@
optionSelector :: Selector '[Id NSString] RawId
optionSelector = mkSelector "option:"

-- | @Selector@ for @mappings@
mappingsSelector :: Selector '[] (Id ODMappings)
mappingsSelector = mkSelector "mappings"

-- | @Selector@ for @setMappings:@
setMappingsSelector :: Selector '[Id ODMappings] ()
setMappingsSelector = mkSelector "setMappings:"

-- | @Selector@ for @supportedOptions@
supportedOptionsSelector :: Selector '[] (Id NSArray)
supportedOptionsSelector = mkSelector "supportedOptions"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @xpcServiceName@
xpcServiceNameSelector :: Selector '[] (Id NSString)
xpcServiceNameSelector = mkSelector "xpcServiceName"

-- | @Selector@ for @setXpcServiceName:@
setXpcServiceNameSelector :: Selector '[Id NSString] ()
setXpcServiceNameSelector = mkSelector "setXpcServiceName:"

-- | @Selector@ for @uuidString@
uuidStringSelector :: Selector '[] (Id NSString)
uuidStringSelector = mkSelector "uuidString"

-- | @Selector@ for @setUuidString:@
setUuidStringSelector :: Selector '[Id NSString] ()
setUuidStringSelector = mkSelector "setUuidString:"

