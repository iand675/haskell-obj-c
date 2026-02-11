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
  , moduleEntryWithName_xpcServiceNameSelector
  , setOption_valueSelector
  , optionSelector
  , mappingsSelector
  , setMappingsSelector
  , supportedOptionsSelector
  , nameSelector
  , setNameSelector
  , xpcServiceNameSelector
  , setXpcServiceNameSelector
  , uuidStringSelector
  , setUuidStringSelector


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
    withObjCPtr name $ \raw_name ->
      withObjCPtr xpcServiceName $ \raw_xpcServiceName ->
        sendClassMsg cls' (mkSelector "moduleEntryWithName:xpcServiceName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_xpcServiceName :: Ptr ())] >>= retainedObject . castPtr

-- | setOption:value:
--
-- Assigns a particular option for this module.
--
-- Options are dictated by the module and can be queried via [module supportedOptions].
--
-- ObjC selector: @- setOption:value:@
setOption_value :: (IsODModuleEntry odModuleEntry, IsNSString optionName) => odModuleEntry -> optionName -> RawId -> IO ()
setOption_value odModuleEntry  optionName value =
withObjCPtr optionName $ \raw_optionName ->
    sendMsg odModuleEntry (mkSelector "setOption:value:") retVoid [argPtr (castPtr raw_optionName :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ())]

-- | option:
--
-- Fetches the current setting for the requested option.
--
-- Fetches the current setting for the requested option.
--
-- ObjC selector: @- option:@
option :: (IsODModuleEntry odModuleEntry, IsNSString optionName) => odModuleEntry -> optionName -> IO RawId
option odModuleEntry  optionName =
withObjCPtr optionName $ \raw_optionName ->
    fmap (RawId . castPtr) $ sendMsg odModuleEntry (mkSelector "option:") (retPtr retVoid) [argPtr (castPtr raw_optionName :: Ptr ())]

-- | @- mappings@
mappings :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id ODMappings)
mappings odModuleEntry  =
  sendMsg odModuleEntry (mkSelector "mappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMappings:@
setMappings :: (IsODModuleEntry odModuleEntry, IsODMappings value) => odModuleEntry -> value -> IO ()
setMappings odModuleEntry  value =
withObjCPtr value $ \raw_value ->
    sendMsg odModuleEntry (mkSelector "setMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedOptions@
supportedOptions :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSArray)
supportedOptions odModuleEntry  =
  sendMsg odModuleEntry (mkSelector "supportedOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
name odModuleEntry  =
  sendMsg odModuleEntry (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setName odModuleEntry  value =
withObjCPtr value $ \raw_value ->
    sendMsg odModuleEntry (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- xpcServiceName@
xpcServiceName :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
xpcServiceName odModuleEntry  =
  sendMsg odModuleEntry (mkSelector "xpcServiceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setXpcServiceName:@
setXpcServiceName :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setXpcServiceName odModuleEntry  value =
withObjCPtr value $ \raw_value ->
    sendMsg odModuleEntry (mkSelector "setXpcServiceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uuidString@
uuidString :: IsODModuleEntry odModuleEntry => odModuleEntry -> IO (Id NSString)
uuidString odModuleEntry  =
  sendMsg odModuleEntry (mkSelector "uuidString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUuidString:@
setUuidString :: (IsODModuleEntry odModuleEntry, IsNSString value) => odModuleEntry -> value -> IO ()
setUuidString odModuleEntry  value =
withObjCPtr value $ \raw_value ->
    sendMsg odModuleEntry (mkSelector "setUuidString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moduleEntryWithName:xpcServiceName:@
moduleEntryWithName_xpcServiceNameSelector :: Selector
moduleEntryWithName_xpcServiceNameSelector = mkSelector "moduleEntryWithName:xpcServiceName:"

-- | @Selector@ for @setOption:value:@
setOption_valueSelector :: Selector
setOption_valueSelector = mkSelector "setOption:value:"

-- | @Selector@ for @option:@
optionSelector :: Selector
optionSelector = mkSelector "option:"

-- | @Selector@ for @mappings@
mappingsSelector :: Selector
mappingsSelector = mkSelector "mappings"

-- | @Selector@ for @setMappings:@
setMappingsSelector :: Selector
setMappingsSelector = mkSelector "setMappings:"

-- | @Selector@ for @supportedOptions@
supportedOptionsSelector :: Selector
supportedOptionsSelector = mkSelector "supportedOptions"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @xpcServiceName@
xpcServiceNameSelector :: Selector
xpcServiceNameSelector = mkSelector "xpcServiceName"

-- | @Selector@ for @setXpcServiceName:@
setXpcServiceNameSelector :: Selector
setXpcServiceNameSelector = mkSelector "setXpcServiceName:"

-- | @Selector@ for @uuidString@
uuidStringSelector :: Selector
uuidStringSelector = mkSelector "uuidString"

-- | @Selector@ for @setUuidString:@
setUuidStringSelector :: Selector
setUuidStringSelector = mkSelector "setUuidString:"

