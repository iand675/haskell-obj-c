{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionProvider
--
-- A CMIOExtensionProvider describes a CoreMediaIO extension provider.
--
-- Generated bindings for @CMIOExtensionProvider@.
module ObjC.CoreMediaIO.CMIOExtensionProvider
  ( CMIOExtensionProvider
  , IsCMIOExtensionProvider(..)
  , startServiceWithProvider
  , stopServiceWithProvider
  , init_
  , new
  , providerWithSource_clientQueue
  , initWithSource_clientQueue
  , addDevice_error
  , removeDevice_error
  , notifyPropertiesChanged
  , ignoreSIGTERM
  , source
  , clientQueue
  , connectedClients
  , devices
  , startServiceWithProviderSelector
  , stopServiceWithProviderSelector
  , initSelector
  , newSelector
  , providerWithSource_clientQueueSelector
  , initWithSource_clientQueueSelector
  , addDevice_errorSelector
  , removeDevice_errorSelector
  , notifyPropertiesChangedSelector
  , ignoreSIGTERMSelector
  , sourceSelector
  , clientQueueSelector
  , connectedClientsSelector
  , devicesSelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | startServiceWithProvider:
--
-- Starts the CoreMediaIO Extension machinery.
--
-- ObjC selector: @+ startServiceWithProvider:@
startServiceWithProvider :: IsCMIOExtensionProvider provider => provider -> IO ()
startServiceWithProvider provider =
  do
    cls' <- getRequiredClass "CMIOExtensionProvider"
    withObjCPtr provider $ \raw_provider ->
      sendClassMsg cls' (mkSelector "startServiceWithProvider:") retVoid [argPtr (castPtr raw_provider :: Ptr ())]

-- | stopServiceWithProvider:
--
-- Stops the CoreMediaIO Extension machinery.
--
-- This should only be called in very rare circumstances.  For example, if an extension is present on a system and there is no possible way that it would ever create a device instance. It is suggested that this be called as soon as possible in the provider's lifecycle, and that the provider's ignoreSIGTERM method be called first. After calling this method the extension should exit.
--
-- ObjC selector: @+ stopServiceWithProvider:@
stopServiceWithProvider :: IsCMIOExtensionProvider provider => provider -> IO ()
stopServiceWithProvider provider =
  do
    cls' <- getRequiredClass "CMIOExtensionProvider"
    withObjCPtr provider $ \raw_provider ->
      sendClassMsg cls' (mkSelector "stopServiceWithProvider:") retVoid [argPtr (castPtr raw_provider :: Ptr ())]

-- | @- init@
init_ :: IsCMIOExtensionProvider cmioExtensionProvider => cmioExtensionProvider -> IO (Id CMIOExtensionProvider)
init_ cmioExtensionProvider  =
    sendMsg cmioExtensionProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionProvider)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | providerWithSource:clientQueue:
--
-- Returns a provider instance.
--
-- @source@ — The provider source.
--
-- @clientQueue@ — The client dispatch queue, or nil for the default dispatch queue.
--
-- Returns: A CMIOExtensionProvider instance.
--
-- ObjC selector: @+ providerWithSource:clientQueue:@
providerWithSource_clientQueue :: IsNSObject clientQueue => RawId -> clientQueue -> IO (Id CMIOExtensionProvider)
providerWithSource_clientQueue source clientQueue =
  do
    cls' <- getRequiredClass "CMIOExtensionProvider"
    withObjCPtr clientQueue $ \raw_clientQueue ->
      sendClassMsg cls' (mkSelector "providerWithSource:clientQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ())] >>= retainedObject . castPtr

-- | initWithSource:clientQueue:
--
-- Initialize a provider instance.
--
-- @source@ — The provider source.
--
-- @clientQueue@ — The client dispatch queue, or nil for the default dispatch queue.
--
-- Returns: A CMIOExtensionProvider instance.
--
-- ObjC selector: @- initWithSource:clientQueue:@
initWithSource_clientQueue :: (IsCMIOExtensionProvider cmioExtensionProvider, IsNSObject clientQueue) => cmioExtensionProvider -> RawId -> clientQueue -> IO (Id CMIOExtensionProvider)
initWithSource_clientQueue cmioExtensionProvider  source clientQueue =
  withObjCPtr clientQueue $ \raw_clientQueue ->
      sendMsg cmioExtensionProvider (mkSelector "initWithSource:clientQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId source) :: Ptr ()), argPtr (castPtr raw_clientQueue :: Ptr ())] >>= ownedObject . castPtr

-- | addDevice:error:
--
-- Add a device to the provider devices array.
--
-- @device@ — The device to be added to the provider devices array.
--
-- @outError@ — An error return on failure.
--
-- Returns: Return YES on success, NO otherwise.
--
-- ObjC selector: @- addDevice:error:@
addDevice_error :: (IsCMIOExtensionProvider cmioExtensionProvider, IsCMIOExtensionDevice device, IsNSError outError) => cmioExtensionProvider -> device -> outError -> IO Bool
addDevice_error cmioExtensionProvider  device outError =
  withObjCPtr device $ \raw_device ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmioExtensionProvider (mkSelector "addDevice:error:") retCULong [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | removeDevice:error:
--
-- Remove a device from the provider devices array.
--
-- @device@ — The device to be removed from the provider devices array.
--
-- @outError@ — An error return on failure.
--
-- Returns: Return YES on success, NO otherwise.
--
-- ObjC selector: @- removeDevice:error:@
removeDevice_error :: (IsCMIOExtensionProvider cmioExtensionProvider, IsCMIOExtensionDevice device, IsNSError outError) => cmioExtensionProvider -> device -> outError -> IO Bool
removeDevice_error cmioExtensionProvider  device outError =
  withObjCPtr device $ \raw_device ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmioExtensionProvider (mkSelector "removeDevice:error:") retCULong [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | notifyPropertiesChanged:
--
-- Notify client(s) of device properties changes.
--
-- @propertyStates@ — The dictionary of properties having changed.
--
-- ObjC selector: @- notifyPropertiesChanged:@
notifyPropertiesChanged :: (IsCMIOExtensionProvider cmioExtensionProvider, IsNSDictionary propertyStates) => cmioExtensionProvider -> propertyStates -> IO ()
notifyPropertiesChanged cmioExtensionProvider  propertyStates =
  withObjCPtr propertyStates $ \raw_propertyStates ->
      sendMsg cmioExtensionProvider (mkSelector "notifyPropertiesChanged:") retVoid [argPtr (castPtr raw_propertyStates :: Ptr ())]

-- | ignoreSIGTERM
--
-- Directs provider class to ignore the SIGTERM signal.
--
-- Call this method if your provider handles SIGTERM signals; NOTE: if so, your handler must call exit.
--
-- ObjC selector: @+ ignoreSIGTERM@
ignoreSIGTERM :: IO ()
ignoreSIGTERM  =
  do
    cls' <- getRequiredClass "CMIOExtensionProvider"
    sendClassMsg cls' (mkSelector "ignoreSIGTERM") retVoid []

-- | source
--
-- The provider source.
--
-- ObjC selector: @- source@
source :: IsCMIOExtensionProvider cmioExtensionProvider => cmioExtensionProvider -> IO RawId
source cmioExtensionProvider  =
    fmap (RawId . castPtr) $ sendMsg cmioExtensionProvider (mkSelector "source") (retPtr retVoid) []

-- | clientQueue
--
-- The dispatch queue on which source methods from the provider/device/stream will be called.
--
-- ObjC selector: @- clientQueue@
clientQueue :: IsCMIOExtensionProvider cmioExtensionProvider => cmioExtensionProvider -> IO (Id NSObject)
clientQueue cmioExtensionProvider  =
    sendMsg cmioExtensionProvider (mkSelector "clientQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | connectedClients
--
-- The array of connected clients.
--
-- This property is key-value observable.
--
-- ObjC selector: @- connectedClients@
connectedClients :: IsCMIOExtensionProvider cmioExtensionProvider => cmioExtensionProvider -> IO (Id NSArray)
connectedClients cmioExtensionProvider  =
    sendMsg cmioExtensionProvider (mkSelector "connectedClients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | devices
--
-- The devices array of the provider.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- devices@
devices :: IsCMIOExtensionProvider cmioExtensionProvider => cmioExtensionProvider -> IO (Id NSArray)
devices cmioExtensionProvider  =
    sendMsg cmioExtensionProvider (mkSelector "devices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startServiceWithProvider:@
startServiceWithProviderSelector :: Selector
startServiceWithProviderSelector = mkSelector "startServiceWithProvider:"

-- | @Selector@ for @stopServiceWithProvider:@
stopServiceWithProviderSelector :: Selector
stopServiceWithProviderSelector = mkSelector "stopServiceWithProvider:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @providerWithSource:clientQueue:@
providerWithSource_clientQueueSelector :: Selector
providerWithSource_clientQueueSelector = mkSelector "providerWithSource:clientQueue:"

-- | @Selector@ for @initWithSource:clientQueue:@
initWithSource_clientQueueSelector :: Selector
initWithSource_clientQueueSelector = mkSelector "initWithSource:clientQueue:"

-- | @Selector@ for @addDevice:error:@
addDevice_errorSelector :: Selector
addDevice_errorSelector = mkSelector "addDevice:error:"

-- | @Selector@ for @removeDevice:error:@
removeDevice_errorSelector :: Selector
removeDevice_errorSelector = mkSelector "removeDevice:error:"

-- | @Selector@ for @notifyPropertiesChanged:@
notifyPropertiesChangedSelector :: Selector
notifyPropertiesChangedSelector = mkSelector "notifyPropertiesChanged:"

-- | @Selector@ for @ignoreSIGTERM@
ignoreSIGTERMSelector :: Selector
ignoreSIGTERMSelector = mkSelector "ignoreSIGTERM"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @clientQueue@
clientQueueSelector :: Selector
clientQueueSelector = mkSelector "clientQueue"

-- | @Selector@ for @connectedClients@
connectedClientsSelector :: Selector
connectedClientsSelector = mkSelector "connectedClients"

-- | @Selector@ for @devices@
devicesSelector :: Selector
devicesSelector = mkSelector "devices"

