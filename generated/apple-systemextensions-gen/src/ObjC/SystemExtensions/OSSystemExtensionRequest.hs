{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSSystemExtensionRequest@.
module ObjC.SystemExtensions.OSSystemExtensionRequest
  ( OSSystemExtensionRequest
  , IsOSSystemExtensionRequest(..)
  , activationRequestForExtension_queue
  , deactivationRequestForExtension_queue
  , propertiesRequestForExtension_queue
  , delegate
  , setDelegate
  , identifier
  , activationRequestForExtension_queueSelector
  , deactivationRequestForExtension_queueSelector
  , delegateSelector
  , identifierSelector
  , propertiesRequestForExtension_queueSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a request to activate a System Extension.
--
-- This method creates a new request to activate a System Extension. Extensions are bundles discovered from the @Contents/Library/SystemExtensions@ directory of the main application bundle.
--
-- @identifier@ — The bundle identifier of the target extension.
--
-- @queue@ — The dispatch queue to use when calling delegate methods.
--
-- Returns: A new extension request.
--
-- Note: It is expected that an application create and submit an activation request whenever an extension should be active. Upon submitting an activation request for an inactive extension, user approval may be required and the request will not succeed until approval is given.
--
-- If the extension is already active then the request will succeed in short order without significant delay or user interaction. Activating an new version of an already active extension will prompt the delegate to resolve the conflict before proceeding.
--
-- An activation request can be successful but also indicate that a reboot is required in order for the extension to become active. This can occur when replacing an existing extension that required a reboot in order to deactivate. The most recently activated extension will then become active when the system is next rebooted.
--
-- ObjC selector: @+ activationRequestForExtension:queue:@
activationRequestForExtension_queue :: (IsNSString identifier, IsNSObject queue) => identifier -> queue -> IO (Id OSSystemExtensionRequest)
activationRequestForExtension_queue identifier queue =
  do
    cls' <- getRequiredClass "OSSystemExtensionRequest"
    sendClassMessage cls' activationRequestForExtension_queueSelector (toNSString identifier) (toNSObject queue)

-- | Creates a request to deactivate a System Extension.
--
-- This method creates a new request to deactivate a System Extension. Extensions are discovered from the @Contents/Library/SystemExtensions@ directory of the main application bundle.
--
-- @identifier@ — The bundle identifier of the target extension.
--
-- @queue@ — The dispatch queue to use when calling delegate methods.
--
-- Note: It is possible for an extension to require a reboot before it is fully deactivated. If a request succeeds and indicates a reboot is required, the extension may still appear to be operational until that time.
--
-- ObjC selector: @+ deactivationRequestForExtension:queue:@
deactivationRequestForExtension_queue :: (IsNSString identifier, IsNSObject queue) => identifier -> queue -> IO (Id OSSystemExtensionRequest)
deactivationRequestForExtension_queue identifier queue =
  do
    cls' <- getRequiredClass "OSSystemExtensionRequest"
    sendClassMessage cls' deactivationRequestForExtension_queueSelector (toNSString identifier) (toNSObject queue)

-- | Creates a request to get information about System Extensions.
--
-- This method creates a new request to retrieve the properties of any System Extensions matching the given identifier.
--
-- @identifier@ — The bundle identifier of the target extension(s).
--
-- @queue@ — The dispatch queue to use when calling delegate methods.
--
-- ObjC selector: @+ propertiesRequestForExtension:queue:@
propertiesRequestForExtension_queue :: (IsNSString identifier, IsNSObject queue) => identifier -> queue -> IO (Id OSSystemExtensionRequest)
propertiesRequestForExtension_queue identifier queue =
  do
    cls' <- getRequiredClass "OSSystemExtensionRequest"
    sendClassMessage cls' propertiesRequestForExtension_queueSelector (toNSString identifier) (toNSObject queue)

-- | A delegate to receive updates about the progress of a request
--
-- ObjC selector: @- delegate@
delegate :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> IO RawId
delegate osSystemExtensionRequest =
  sendMessage osSystemExtensionRequest delegateSelector

-- | A delegate to receive updates about the progress of a request
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> RawId -> IO ()
setDelegate osSystemExtensionRequest value =
  sendMessage osSystemExtensionRequest setDelegateSelector value

-- | The bundle identifier of the target extension
--
-- ObjC selector: @- identifier@
identifier :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> IO (Id NSString)
identifier osSystemExtensionRequest =
  sendMessage osSystemExtensionRequest identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activationRequestForExtension:queue:@
activationRequestForExtension_queueSelector :: Selector '[Id NSString, Id NSObject] (Id OSSystemExtensionRequest)
activationRequestForExtension_queueSelector = mkSelector "activationRequestForExtension:queue:"

-- | @Selector@ for @deactivationRequestForExtension:queue:@
deactivationRequestForExtension_queueSelector :: Selector '[Id NSString, Id NSObject] (Id OSSystemExtensionRequest)
deactivationRequestForExtension_queueSelector = mkSelector "deactivationRequestForExtension:queue:"

-- | @Selector@ for @propertiesRequestForExtension:queue:@
propertiesRequestForExtension_queueSelector :: Selector '[Id NSString, Id NSObject] (Id OSSystemExtensionRequest)
propertiesRequestForExtension_queueSelector = mkSelector "propertiesRequestForExtension:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

