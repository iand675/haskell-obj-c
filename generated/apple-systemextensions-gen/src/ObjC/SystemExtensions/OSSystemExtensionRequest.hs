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
  , propertiesRequestForExtension_queueSelector
  , delegateSelector
  , setDelegateSelector
  , identifierSelector


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
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "activationRequestForExtension:queue:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "deactivationRequestForExtension:queue:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "propertiesRequestForExtension:queue:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

-- | A delegate to receive updates about the progress of a request
--
-- ObjC selector: @- delegate@
delegate :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> IO RawId
delegate osSystemExtensionRequest  =
    fmap (RawId . castPtr) $ sendMsg osSystemExtensionRequest (mkSelector "delegate") (retPtr retVoid) []

-- | A delegate to receive updates about the progress of a request
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> RawId -> IO ()
setDelegate osSystemExtensionRequest  value =
    sendMsg osSystemExtensionRequest (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The bundle identifier of the target extension
--
-- ObjC selector: @- identifier@
identifier :: IsOSSystemExtensionRequest osSystemExtensionRequest => osSystemExtensionRequest -> IO (Id NSString)
identifier osSystemExtensionRequest  =
    sendMsg osSystemExtensionRequest (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activationRequestForExtension:queue:@
activationRequestForExtension_queueSelector :: Selector
activationRequestForExtension_queueSelector = mkSelector "activationRequestForExtension:queue:"

-- | @Selector@ for @deactivationRequestForExtension:queue:@
deactivationRequestForExtension_queueSelector :: Selector
deactivationRequestForExtension_queueSelector = mkSelector "deactivationRequestForExtension:queue:"

-- | @Selector@ for @propertiesRequestForExtension:queue:@
propertiesRequestForExtension_queueSelector :: Selector
propertiesRequestForExtension_queueSelector = mkSelector "propertiesRequestForExtension:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

