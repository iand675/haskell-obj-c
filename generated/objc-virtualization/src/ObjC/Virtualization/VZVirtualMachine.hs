{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VZVirtualMachine represents the entire state of a single virtual machine.
--
-- A Virtual Machine is the emulation of a complete hardware machine of the same architecture as the real hardware machine.    When executing the Virtual Machine, the Virtualization framework uses certain hardware resources and emulates others to provide isolation    and great performance.
--
-- The definition of a virtual machine starts with its configuration. This is done by setting up a VZVirtualMachineConfiguration object.    Once configured, the virtual machine can be started with [VZVirtualMachine startWithCompletionHandler:].
--
-- To install macOS on a virtual machine, configure a new virtual machine with a suitable VZMacPlatformConfiguration, then use a VZMacOSInstaller    to install the restore image on it.
--
-- Creating a virtual machine using the Virtualization framework requires the app to have the "com.apple.security.virtualization" entitlement.
--
-- VZVirtualMachineConfiguration
--
-- VZMacOSInstaller
--
-- Generated bindings for @VZVirtualMachine@.
module ObjC.Virtualization.VZVirtualMachine
  ( VZVirtualMachine
  , IsVZVirtualMachine(..)
  , new
  , init_
  , initWithConfiguration
  , initWithConfiguration_queue
  , startWithCompletionHandler
  , startWithOptions_completionHandler
  , stopWithCompletionHandler
  , pauseWithCompletionHandler
  , resumeWithCompletionHandler
  , restoreMachineStateFromURL_completionHandler
  , saveMachineStateToURL_completionHandler
  , requestStopWithError
  , queue
  , supported
  , state
  , canStart
  , canStop
  , canPause
  , canResume
  , canRequestStop
  , memoryBalloonDevices
  , socketDevices
  , newSelector
  , initSelector
  , initWithConfigurationSelector
  , initWithConfiguration_queueSelector
  , startWithCompletionHandlerSelector
  , startWithOptions_completionHandlerSelector
  , stopWithCompletionHandlerSelector
  , pauseWithCompletionHandlerSelector
  , resumeWithCompletionHandlerSelector
  , restoreMachineStateFromURL_completionHandlerSelector
  , saveMachineStateToURL_completionHandlerSelector
  , requestStopWithErrorSelector
  , queueSelector
  , supportedSelector
  , stateSelector
  , canStartSelector
  , canStopSelector
  , canPauseSelector
  , canResumeSelector
  , canRequestStopSelector
  , memoryBalloonDevicesSelector
  , socketDevicesSelector

  -- * Enum types
  , VZVirtualMachineState(VZVirtualMachineState)
  , pattern VZVirtualMachineStateStopped
  , pattern VZVirtualMachineStateRunning
  , pattern VZVirtualMachineStatePaused
  , pattern VZVirtualMachineStateError
  , pattern VZVirtualMachineStateStarting
  , pattern VZVirtualMachineStatePausing
  , pattern VZVirtualMachineStateResuming
  , pattern VZVirtualMachineStateStopping
  , pattern VZVirtualMachineStateSaving
  , pattern VZVirtualMachineStateRestoring

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

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZVirtualMachine)
new  =
  do
    cls' <- getRequiredClass "VZVirtualMachine"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO (Id VZVirtualMachine)
init_ vzVirtualMachine  =
  sendMsg vzVirtualMachine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize the virtual machine.
--
-- This initializer uses the main queue to operate the virtual machine. Every call must be done on the main queue and the callbacks are invoked    on the main queue.
--
-- @configuration@ — The configuration of the virtual machine.    The configuration must be valid. Validation can be performed at runtime with [VZVirtualMachineConfiguration validateWithError:].    The configuration is copied by the initializer.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsVZVirtualMachine vzVirtualMachine, IsVZVirtualMachineConfiguration configuration) => vzVirtualMachine -> configuration -> IO (Id VZVirtualMachine)
initWithConfiguration vzVirtualMachine  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg vzVirtualMachine (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the virtual machine.
--
-- @configuration@ — The configuration of the virtual machine.    The configuration must be valid. Validation can be performed at runtime with [VZVirtualMachineConfiguration validateWithError:].    The configuration is copied by the initializer.
--
-- @queue@ — The serial queue on which the virtual machine operates.    Every operation on the virtual machine must be done on that queue. The callbacks and delegate methods are invoked on that queue.    If the queue is not serial, the behavior is undefined.
--
-- ObjC selector: @- initWithConfiguration:queue:@
initWithConfiguration_queue :: (IsVZVirtualMachine vzVirtualMachine, IsVZVirtualMachineConfiguration configuration, IsNSObject queue) => vzVirtualMachine -> configuration -> queue -> IO (Id VZVirtualMachine)
initWithConfiguration_queue vzVirtualMachine  configuration queue =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr queue $ \raw_queue ->
      sendMsg vzVirtualMachine (mkSelector "initWithConfiguration:queue:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | Start a virtual machine.
--
-- Start a virtual machine that is in either Stopped or Error state.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully started or on error.    The error parameter passed to the block is nil if the start was successful.
--
-- ObjC selector: @- startWithCompletionHandler:@
startWithCompletionHandler :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> Ptr () -> IO ()
startWithCompletionHandler vzVirtualMachine  completionHandler =
  sendMsg vzVirtualMachine (mkSelector "startWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Start a virtual machine with options.
--
-- Start a virtual machine that is in either Stopped or Error state.
--
-- @options@ — Options used to control how the virtual machine is started.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully started or on error.    The error parameter passed to the block is nil if the start was successful.
--
-- VZMacOSVirtualMachineStartOptions
--
-- ObjC selector: @- startWithOptions:completionHandler:@
startWithOptions_completionHandler :: (IsVZVirtualMachine vzVirtualMachine, IsVZVirtualMachineStartOptions options) => vzVirtualMachine -> options -> Ptr () -> IO ()
startWithOptions_completionHandler vzVirtualMachine  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vzVirtualMachine (mkSelector "startWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Stop a virtual machine.
--
-- Stop a virtual machine that is in either Running or Paused state.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully stopped or on error.    The error parameter passed to the block is nil if the stop was successful.
--
-- This is a destructive operation. It stops the virtual machine without giving the guest a chance to stop cleanly.
--
-- -[VZVirtualMachine requestStopWithError:]
--
-- ObjC selector: @- stopWithCompletionHandler:@
stopWithCompletionHandler :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> Ptr () -> IO ()
stopWithCompletionHandler vzVirtualMachine  completionHandler =
  sendMsg vzVirtualMachine (mkSelector "stopWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Pause a virtual machine.
--
-- Pause a virtual machine that is in Running state.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully paused or on error.    The error parameter passed to the block is nil if the pause was successful.
--
-- ObjC selector: @- pauseWithCompletionHandler:@
pauseWithCompletionHandler :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> Ptr () -> IO ()
pauseWithCompletionHandler vzVirtualMachine  completionHandler =
  sendMsg vzVirtualMachine (mkSelector "pauseWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Resume a virtual machine.
--
-- Resume a virtual machine that is in the Paused state.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully resumed or on error.    The error parameter passed to the block is nil if the resumption was successful.
--
-- ObjC selector: @- resumeWithCompletionHandler:@
resumeWithCompletionHandler :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> Ptr () -> IO ()
resumeWithCompletionHandler vzVirtualMachine  completionHandler =
  sendMsg vzVirtualMachine (mkSelector "resumeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Restore a virtual machine.
--
-- Restore a stopped virtual machine to a state previously saved to file through @saveMachineStateToURL:completionHandler:@.
--
-- If the file cannot be read, or contains otherwise invalid contents, this operation will fail with a @VZErrorRestore@ error.    If the virtual machine is not in the stopped state, this operation will fail with a @VZErrorInvalidVirtualMachineStateTransition@ error.    If the virtual machine cannot be started due to an internal error, this operation will fail with a @VZErrorInternal@ error.    The @VZVirtualMachineConfiguration@ must also support restoring, which can be checked with  @-[VZVirtualMachineConfiguration validateSaveRestoreSupportWithError:]@.
--
-- If this operation fails, the virtual machine state is unchanged.    If successful, the virtual machine is restored and placed in the paused state.
--
-- @saveFileURL@ — URL to file containing saved state of a suspended virtual machine.    The file must have been generated by @saveMachineStateToURL:completionHandler:@ on the same host.    Otherwise, this operation will fail with a @VZErrorRestore@ error indicating a permission denied failure reason.
--
-- The virtual machine must also be configured compatibly with the state contained in the file.    If the @VZVirtualMachineConfiguration@ is not compatible with the content of the file, this operation will fail with a @VZErrorRestore@ error indicating an invalid argument failure reason.
--
-- Files generated with @saveMachineStateToURL:completionHandler:@ on a software version that is newer than the current version will also be rejected with an invalid argument failure reason.    In some cases, @restoreMachineStateFromURL:completionHandler:@ can fail if a software update has changed the host in a way that would be incompatible with the previous format.    In this case, an invalid argument error will be surfaced. In most cases, the virtual machine should be restarted with @startWithCompletionHandler:@.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully started or on error.    The error parameter passed to the block is nil if the restore was successful.
--
-- See: -[VZVirtualMachineConfiguration validateSaveRestoreSupportWithError:]
--
-- ObjC selector: @- restoreMachineStateFromURL:completionHandler:@
restoreMachineStateFromURL_completionHandler :: (IsVZVirtualMachine vzVirtualMachine, IsNSURL saveFileURL) => vzVirtualMachine -> saveFileURL -> Ptr () -> IO ()
restoreMachineStateFromURL_completionHandler vzVirtualMachine  saveFileURL completionHandler =
withObjCPtr saveFileURL $ \raw_saveFileURL ->
    sendMsg vzVirtualMachine (mkSelector "restoreMachineStateFromURL:completionHandler:") retVoid [argPtr (castPtr raw_saveFileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Save a virtual machine.
--
-- Save a paused virtual machine to file.    The contents of this file can be used later to restore the state of the paused virtual machine.
--
-- If the virtual machine is not paused, this operation will fail with a @VZErrorInvalidVirtualMachineState@ error.    If the virtual machine cannot be saved, this operation will fail with a @VZErrorSave@ error.    The @VZVirtualMachineConfiguration@ must also support saving, which can be checked with  @-[VZVirtualMachineConfiguration validateSaveRestoreSupportWithError:]@.
--
-- If this operation fails, the virtual machine state is unchanged.    If successful, the file is written out and the virtual machine state is unchanged.
--
-- @saveFileURL@ — URL to location where the saved state of the virtual machine is to be written.    Each file is protected by an encryption key that is tied to the host on which it is created.
--
-- @completionHandler@ — Block called after the virtual machine has been successfully saved or on error.    The error parameter passed to the block is nil if the save was successful.
--
-- See: -[VZVirtualMachineConfiguration validateSaveRestoreSupportWithError:]
--
-- ObjC selector: @- saveMachineStateToURL:completionHandler:@
saveMachineStateToURL_completionHandler :: (IsVZVirtualMachine vzVirtualMachine, IsNSURL saveFileURL) => vzVirtualMachine -> saveFileURL -> Ptr () -> IO ()
saveMachineStateToURL_completionHandler vzVirtualMachine  saveFileURL completionHandler =
withObjCPtr saveFileURL $ \raw_saveFileURL ->
    sendMsg vzVirtualMachine (mkSelector "saveMachineStateToURL:completionHandler:") retVoid [argPtr (castPtr raw_saveFileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Request that the guest turns itself off.
--
-- @error@ — If not nil, assigned with the error if the request failed.
--
-- Returns: YES if the request was made successfully.
--
-- The -[VZVirtualMachineDelegate guestDidStopVirtualMachine:] delegate method is invoked when the guest has turned itself off.
--
-- -[VZVirtualMachineDelegate guestDidStopVirtualMachine:].
--
-- ObjC selector: @- requestStopWithError:@
requestStopWithError :: (IsVZVirtualMachine vzVirtualMachine, IsNSError error_) => vzVirtualMachine -> error_ -> IO Bool
requestStopWithError vzVirtualMachine  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "requestStopWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | The queue associated with this virtual machine.
--
-- This property is a reference to the queue used to create the virtual machine.    If no queue was passed, the default queue is the main queue.
--
-- The property can be accessed from any queue or actor.
--
-- Other properties or function calls on the VZVirtualMachine must happen on this queue.    The completion handlers from the asynchronous functions are also invoked on this queue.
--
-- ObjC selector: @- queue@
queue :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO (Id NSObject)
queue vzVirtualMachine  =
  sendMsg vzVirtualMachine (mkSelector "queue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicate whether or not virtualization is available.
--
-- If virtualization is unavailable, no VZVirtualMachineConfiguration will validate.    The validation error of the VZVirtualMachineConfiguration provides more information about why virtualization is unavailable.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VZVirtualMachine"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | Execution state of the virtual machine.
--
-- ObjC selector: @- state@
state :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO VZVirtualMachineState
state vzVirtualMachine  =
  fmap (coerce :: CLong -> VZVirtualMachineState) $ sendMsg vzVirtualMachine (mkSelector "state") retCLong []

-- | Return YES if the machine is in a state that can be started.
--
-- See: -[VZVirtualMachine startWithCompletionHandler:].
--
-- See: -[VZVirtualMachine state]
--
-- ObjC selector: @- canStart@
canStart :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO Bool
canStart vzVirtualMachine  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "canStart") retCULong []

-- | Return YES if the machine is in a state that can be stopped.
--
-- See: -[VZVirtualMachine stopWithCompletionHandler:]
--
-- See: -[VZVirtualMachine state]
--
-- ObjC selector: @- canStop@
canStop :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO Bool
canStop vzVirtualMachine  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "canStop") retCULong []

-- | Return YES if the machine is in a state that can be paused.
--
-- See: -[VZVirtualMachine pauseWithCompletionHandler:]
--
-- See: -[VZVirtualMachine state]
--
-- ObjC selector: @- canPause@
canPause :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO Bool
canPause vzVirtualMachine  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "canPause") retCULong []

-- | Return YES if the machine is in a state that can be resumed.
--
-- See: -[VZVirtualMachine resumeWithCompletionHandler:]
--
-- See: -[VZVirtualMachine state]
--
-- ObjC selector: @- canResume@
canResume :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO Bool
canResume vzVirtualMachine  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "canResume") retCULong []

-- | Returns whether the machine is in a state where the guest can be asked to stop.
--
-- See: -[VZVirtualMachine requestStopWithError:]
--
-- See: -[VZVirtualMachine state]
--
-- ObjC selector: @- canRequestStop@
canRequestStop :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO Bool
canRequestStop vzVirtualMachine  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachine (mkSelector "canRequestStop") retCULong []

-- | Return the list of memory balloon devices configured on this virtual machine. Return an empty array if no memory balloon device is configured.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- See: VZVirtualMachineConfiguration
--
-- ObjC selector: @- memoryBalloonDevices@
memoryBalloonDevices :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO (Id NSArray)
memoryBalloonDevices vzVirtualMachine  =
  sendMsg vzVirtualMachine (mkSelector "memoryBalloonDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return the list of socket devices configured on this virtual machine. Return an empty array if no socket device is configured.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- See: VZVirtualMachineConfiguration
--
-- ObjC selector: @- socketDevices@
socketDevices :: IsVZVirtualMachine vzVirtualMachine => vzVirtualMachine -> IO (Id NSArray)
socketDevices vzVirtualMachine  =
  sendMsg vzVirtualMachine (mkSelector "socketDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @initWithConfiguration:queue:@
initWithConfiguration_queueSelector :: Selector
initWithConfiguration_queueSelector = mkSelector "initWithConfiguration:queue:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @startWithOptions:completionHandler:@
startWithOptions_completionHandlerSelector :: Selector
startWithOptions_completionHandlerSelector = mkSelector "startWithOptions:completionHandler:"

-- | @Selector@ for @stopWithCompletionHandler:@
stopWithCompletionHandlerSelector :: Selector
stopWithCompletionHandlerSelector = mkSelector "stopWithCompletionHandler:"

-- | @Selector@ for @pauseWithCompletionHandler:@
pauseWithCompletionHandlerSelector :: Selector
pauseWithCompletionHandlerSelector = mkSelector "pauseWithCompletionHandler:"

-- | @Selector@ for @resumeWithCompletionHandler:@
resumeWithCompletionHandlerSelector :: Selector
resumeWithCompletionHandlerSelector = mkSelector "resumeWithCompletionHandler:"

-- | @Selector@ for @restoreMachineStateFromURL:completionHandler:@
restoreMachineStateFromURL_completionHandlerSelector :: Selector
restoreMachineStateFromURL_completionHandlerSelector = mkSelector "restoreMachineStateFromURL:completionHandler:"

-- | @Selector@ for @saveMachineStateToURL:completionHandler:@
saveMachineStateToURL_completionHandlerSelector :: Selector
saveMachineStateToURL_completionHandlerSelector = mkSelector "saveMachineStateToURL:completionHandler:"

-- | @Selector@ for @requestStopWithError:@
requestStopWithErrorSelector :: Selector
requestStopWithErrorSelector = mkSelector "requestStopWithError:"

-- | @Selector@ for @queue@
queueSelector :: Selector
queueSelector = mkSelector "queue"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @canStart@
canStartSelector :: Selector
canStartSelector = mkSelector "canStart"

-- | @Selector@ for @canStop@
canStopSelector :: Selector
canStopSelector = mkSelector "canStop"

-- | @Selector@ for @canPause@
canPauseSelector :: Selector
canPauseSelector = mkSelector "canPause"

-- | @Selector@ for @canResume@
canResumeSelector :: Selector
canResumeSelector = mkSelector "canResume"

-- | @Selector@ for @canRequestStop@
canRequestStopSelector :: Selector
canRequestStopSelector = mkSelector "canRequestStop"

-- | @Selector@ for @memoryBalloonDevices@
memoryBalloonDevicesSelector :: Selector
memoryBalloonDevicesSelector = mkSelector "memoryBalloonDevices"

-- | @Selector@ for @socketDevices@
socketDevicesSelector :: Selector
socketDevicesSelector = mkSelector "socketDevices"

