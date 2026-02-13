{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceController@.
module ObjC.Matter.MTRDeviceController
  ( MTRDeviceController
  , IsMTRDeviceController(..)
  , init_
  , new
  , initWithParameters_error
  , setupCommissioningSessionWithPayload_newNodeID_error
  , setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error
  , commissionNodeWithID_commissioningParams_error
  , continueCommissioningDevice_ignoreAttestationFailure_error
  , cancelCommissioningForNodeID_error
  , deviceBeingCommissionedWithNodeID_error
  , preWarmCommissioningSession
  , setDeviceControllerDelegate_queue
  , addDeviceControllerDelegate_queue
  , removeDeviceControllerDelegate
  , startBrowseForCommissionables_queue
  , stopBrowseForCommissionables
  , attestationChallengeForDeviceID
  , addServerEndpoint
  , removeServerEndpoint_queue_completion
  , removeServerEndpoint
  , forgetDeviceWithNodeID
  , computePASEVerifierForSetupPasscode_iterations_salt_error
  , suspend
  , resume
  , shutdown
  , sharedControllerWithId_xpcConnectBlock
  , sharedControllerWithID_xpcConnectBlock
  , encodeXPCResponseValues
  , decodeXPCResponseValues
  , encodeXPCReadParams
  , decodeXPCReadParams
  , encodeXPCSubscribeParams
  , decodeXPCSubscribeParams
  , xpcInterfaceForServerProtocol
  , xpcInterfaceForClientProtocol
  , fetchAttestationChallengeForDeviceId
  , getBaseDevice_queue_completionHandler
  , pairDevice_discriminator_setupPINCode_error
  , pairDevice_address_port_setupPINCode_error
  , pairDevice_onboardingPayload_error
  , commissionDevice_commissioningParams_error
  , stopDevicePairing_error
  , getDeviceBeingCommissioned_error
  , openPairingWindow_duration_error
  , openPairingWindowWithPIN_duration_discriminator_setupPIN_error
  , computePaseVerifier_iterations_salt
  , setPairingDelegate_queue
  , setNocChainIssuer_queue
  , running
  , suspended
  , uniqueIdentifier
  , controllerNodeID
  , devices
  , nodesWithStoredData
  , controllerNodeId
  , addDeviceControllerDelegate_queueSelector
  , addServerEndpointSelector
  , attestationChallengeForDeviceIDSelector
  , cancelCommissioningForNodeID_errorSelector
  , commissionDevice_commissioningParams_errorSelector
  , commissionNodeWithID_commissioningParams_errorSelector
  , computePASEVerifierForSetupPasscode_iterations_salt_errorSelector
  , computePaseVerifier_iterations_saltSelector
  , continueCommissioningDevice_ignoreAttestationFailure_errorSelector
  , controllerNodeIDSelector
  , controllerNodeIdSelector
  , decodeXPCReadParamsSelector
  , decodeXPCResponseValuesSelector
  , decodeXPCSubscribeParamsSelector
  , deviceBeingCommissionedWithNodeID_errorSelector
  , devicesSelector
  , encodeXPCReadParamsSelector
  , encodeXPCResponseValuesSelector
  , encodeXPCSubscribeParamsSelector
  , fetchAttestationChallengeForDeviceIdSelector
  , forgetDeviceWithNodeIDSelector
  , getBaseDevice_queue_completionHandlerSelector
  , getDeviceBeingCommissioned_errorSelector
  , initSelector
  , initWithParameters_errorSelector
  , newSelector
  , nodesWithStoredDataSelector
  , openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector
  , openPairingWindow_duration_errorSelector
  , pairDevice_address_port_setupPINCode_errorSelector
  , pairDevice_discriminator_setupPINCode_errorSelector
  , pairDevice_onboardingPayload_errorSelector
  , preWarmCommissioningSessionSelector
  , removeDeviceControllerDelegateSelector
  , removeServerEndpointSelector
  , removeServerEndpoint_queue_completionSelector
  , resumeSelector
  , runningSelector
  , setDeviceControllerDelegate_queueSelector
  , setNocChainIssuer_queueSelector
  , setPairingDelegate_queueSelector
  , setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector
  , setupCommissioningSessionWithPayload_newNodeID_errorSelector
  , sharedControllerWithID_xpcConnectBlockSelector
  , sharedControllerWithId_xpcConnectBlockSelector
  , shutdownSelector
  , startBrowseForCommissionables_queueSelector
  , stopBrowseForCommissionablesSelector
  , stopDevicePairing_errorSelector
  , suspendSelector
  , suspendedSelector
  , uniqueIdentifierSelector
  , xpcInterfaceForClientProtocolSelector
  , xpcInterfaceForServerProtocolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Controllers are created via the MTRDeviceControllerFactory object or initialized via initWithParameters:error:.
--
-- ObjC selector: @- init@
init_ :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id MTRDeviceController)
init_ mtrDeviceController =
  sendOwnedMessage mtrDeviceController initSelector

-- | @+ new@
new :: IO (Id MTRDeviceController)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendOwnedClassMessage cls' newSelector

-- | Initialize a device controller with the provided parameters.  This will:
--
-- 1) Auto-start the MTRDeviceControllerFactory in storage-per-controller mode    if it has not already been started. 2) Return nil or a running controller.
--
-- Once this returns non-nil, it's the caller's responsibility to call shutdown on the controller to avoid leaking it.
--
-- ObjC selector: @- initWithParameters:error:@
initWithParameters_error :: (IsMTRDeviceController mtrDeviceController, IsMTRDeviceControllerAbstractParameters parameters, IsNSError error_) => mtrDeviceController -> parameters -> error_ -> IO (Id MTRDeviceController)
initWithParameters_error mtrDeviceController parameters error_ =
  sendOwnedMessage mtrDeviceController initWithParameters_errorSelector (toMTRDeviceControllerAbstractParameters parameters) (toNSError error_)

-- | Set up a commissioning session for a device, using the provided setup payload to discover it and connect to it.
--
-- @payload@ — a setup payload (probably created from a QR code or numeric                code onboarding payload).
--
-- @newNodeID@ — the planned node id for the node.  error indication if discovery can't start at all (e.g. because the              setup payload is invalid).
--
-- The IP and port for the device will be discovered automatically based on the provided discriminator.
--
-- Then a PASE session will be established with the device, unless an error occurs.  MTRDeviceControllerDelegate will be notified as follows:
--
-- * Discovery fails: controller:statusUpdate: with MTRCommissioningStatusFailed.
--
-- * Commissioning session setup fails:   controller:commissioningSessionEstablishmentDone: with non-nil error.
--
-- * Commissioning session setup succeeds:   controller:commissioningSessionEstablishmentDone: with nil error.
--
-- Once a commissioning session is set up, getDeviceBeingCommissioned can be used to get an MTRBaseDevice and discover what sort of network credentials the device might need, and commissionDevice can be used to commission the device.
--
-- ObjC selector: @- setupCommissioningSessionWithPayload:newNodeID:error:@
setupCommissioningSessionWithPayload_newNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsMTRSetupPayload payload, IsNSNumber newNodeID, IsNSError error_) => mtrDeviceController -> payload -> newNodeID -> error_ -> IO Bool
setupCommissioningSessionWithPayload_newNodeID_error mtrDeviceController payload newNodeID error_ =
  sendMessage mtrDeviceController setupCommissioningSessionWithPayload_newNodeID_errorSelector (toMTRSetupPayload payload) (toNSNumber newNodeID) (toNSError error_)

-- | Set up a commissioning session for a device, using the provided discovered result to connect to it.
--
-- @discoveredDevice@ — a previously discovered device.
--
-- @payload@ — a setup payload (probably created from a QR code or numeric                code onboarding payload).
--
-- @newNodeID@ — the planned node id for the node.  error indication if the commissioning session establishment can't start at all.
--
-- The connection information for the device will be retrieved from the discovered device. A device discovered over DNS-SD will use the discovered IPs/ports, while a device discovered over BLE will use the underlying CBPeripheral.
--
-- Then a PASE session will be established with the device, unless an error occurs.  MTRDeviceControllerDelegate will be notified as follows:
--
-- * Invalid connection information: controller:statusUpdate: with MTRCommissioningStatusFailed.
--
-- * Commissioning session setup fails:   controller:commissioningSessionEstablishmentDone: with non-nil error.
--
-- * Commissioning session setup succeeds:   controller:commissioningSessionEstablishmentDone: with nil error.
--
-- Once a commissioning session is set up, getDeviceBeingCommissioned can be used to get an MTRBaseDevice and discover what sort of network credentials the device might need, and commissionDevice can be used to commission the device.
--
-- ObjC selector: @- setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:@
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsMTRCommissionableBrowserResult discoveredDevice, IsMTRSetupPayload payload, IsNSNumber newNodeID, IsNSError error_) => mtrDeviceController -> discoveredDevice -> payload -> newNodeID -> error_ -> IO Bool
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_error mtrDeviceController discoveredDevice payload newNodeID error_ =
  sendMessage mtrDeviceController setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector (toMTRCommissionableBrowserResult discoveredDevice) (toMTRSetupPayload payload) (toNSNumber newNodeID) (toNSError error_)

-- | Commission the node with the given node ID.  The node ID must match the node ID that was used to set up the commissioning session.
--
-- NOTE: The forceWiFiScan and forceThreadScan properties of MTRCommissioningParameters are ignored by this API.
--
-- ObjC selector: @- commissionNodeWithID:commissioningParams:error:@
commissionNodeWithID_commissioningParams_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsMTRCommissioningParameters commissioningParams, IsNSError error_) => mtrDeviceController -> nodeID -> commissioningParams -> error_ -> IO Bool
commissionNodeWithID_commissioningParams_error mtrDeviceController nodeID commissioningParams error_ =
  sendMessage mtrDeviceController commissionNodeWithID_commissioningParams_errorSelector (toNSNumber nodeID) (toMTRCommissioningParameters commissioningParams) (toNSError error_)

-- | Call this method after MTRDeviceAttestationDelegate deviceAttestationFailedForController:opaqueDeviceHandle:error: or deviceAttestationCompletedForController:opaqueDeviceHandle:attestationDeviceInfo:error: is called to continue commissioning the device.
--
-- ObjC selector: @- continueCommissioningDevice:ignoreAttestationFailure:error:@
continueCommissioningDevice_ignoreAttestationFailure_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> Ptr () -> Bool -> error_ -> IO Bool
continueCommissioningDevice_ignoreAttestationFailure_error mtrDeviceController opaqueDeviceHandle ignoreAttestationFailure error_ =
  sendMessage mtrDeviceController continueCommissioningDevice_ignoreAttestationFailure_errorSelector opaqueDeviceHandle ignoreAttestationFailure (toNSError error_)

-- | Cancel commissioning for the given node id.  This will shut down any existing commissioning session for that node id.
--
-- ObjC selector: @- cancelCommissioningForNodeID:error:@
cancelCommissioningForNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsNSError error_) => mtrDeviceController -> nodeID -> error_ -> IO Bool
cancelCommissioningForNodeID_error mtrDeviceController nodeID error_ =
  sendMessage mtrDeviceController cancelCommissioningForNodeID_errorSelector (toNSNumber nodeID) (toNSError error_)

-- | Get an MTRBaseDevice for a commissioning session that was set up for the given node ID.  Returns nil if no such commissioning session is available.
--
-- ObjC selector: @- deviceBeingCommissionedWithNodeID:error:@
deviceBeingCommissionedWithNodeID_error :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID, IsNSError error_) => mtrDeviceController -> nodeID -> error_ -> IO (Id MTRBaseDevice)
deviceBeingCommissionedWithNodeID_error mtrDeviceController nodeID error_ =
  sendMessage mtrDeviceController deviceBeingCommissionedWithNodeID_errorSelector (toNSNumber nodeID) (toNSError error_)

-- | @- preWarmCommissioningSession@
preWarmCommissioningSession :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
preWarmCommissioningSession mtrDeviceController =
  sendMessage mtrDeviceController preWarmCommissioningSessionSelector

-- | Set the Delegate for the device controller as well as the Queue on which the Delegate callbacks will be triggered
--
-- @delegate@ — The delegate the commissioning process should use
--
-- @queue@ — The queue on which the callbacks will be delivered
--
-- ObjC selector: @- setDeviceControllerDelegate:queue:@
setDeviceControllerDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setDeviceControllerDelegate_queue mtrDeviceController delegate queue =
  sendMessage mtrDeviceController setDeviceControllerDelegate_queueSelector delegate (toNSObject queue)

-- | Adds a Delegate to the device controller as well as the Queue on which the Delegate callbacks will be triggered
--
-- Multiple delegates can be added to monitor MTRDeviceController state changes. Note that there should only be one delegate that responds to pairing related callbacks.
--
-- If a delegate is added a second time, the call would be ignored.
--
-- All delegates are held by weak references, and so if a delegate object goes away, it will be automatically removed.
--
-- @delegate@ — The delegate the commissioning process should use
--
-- @queue@ — The queue on which the callbacks will be delivered
--
-- ObjC selector: @- addDeviceControllerDelegate:queue:@
addDeviceControllerDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
addDeviceControllerDelegate_queue mtrDeviceController delegate queue =
  sendMessage mtrDeviceController addDeviceControllerDelegate_queueSelector delegate (toNSObject queue)

-- | Removes a Delegate from the device controller
--
-- @delegate@ — The delegate to be removed
--
-- ObjC selector: @- removeDeviceControllerDelegate:@
removeDeviceControllerDelegate :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> RawId -> IO ()
removeDeviceControllerDelegate mtrDeviceController delegate =
  sendMessage mtrDeviceController removeDeviceControllerDelegateSelector delegate

-- | Start scanning for commissionable devices.
--
-- This method will fail if the controller factory is not running or the browse has already been started.
--
-- ObjC selector: @- startBrowseForCommissionables:queue:@
startBrowseForCommissionables_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO Bool
startBrowseForCommissionables_queue mtrDeviceController delegate queue =
  sendMessage mtrDeviceController startBrowseForCommissionables_queueSelector delegate (toNSObject queue)

-- | Stop scanning for commissionable devices.
--
-- This method will fail if the controller factory is not running or the browse has not been started.
--
-- ObjC selector: @- stopBrowseForCommissionables@
stopBrowseForCommissionables :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
stopBrowseForCommissionables mtrDeviceController =
  sendMessage mtrDeviceController stopBrowseForCommissionablesSelector

-- | Return the attestation challenge for the secure session of the device being commissioned.
--
-- Attempts to retrieve the attestation challenge for a commissionee with the given Device ID. Returns nil if given Device ID does not match an active commissionee, or if a Secure Session is not availale.
--
-- ObjC selector: @- attestationChallengeForDeviceID:@
attestationChallengeForDeviceID :: (IsMTRDeviceController mtrDeviceController, IsNSNumber deviceID) => mtrDeviceController -> deviceID -> IO (Id NSData)
attestationChallengeForDeviceID mtrDeviceController deviceID =
  sendMessage mtrDeviceController attestationChallengeForDeviceIDSelector (toNSNumber deviceID)

-- | Add a server endpoint for this controller.  The endpoint starts off enabled.
--
-- Will fail in the following cases:
--
-- 1) There is already an endpoint defined with the given endpoint id. 2) There are too many endpoints defined already.
--
-- ObjC selector: @- addServerEndpoint:@
addServerEndpoint :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint) => mtrDeviceController -> endpoint -> IO Bool
addServerEndpoint mtrDeviceController endpoint =
  sendMessage mtrDeviceController addServerEndpointSelector (toMTRServerEndpoint endpoint)

-- | Remove the given server endpoint from this controller.  If the endpoint is not attached to this controller, will just call the completion and do nothing else.
--
-- ObjC selector: @- removeServerEndpoint:queue:completion:@
removeServerEndpoint_queue_completion :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint, IsNSObject queue) => mtrDeviceController -> endpoint -> queue -> Ptr () -> IO ()
removeServerEndpoint_queue_completion mtrDeviceController endpoint queue completion =
  sendMessage mtrDeviceController removeServerEndpoint_queue_completionSelector (toMTRServerEndpoint endpoint) (toNSObject queue) completion

-- | Remove the given server endpoint without being notified when the removal completes.
--
-- ObjC selector: @- removeServerEndpoint:@
removeServerEndpoint :: (IsMTRDeviceController mtrDeviceController, IsMTRServerEndpoint endpoint) => mtrDeviceController -> endpoint -> IO ()
removeServerEndpoint mtrDeviceController endpoint =
  sendMessage mtrDeviceController removeServerEndpointSelector (toMTRServerEndpoint endpoint)

-- | Forget any information we have about the device with the given node ID.  That includes clearing any information we have stored about it.
--
-- ObjC selector: @- forgetDeviceWithNodeID:@
forgetDeviceWithNodeID :: (IsMTRDeviceController mtrDeviceController, IsNSNumber nodeID) => mtrDeviceController -> nodeID -> IO ()
forgetDeviceWithNodeID mtrDeviceController nodeID =
  sendMessage mtrDeviceController forgetDeviceWithNodeIDSelector (toNSNumber nodeID)

-- | Compute a PASE verifier for the desired setup passcode.
--
-- @setupPasscode@ — The desired passcode to use.
--
-- @iterations@ — The number of iterations to use when generating the verifier.
--
-- @salt@ — The 16-byte salt for verifier computation.
--
-- Returns nil on errors (e.g. salt has the wrong size), otherwise the computed verifier bytes.
--
-- ObjC selector: @+ computePASEVerifierForSetupPasscode:iterations:salt:error:@
computePASEVerifierForSetupPasscode_iterations_salt_error :: (IsNSNumber setupPasscode, IsNSNumber iterations, IsNSData salt, IsNSError error_) => setupPasscode -> iterations -> salt -> error_ -> IO (Id NSData)
computePASEVerifierForSetupPasscode_iterations_salt_error setupPasscode iterations salt error_ =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' computePASEVerifierForSetupPasscode_iterations_salt_errorSelector (toNSNumber setupPasscode) (toNSNumber iterations) (toNSData salt) (toNSError error_)

-- | Suspend the controller.  This will attempt to stop all network traffic associated with the controller.  The controller will remain suspended until it is resumed.
--
-- Suspending an already-suspended controller has no effect.
--
-- ObjC selector: @- suspend@
suspend :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
suspend mtrDeviceController =
  sendMessage mtrDeviceController suspendSelector

-- | Resume the controller.  This has no effect if the controller is not suspended.
--
-- A resume following any number of suspend calls will resume the controller; there does not need to be a resume call to match every suspend call.
--
-- ObjC selector: @- resume@
resume :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
resume mtrDeviceController =
  sendMessage mtrDeviceController resumeSelector

-- | Shut down the controller. Calls to shutdown after the first one are NO-OPs. This must be called, either directly or via shutting down the MTRDeviceControllerFactory, to avoid leaking the controller.
--
-- ObjC selector: @- shutdown@
shutdown :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO ()
shutdown mtrDeviceController =
  sendMessage mtrDeviceController shutdownSelector

-- | @+ sharedControllerWithId:xpcConnectBlock:@
sharedControllerWithId_xpcConnectBlock :: RawId -> Ptr () -> IO (Id MTRDeviceController)
sharedControllerWithId_xpcConnectBlock controllerID xpcConnectBlock =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' sharedControllerWithId_xpcConnectBlockSelector controllerID xpcConnectBlock

-- | Returns a shared device controller proxy for the controller object over XPC connection.
--
-- @controllerID@ — an implementation specific id in case multiple shared device controllers are available over XPC connection
--
-- @xpcConnectBlock@ — block to connect to an XPC listener serving the shared device controllers in an implementation specific way
--
-- ObjC selector: @+ sharedControllerWithID:xpcConnectBlock:@
sharedControllerWithID_xpcConnectBlock :: RawId -> Ptr () -> IO (Id MTRDeviceController)
sharedControllerWithID_xpcConnectBlock controllerID xpcConnectBlock =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' sharedControllerWithID_xpcConnectBlockSelector controllerID xpcConnectBlock

-- | Returns an encoded values object to send over XPC for read, write and command interactions
--
-- ObjC selector: @+ encodeXPCResponseValues:@
encodeXPCResponseValues :: IsNSArray values => values -> IO (Id NSArray)
encodeXPCResponseValues values =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' encodeXPCResponseValuesSelector (toNSArray values)

-- | Returns a decoded values object from a values object received from XPC for read, write and command interactions
--
-- ObjC selector: @+ decodeXPCResponseValues:@
decodeXPCResponseValues :: IsNSArray values => values -> IO (Id NSArray)
decodeXPCResponseValues values =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' decodeXPCResponseValuesSelector (toNSArray values)

-- | Returns a serialized read parameter object to send over XPC
--
-- ObjC selector: @+ encodeXPCReadParams:@
encodeXPCReadParams :: IsMTRReadParams params => params -> IO (Id NSDictionary)
encodeXPCReadParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' encodeXPCReadParamsSelector (toMTRReadParams params)

-- | Returns a deserialized read parameter object from an object received over XPC
--
-- ObjC selector: @+ decodeXPCReadParams:@
decodeXPCReadParams :: IsNSDictionary params => params -> IO (Id MTRReadParams)
decodeXPCReadParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' decodeXPCReadParamsSelector (toNSDictionary params)

-- | Returns a serialized subscribe parameter object to send over XPC
--
-- ObjC selector: @+ encodeXPCSubscribeParams:@
encodeXPCSubscribeParams :: IsMTRSubscribeParams params => params -> IO (Id NSDictionary)
encodeXPCSubscribeParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' encodeXPCSubscribeParamsSelector (toMTRSubscribeParams params)

-- | Returns a deserialized subscribe parameter object from an object received over XPC
--
-- ObjC selector: @+ decodeXPCSubscribeParams:@
decodeXPCSubscribeParams :: IsNSDictionary params => params -> IO (Id MTRSubscribeParams)
decodeXPCSubscribeParams params =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' decodeXPCSubscribeParamsSelector (toNSDictionary params)

-- | Returns an NSXPCInterface configured for MTRDeviceControllerServerProtocol.
--
-- ObjC selector: @+ xpcInterfaceForServerProtocol@
xpcInterfaceForServerProtocol :: IO (Id NSXPCInterface)
xpcInterfaceForServerProtocol  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' xpcInterfaceForServerProtocolSelector

-- | Returns an NSXPCInterface configured for MTRDeviceControllerClientProtocol.
--
-- ObjC selector: @+ xpcInterfaceForClientProtocol@
xpcInterfaceForClientProtocol :: IO (Id NSXPCInterface)
xpcInterfaceForClientProtocol  =
  do
    cls' <- getRequiredClass "MTRDeviceController"
    sendClassMessage cls' xpcInterfaceForClientProtocolSelector

-- | @- fetchAttestationChallengeForDeviceId:@
fetchAttestationChallengeForDeviceId :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> CULong -> IO (Id NSData)
fetchAttestationChallengeForDeviceId mtrDeviceController deviceId =
  sendMessage mtrDeviceController fetchAttestationChallengeForDeviceIdSelector deviceId

-- | @- getBaseDevice:queue:completionHandler:@
getBaseDevice_queue_completionHandler :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> CULong -> queue -> Ptr () -> IO Bool
getBaseDevice_queue_completionHandler mtrDeviceController deviceID queue completionHandler =
  sendMessage mtrDeviceController getBaseDevice_queue_completionHandlerSelector deviceID (toNSObject queue) completionHandler

-- | @- pairDevice:discriminator:setupPINCode:error:@
pairDevice_discriminator_setupPINCode_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CUShort -> CUInt -> error_ -> IO Bool
pairDevice_discriminator_setupPINCode_error mtrDeviceController deviceID discriminator setupPINCode error_ =
  sendMessage mtrDeviceController pairDevice_discriminator_setupPINCode_errorSelector deviceID discriminator setupPINCode (toNSError error_)

-- | @- pairDevice:address:port:setupPINCode:error:@
pairDevice_address_port_setupPINCode_error :: (IsMTRDeviceController mtrDeviceController, IsNSString address, IsNSError error_) => mtrDeviceController -> CULong -> address -> CUShort -> CUInt -> error_ -> IO Bool
pairDevice_address_port_setupPINCode_error mtrDeviceController deviceID address port setupPINCode error_ =
  sendMessage mtrDeviceController pairDevice_address_port_setupPINCode_errorSelector deviceID (toNSString address) port setupPINCode (toNSError error_)

-- | @- pairDevice:onboardingPayload:error:@
pairDevice_onboardingPayload_error :: (IsMTRDeviceController mtrDeviceController, IsNSString onboardingPayload, IsNSError error_) => mtrDeviceController -> CULong -> onboardingPayload -> error_ -> IO Bool
pairDevice_onboardingPayload_error mtrDeviceController deviceID onboardingPayload error_ =
  sendMessage mtrDeviceController pairDevice_onboardingPayload_errorSelector deviceID (toNSString onboardingPayload) (toNSError error_)

-- | @- commissionDevice:commissioningParams:error:@
commissionDevice_commissioningParams_error :: (IsMTRDeviceController mtrDeviceController, IsMTRCommissioningParameters commissioningParams, IsNSError error_) => mtrDeviceController -> CULong -> commissioningParams -> error_ -> IO Bool
commissionDevice_commissioningParams_error mtrDeviceController deviceId commissioningParams error_ =
  sendMessage mtrDeviceController commissionDevice_commissioningParams_errorSelector deviceId (toMTRCommissioningParameters commissioningParams) (toNSError error_)

-- | @- stopDevicePairing:error:@
stopDevicePairing_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> error_ -> IO Bool
stopDevicePairing_error mtrDeviceController deviceID error_ =
  sendMessage mtrDeviceController stopDevicePairing_errorSelector deviceID (toNSError error_)

-- | @- getDeviceBeingCommissioned:error:@
getDeviceBeingCommissioned_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> error_ -> IO (Id MTRBaseDevice)
getDeviceBeingCommissioned_error mtrDeviceController deviceId error_ =
  sendMessage mtrDeviceController getDeviceBeingCommissioned_errorSelector deviceId (toNSError error_)

-- | @- openPairingWindow:duration:error:@
openPairingWindow_duration_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CULong -> error_ -> IO Bool
openPairingWindow_duration_error mtrDeviceController deviceID duration error_ =
  sendMessage mtrDeviceController openPairingWindow_duration_errorSelector deviceID duration (toNSError error_)

-- | @- openPairingWindowWithPIN:duration:discriminator:setupPIN:error:@
openPairingWindowWithPIN_duration_discriminator_setupPIN_error :: (IsMTRDeviceController mtrDeviceController, IsNSError error_) => mtrDeviceController -> CULong -> CULong -> CULong -> CULong -> error_ -> IO (Id NSString)
openPairingWindowWithPIN_duration_discriminator_setupPIN_error mtrDeviceController deviceID duration discriminator setupPIN error_ =
  sendMessage mtrDeviceController openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector deviceID duration discriminator setupPIN (toNSError error_)

-- | @- computePaseVerifier:iterations:salt:@
computePaseVerifier_iterations_salt :: (IsMTRDeviceController mtrDeviceController, IsNSData salt) => mtrDeviceController -> CUInt -> CUInt -> salt -> IO (Id NSData)
computePaseVerifier_iterations_salt mtrDeviceController setupPincode iterations salt =
  sendMessage mtrDeviceController computePaseVerifier_iterations_saltSelector setupPincode iterations (toNSData salt)

-- | @- setPairingDelegate:queue:@
setPairingDelegate_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setPairingDelegate_queue mtrDeviceController delegate queue =
  sendMessage mtrDeviceController setPairingDelegate_queueSelector delegate (toNSObject queue)

-- | @- setNocChainIssuer:queue:@
setNocChainIssuer_queue :: (IsMTRDeviceController mtrDeviceController, IsNSObject queue) => mtrDeviceController -> RawId -> queue -> IO ()
setNocChainIssuer_queue mtrDeviceController nocChainIssuer queue =
  sendMessage mtrDeviceController setNocChainIssuer_queueSelector nocChainIssuer (toNSObject queue)

-- | If true, the controller has not been shut down yet.
--
-- ObjC selector: @- running@
running :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
running mtrDeviceController =
  sendMessage mtrDeviceController runningSelector

-- | If true, the controller has been suspended via @suspend@ and not resumed yet.
--
-- ObjC selector: @- suspended@
suspended :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO Bool
suspended mtrDeviceController =
  sendMessage mtrDeviceController suspendedSelector

-- | The ID assigned to this controller at creation time.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSUUID)
uniqueIdentifier mtrDeviceController =
  sendMessage mtrDeviceController uniqueIdentifierSelector

-- | Return the Node ID assigned to the controller.  Will return nil if the controller is not running (and hence does not know its node id).
--
-- ObjC selector: @- controllerNodeID@
controllerNodeID :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSNumber)
controllerNodeID mtrDeviceController =
  sendMessage mtrDeviceController controllerNodeIDSelector

-- | Returns the list of MTRDevice instances that this controller has loaded into memory. Returns an empty array if no devices are in memory.
--
-- ObjC selector: @- devices@
devices :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSArray)
devices mtrDeviceController =
  sendMessage mtrDeviceController devicesSelector

-- | Returns the list of node IDs for which this controller has stored information.  Returns empty list if the controller does not have any information stored.
--
-- ObjC selector: @- nodesWithStoredData@
nodesWithStoredData :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSArray)
nodesWithStoredData mtrDeviceController =
  sendMessage mtrDeviceController nodesWithStoredDataSelector

-- | @- controllerNodeId@
controllerNodeId :: IsMTRDeviceController mtrDeviceController => mtrDeviceController -> IO (Id NSNumber)
controllerNodeId mtrDeviceController =
  sendMessage mtrDeviceController controllerNodeIdSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceController)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceController)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithParameters:error:@
initWithParameters_errorSelector :: Selector '[Id MTRDeviceControllerAbstractParameters, Id NSError] (Id MTRDeviceController)
initWithParameters_errorSelector = mkSelector "initWithParameters:error:"

-- | @Selector@ for @setupCommissioningSessionWithPayload:newNodeID:error:@
setupCommissioningSessionWithPayload_newNodeID_errorSelector :: Selector '[Id MTRSetupPayload, Id NSNumber, Id NSError] Bool
setupCommissioningSessionWithPayload_newNodeID_errorSelector = mkSelector "setupCommissioningSessionWithPayload:newNodeID:error:"

-- | @Selector@ for @setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:@
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector :: Selector '[Id MTRCommissionableBrowserResult, Id MTRSetupPayload, Id NSNumber, Id NSError] Bool
setupCommissioningSessionWithDiscoveredDevice_payload_newNodeID_errorSelector = mkSelector "setupCommissioningSessionWithDiscoveredDevice:payload:newNodeID:error:"

-- | @Selector@ for @commissionNodeWithID:commissioningParams:error:@
commissionNodeWithID_commissioningParams_errorSelector :: Selector '[Id NSNumber, Id MTRCommissioningParameters, Id NSError] Bool
commissionNodeWithID_commissioningParams_errorSelector = mkSelector "commissionNodeWithID:commissioningParams:error:"

-- | @Selector@ for @continueCommissioningDevice:ignoreAttestationFailure:error:@
continueCommissioningDevice_ignoreAttestationFailure_errorSelector :: Selector '[Ptr (), Bool, Id NSError] Bool
continueCommissioningDevice_ignoreAttestationFailure_errorSelector = mkSelector "continueCommissioningDevice:ignoreAttestationFailure:error:"

-- | @Selector@ for @cancelCommissioningForNodeID:error:@
cancelCommissioningForNodeID_errorSelector :: Selector '[Id NSNumber, Id NSError] Bool
cancelCommissioningForNodeID_errorSelector = mkSelector "cancelCommissioningForNodeID:error:"

-- | @Selector@ for @deviceBeingCommissionedWithNodeID:error:@
deviceBeingCommissionedWithNodeID_errorSelector :: Selector '[Id NSNumber, Id NSError] (Id MTRBaseDevice)
deviceBeingCommissionedWithNodeID_errorSelector = mkSelector "deviceBeingCommissionedWithNodeID:error:"

-- | @Selector@ for @preWarmCommissioningSession@
preWarmCommissioningSessionSelector :: Selector '[] ()
preWarmCommissioningSessionSelector = mkSelector "preWarmCommissioningSession"

-- | @Selector@ for @setDeviceControllerDelegate:queue:@
setDeviceControllerDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDeviceControllerDelegate_queueSelector = mkSelector "setDeviceControllerDelegate:queue:"

-- | @Selector@ for @addDeviceControllerDelegate:queue:@
addDeviceControllerDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
addDeviceControllerDelegate_queueSelector = mkSelector "addDeviceControllerDelegate:queue:"

-- | @Selector@ for @removeDeviceControllerDelegate:@
removeDeviceControllerDelegateSelector :: Selector '[RawId] ()
removeDeviceControllerDelegateSelector = mkSelector "removeDeviceControllerDelegate:"

-- | @Selector@ for @startBrowseForCommissionables:queue:@
startBrowseForCommissionables_queueSelector :: Selector '[RawId, Id NSObject] Bool
startBrowseForCommissionables_queueSelector = mkSelector "startBrowseForCommissionables:queue:"

-- | @Selector@ for @stopBrowseForCommissionables@
stopBrowseForCommissionablesSelector :: Selector '[] Bool
stopBrowseForCommissionablesSelector = mkSelector "stopBrowseForCommissionables"

-- | @Selector@ for @attestationChallengeForDeviceID:@
attestationChallengeForDeviceIDSelector :: Selector '[Id NSNumber] (Id NSData)
attestationChallengeForDeviceIDSelector = mkSelector "attestationChallengeForDeviceID:"

-- | @Selector@ for @addServerEndpoint:@
addServerEndpointSelector :: Selector '[Id MTRServerEndpoint] Bool
addServerEndpointSelector = mkSelector "addServerEndpoint:"

-- | @Selector@ for @removeServerEndpoint:queue:completion:@
removeServerEndpoint_queue_completionSelector :: Selector '[Id MTRServerEndpoint, Id NSObject, Ptr ()] ()
removeServerEndpoint_queue_completionSelector = mkSelector "removeServerEndpoint:queue:completion:"

-- | @Selector@ for @removeServerEndpoint:@
removeServerEndpointSelector :: Selector '[Id MTRServerEndpoint] ()
removeServerEndpointSelector = mkSelector "removeServerEndpoint:"

-- | @Selector@ for @forgetDeviceWithNodeID:@
forgetDeviceWithNodeIDSelector :: Selector '[Id NSNumber] ()
forgetDeviceWithNodeIDSelector = mkSelector "forgetDeviceWithNodeID:"

-- | @Selector@ for @computePASEVerifierForSetupPasscode:iterations:salt:error:@
computePASEVerifierForSetupPasscode_iterations_salt_errorSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSData, Id NSError] (Id NSData)
computePASEVerifierForSetupPasscode_iterations_salt_errorSelector = mkSelector "computePASEVerifierForSetupPasscode:iterations:salt:error:"

-- | @Selector@ for @suspend@
suspendSelector :: Selector '[] ()
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @shutdown@
shutdownSelector :: Selector '[] ()
shutdownSelector = mkSelector "shutdown"

-- | @Selector@ for @sharedControllerWithId:xpcConnectBlock:@
sharedControllerWithId_xpcConnectBlockSelector :: Selector '[RawId, Ptr ()] (Id MTRDeviceController)
sharedControllerWithId_xpcConnectBlockSelector = mkSelector "sharedControllerWithId:xpcConnectBlock:"

-- | @Selector@ for @sharedControllerWithID:xpcConnectBlock:@
sharedControllerWithID_xpcConnectBlockSelector :: Selector '[RawId, Ptr ()] (Id MTRDeviceController)
sharedControllerWithID_xpcConnectBlockSelector = mkSelector "sharedControllerWithID:xpcConnectBlock:"

-- | @Selector@ for @encodeXPCResponseValues:@
encodeXPCResponseValuesSelector :: Selector '[Id NSArray] (Id NSArray)
encodeXPCResponseValuesSelector = mkSelector "encodeXPCResponseValues:"

-- | @Selector@ for @decodeXPCResponseValues:@
decodeXPCResponseValuesSelector :: Selector '[Id NSArray] (Id NSArray)
decodeXPCResponseValuesSelector = mkSelector "decodeXPCResponseValues:"

-- | @Selector@ for @encodeXPCReadParams:@
encodeXPCReadParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
encodeXPCReadParamsSelector = mkSelector "encodeXPCReadParams:"

-- | @Selector@ for @decodeXPCReadParams:@
decodeXPCReadParamsSelector :: Selector '[Id NSDictionary] (Id MTRReadParams)
decodeXPCReadParamsSelector = mkSelector "decodeXPCReadParams:"

-- | @Selector@ for @encodeXPCSubscribeParams:@
encodeXPCSubscribeParamsSelector :: Selector '[Id MTRSubscribeParams] (Id NSDictionary)
encodeXPCSubscribeParamsSelector = mkSelector "encodeXPCSubscribeParams:"

-- | @Selector@ for @decodeXPCSubscribeParams:@
decodeXPCSubscribeParamsSelector :: Selector '[Id NSDictionary] (Id MTRSubscribeParams)
decodeXPCSubscribeParamsSelector = mkSelector "decodeXPCSubscribeParams:"

-- | @Selector@ for @xpcInterfaceForServerProtocol@
xpcInterfaceForServerProtocolSelector :: Selector '[] (Id NSXPCInterface)
xpcInterfaceForServerProtocolSelector = mkSelector "xpcInterfaceForServerProtocol"

-- | @Selector@ for @xpcInterfaceForClientProtocol@
xpcInterfaceForClientProtocolSelector :: Selector '[] (Id NSXPCInterface)
xpcInterfaceForClientProtocolSelector = mkSelector "xpcInterfaceForClientProtocol"

-- | @Selector@ for @fetchAttestationChallengeForDeviceId:@
fetchAttestationChallengeForDeviceIdSelector :: Selector '[CULong] (Id NSData)
fetchAttestationChallengeForDeviceIdSelector = mkSelector "fetchAttestationChallengeForDeviceId:"

-- | @Selector@ for @getBaseDevice:queue:completionHandler:@
getBaseDevice_queue_completionHandlerSelector :: Selector '[CULong, Id NSObject, Ptr ()] Bool
getBaseDevice_queue_completionHandlerSelector = mkSelector "getBaseDevice:queue:completionHandler:"

-- | @Selector@ for @pairDevice:discriminator:setupPINCode:error:@
pairDevice_discriminator_setupPINCode_errorSelector :: Selector '[CULong, CUShort, CUInt, Id NSError] Bool
pairDevice_discriminator_setupPINCode_errorSelector = mkSelector "pairDevice:discriminator:setupPINCode:error:"

-- | @Selector@ for @pairDevice:address:port:setupPINCode:error:@
pairDevice_address_port_setupPINCode_errorSelector :: Selector '[CULong, Id NSString, CUShort, CUInt, Id NSError] Bool
pairDevice_address_port_setupPINCode_errorSelector = mkSelector "pairDevice:address:port:setupPINCode:error:"

-- | @Selector@ for @pairDevice:onboardingPayload:error:@
pairDevice_onboardingPayload_errorSelector :: Selector '[CULong, Id NSString, Id NSError] Bool
pairDevice_onboardingPayload_errorSelector = mkSelector "pairDevice:onboardingPayload:error:"

-- | @Selector@ for @commissionDevice:commissioningParams:error:@
commissionDevice_commissioningParams_errorSelector :: Selector '[CULong, Id MTRCommissioningParameters, Id NSError] Bool
commissionDevice_commissioningParams_errorSelector = mkSelector "commissionDevice:commissioningParams:error:"

-- | @Selector@ for @stopDevicePairing:error:@
stopDevicePairing_errorSelector :: Selector '[CULong, Id NSError] Bool
stopDevicePairing_errorSelector = mkSelector "stopDevicePairing:error:"

-- | @Selector@ for @getDeviceBeingCommissioned:error:@
getDeviceBeingCommissioned_errorSelector :: Selector '[CULong, Id NSError] (Id MTRBaseDevice)
getDeviceBeingCommissioned_errorSelector = mkSelector "getDeviceBeingCommissioned:error:"

-- | @Selector@ for @openPairingWindow:duration:error:@
openPairingWindow_duration_errorSelector :: Selector '[CULong, CULong, Id NSError] Bool
openPairingWindow_duration_errorSelector = mkSelector "openPairingWindow:duration:error:"

-- | @Selector@ for @openPairingWindowWithPIN:duration:discriminator:setupPIN:error:@
openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector :: Selector '[CULong, CULong, CULong, CULong, Id NSError] (Id NSString)
openPairingWindowWithPIN_duration_discriminator_setupPIN_errorSelector = mkSelector "openPairingWindowWithPIN:duration:discriminator:setupPIN:error:"

-- | @Selector@ for @computePaseVerifier:iterations:salt:@
computePaseVerifier_iterations_saltSelector :: Selector '[CUInt, CUInt, Id NSData] (Id NSData)
computePaseVerifier_iterations_saltSelector = mkSelector "computePaseVerifier:iterations:salt:"

-- | @Selector@ for @setPairingDelegate:queue:@
setPairingDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setPairingDelegate_queueSelector = mkSelector "setPairingDelegate:queue:"

-- | @Selector@ for @setNocChainIssuer:queue:@
setNocChainIssuer_queueSelector :: Selector '[RawId, Id NSObject] ()
setNocChainIssuer_queueSelector = mkSelector "setNocChainIssuer:queue:"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] Bool
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSUUID)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @controllerNodeID@
controllerNodeIDSelector :: Selector '[] (Id NSNumber)
controllerNodeIDSelector = mkSelector "controllerNodeID"

-- | @Selector@ for @devices@
devicesSelector :: Selector '[] (Id NSArray)
devicesSelector = mkSelector "devices"

-- | @Selector@ for @nodesWithStoredData@
nodesWithStoredDataSelector :: Selector '[] (Id NSArray)
nodesWithStoredDataSelector = mkSelector "nodesWithStoredData"

-- | @Selector@ for @controllerNodeId@
controllerNodeIdSelector :: Selector '[] (Id NSNumber)
controllerNodeIdSelector = mkSelector "controllerNodeId"

