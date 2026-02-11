{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDevice@.
module ObjC.Matter.MTRDevice
  ( MTRDevice
  , IsMTRDevice(..)
  , init_
  , new
  , deviceWithNodeID_controller
  , setDelegate_queue
  , addDelegate_queue
  , addDelegate_queue_interestedPathsForAttributes_interestedPathsForEvents
  , removeDelegate
  , readAttributeWithEndpointID_clusterID_attributeID_params
  , writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout
  , readAttributePaths
  , descriptorClusters
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completion
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completion
  , invokeCommands_queue_completion
  , openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion
  , openCommissioningWindowWithDiscriminator_duration_queue_completion
  , downloadLogOfType_timeout_queue_completion
  , waitForAttributeValues_timeout_queue_completion
  , deviceWithNodeID_deviceController
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completion
  , state
  , deviceCachePrimed
  , estimatedStartTime
  , deviceController
  , nodeID
  , estimatedSubscriptionLatency
  , vendorID
  , productID
  , networkCommissioningFeatures
  , initSelector
  , newSelector
  , deviceWithNodeID_controllerSelector
  , setDelegate_queueSelector
  , addDelegate_queueSelector
  , addDelegate_queue_interestedPathsForAttributes_interestedPathsForEventsSelector
  , removeDelegateSelector
  , readAttributeWithEndpointID_clusterID_attributeID_paramsSelector
  , writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeoutSelector
  , readAttributePathsSelector
  , descriptorClustersSelector
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completionSelector
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completionSelector
  , invokeCommands_queue_completionSelector
  , openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector
  , openCommissioningWindowWithDiscriminator_duration_queue_completionSelector
  , downloadLogOfType_timeout_queue_completionSelector
  , waitForAttributeValues_timeout_queue_completionSelector
  , deviceWithNodeID_deviceControllerSelector
  , invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completionSelector
  , stateSelector
  , deviceCachePrimedSelector
  , estimatedStartTimeSelector
  , deviceControllerSelector
  , nodeIDSelector
  , estimatedSubscriptionLatencySelector
  , vendorIDSelector
  , productIDSelector
  , networkCommissioningFeaturesSelector

  -- * Enum types
  , MTRDeviceState(MTRDeviceState)
  , pattern MTRDeviceStateUnknown
  , pattern MTRDeviceStateReachable
  , pattern MTRDeviceStateUnreachable
  , MTRDiagnosticLogType(MTRDiagnosticLogType)
  , pattern MTRDiagnosticLogTypeEndUserSupport
  , pattern MTRDiagnosticLogTypeNetworkDiagnostics
  , pattern MTRDiagnosticLogTypeCrash
  , MTRNetworkCommissioningFeature(MTRNetworkCommissioningFeature)
  , pattern MTRNetworkCommissioningFeatureWiFiNetworkInterface
  , pattern MTRNetworkCommissioningFeatureThreadNetworkInterface
  , pattern MTRNetworkCommissioningFeatureEthernetNetworkInterface
  , pattern MTRNetworkCommissioningFeaturePerDeviceCredentials

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

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id MTRDevice)
init_ mtrDevice  =
    sendMsg mtrDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDevice)
new  =
  do
    cls' <- getRequiredClass "MTRDevice"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get an MTRDevice object representing a device with a specific node ID associated with a specific controller.
--
-- MTRDevice objects are stateful, and callers should hold on to the MTRDevice while they are using it.
--
-- ObjC selector: @+ deviceWithNodeID:controller:@
deviceWithNodeID_controller :: (IsNSNumber nodeID, IsMTRDeviceController controller) => nodeID -> controller -> IO (Id MTRDevice)
deviceWithNodeID_controller nodeID controller =
  do
    cls' <- getRequiredClass "MTRDevice"
    withObjCPtr nodeID $ \raw_nodeID ->
      withObjCPtr controller $ \raw_controller ->
        sendClassMsg cls' (mkSelector "deviceWithNodeID:controller:") (retPtr retVoid) [argPtr (castPtr raw_nodeID :: Ptr ()), argPtr (castPtr raw_controller :: Ptr ())] >>= retainedObject . castPtr

-- | Set the delegate to receive asynchronous callbacks about the device.
--
-- The delegate will be called on the provided queue, for attribute reports, event reports, and device state changes.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsMTRDevice mtrDevice, IsNSObject queue) => mtrDevice -> RawId -> queue -> IO ()
setDelegate_queue mtrDevice  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDevice (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Adds a delegate to receive asynchronous callbacks about the device.
--
-- The delegate will be called on the provided queue, for attribute reports, event reports, and device state changes.
--
-- MTRDevice holds a weak reference to the delegate object.
--
-- ObjC selector: @- addDelegate:queue:@
addDelegate_queue :: (IsMTRDevice mtrDevice, IsNSObject queue) => mtrDevice -> RawId -> queue -> IO ()
addDelegate_queue mtrDevice  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDevice (mkSelector "addDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Adds a delegate to receive asynchronous callbacks about the device, and limit attribute and/or event reports to a specific set of paths.
--
-- interestedPathsForAttributes may contain either MTRClusterPath or MTRAttributePath to specify interested clusters and attributes, or NSNumber for endpoints.
--
-- interestedPathsForEvents may contain either MTRClusterPath or MTREventPath to specify interested clusters and events, or NSNumber for endpoints.
--
-- For both interested paths arguments, if nil is specified, then no filter will be applied.
--
-- Calling addDelegate: again with the same delegate object will update the interested paths for attributes and events for this delegate.
--
-- MTRDevice holds a weak reference to the delegate object.
--
-- ObjC selector: @- addDelegate:queue:interestedPathsForAttributes:interestedPathsForEvents:@
addDelegate_queue_interestedPathsForAttributes_interestedPathsForEvents :: (IsMTRDevice mtrDevice, IsNSObject queue, IsNSArray interestedPathsForAttributes, IsNSArray interestedPathsForEvents) => mtrDevice -> RawId -> queue -> interestedPathsForAttributes -> interestedPathsForEvents -> IO ()
addDelegate_queue_interestedPathsForAttributes_interestedPathsForEvents mtrDevice  delegate queue interestedPathsForAttributes interestedPathsForEvents =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr interestedPathsForAttributes $ \raw_interestedPathsForAttributes ->
      withObjCPtr interestedPathsForEvents $ \raw_interestedPathsForEvents ->
          sendMsg mtrDevice (mkSelector "addDelegate:queue:interestedPathsForAttributes:interestedPathsForEvents:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_interestedPathsForAttributes :: Ptr ()), argPtr (castPtr raw_interestedPathsForEvents :: Ptr ())]

-- | Removes the delegate from receiving callbacks about the device.
--
-- ObjC selector: @- removeDelegate:@
removeDelegate :: IsMTRDevice mtrDevice => mtrDevice -> RawId -> IO ()
removeDelegate mtrDevice  delegate =
    sendMsg mtrDevice (mkSelector "removeDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | Read attribute in a designated attribute path.  If there is no value available for the attribute, whether because the device does not implement it or because the subscription priming read has not yet gotten to this attribute, nil will be returned.
--
-- TODO: Need to fully document that this returns "the system's best guess" of attribute values.
--
-- Returns: a data-value dictionary of the attribute as described in MTRDeviceResponseHandler,         or nil if there is no value.
--
-- ObjC selector: @- readAttributeWithEndpointID:clusterID:attributeID:params:@
readAttributeWithEndpointID_clusterID_attributeID_params :: (IsMTRDevice mtrDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsMTRReadParams params) => mtrDevice -> endpointID -> clusterID -> attributeID -> params -> IO (Id NSDictionary)
readAttributeWithEndpointID_clusterID_attributeID_params mtrDevice  endpointID clusterID attributeID params =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr attributeID $ \raw_attributeID ->
        withObjCPtr params $ \raw_params ->
            sendMsg mtrDevice (mkSelector "readAttributeWithEndpointID:clusterID:attributeID:params:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | Write to attribute in a designated attribute path
--
-- @value@ — A data-value NSDictionary object as described in                    MTRDeviceResponseHandler.
--
-- @expectedValueInterval@ — maximum interval in milliseconds during which reads of the attribute will return the value being written. This value must be within [1, UINT32_MAX], and will be clamped to this range.
--
-- TODO: document that -readAttribute... will return the expected value for the [endpoint,cluster,attribute] until one of the following:  1. Another write for the same attribute happens.  2. expectedValueIntervalMs (clamped) expires. Need to figure out phrasing here.  3. We succeed at writing the attribute.  4. We fail at writing the attribute and give up on the write
--
-- @timeout@ — timeout in milliseconds for timed write, or nil. This value must be within [1, UINT16_MAX], and will be clamped to this range. TODO: make timeout arguments uniform
--
-- ObjC selector: @- writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:@
writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout :: (IsMTRDevice mtrDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsNSNumber expectedValueInterval, IsNSNumber timeout) => mtrDevice -> endpointID -> clusterID -> attributeID -> RawId -> expectedValueInterval -> timeout -> IO ()
writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeout mtrDevice  endpointID clusterID attributeID value expectedValueInterval timeout =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr attributeID $ \raw_attributeID ->
        withObjCPtr expectedValueInterval $ \raw_expectedValueInterval ->
          withObjCPtr timeout $ \raw_timeout ->
              sendMsg mtrDevice (mkSelector "writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_attributeID :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_expectedValueInterval :: Ptr ()), argPtr (castPtr raw_timeout :: Ptr ())]

-- | Read the attributes identified by the provided attribute paths.  The paths can include wildcards.
--
-- Paths that do not correspond to any existing attributes, or that the MTRDevice does not have attribute values for, will not be present in the return value from this function.
--
-- Returns: an array of response-value dictionaries as described in the         documentation for MTRDeviceResponseHandler.  Each one will have an         MTRAttributePathKey and an MTRDataKey.
--
-- ObjC selector: @- readAttributePaths:@
readAttributePaths :: (IsMTRDevice mtrDevice, IsNSArray attributePaths) => mtrDevice -> attributePaths -> IO (Id NSArray)
readAttributePaths mtrDevice  attributePaths =
  withObjCPtr attributePaths $ \raw_attributePaths ->
      sendMsg mtrDevice (mkSelector "readAttributePaths:") (retPtr retVoid) [argPtr (castPtr raw_attributePaths :: Ptr ())] >>= retainedObject . castPtr

-- | Read all known attributes from descriptor clusters on all known endpoints.
--
-- Returns: A dictionary with the paths of the attributes as keys and the         data-values (as described in the documentation for         MTRDeviceResponseHandler) as values.
--
-- ObjC selector: @- descriptorClusters@
descriptorClusters :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSDictionary)
descriptorClusters mtrDevice  =
    sendMsg mtrDevice (mkSelector "descriptorClusters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Invoke a command with a designated command path
--
-- @commandFields@ — command fields object. If not nil, the object must be a data-value                      NSDictionary object as described in the MTRDeviceResponseHandler                      documentation. The value must be a Structure, i.e., the NSDictionary                      MTRTypeKey key must have the value MTRStructureValueType.
--
-- If commandFields is nil, it will be treated as a Structure with no fields.
--
-- @expectedValues@ — The expected values of attributes that will be affected by the command, if                       any.  If these are provided, the relevant attributes will have the provided                       values when read until one of the following happens:
--
-- 1. Something (another invoke or a write) sets different expected values.                       2. expectedValueInterval elapses without the device reporting the                          attributes changing their values to the expected values.                       3. The command invoke fails.                       4. The device reports some other values for these attributes.
--
-- The dictionaries in this array are expected to be response-value                       dictionaries as documented in the documentation of                       MTRDeviceResponseHandler, and each one must have an MTRAttributePathKey.
--
-- The expectedValues and expectedValueInterval arguments need to be both                       nil or both non-nil, or both will be both ignored.
--
-- @expectedValueInterval@ — maximum interval in milliseconds during which reads of the                               attributes that had expected values provided will return the                               expected values. If the value is less than 1, both this value and                               expectedValues will be ignored. If this value is greater than                               UINT32_MAX, it will be clamped to UINT32_MAX.
--
-- @completion@ — response handler will receive either values or error.  A                    path-specific error status from the command invocation                    will result in an error being passed to the completion, so                    values will only be passed in when the command succeeds.
--
-- If values are passed, the array length will always be 1 and the single                    response-value in it will have an MTRCommandPathKey.  If the command                    response is just a success status, there will be no MTRDataKey.  If the                    command response has data fields, there will be an MTRDataKey, whose value                    will be of type MTRStructureValueType and describe the response payload.
--
-- ObjC selector: @- invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completion :: (IsMTRDevice mtrDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID, IsNSDictionary commandFields, IsNSArray expectedValues, IsNSNumber expectedValueInterval, IsNSObject queue) => mtrDevice -> endpointID -> clusterID -> commandID -> commandFields -> expectedValues -> expectedValueInterval -> queue -> Ptr () -> IO ()
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completion mtrDevice  endpointID clusterID commandID commandFields expectedValues expectedValueInterval queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr commandID $ \raw_commandID ->
        withObjCPtr commandFields $ \raw_commandFields ->
          withObjCPtr expectedValues $ \raw_expectedValues ->
            withObjCPtr expectedValueInterval $ \raw_expectedValueInterval ->
              withObjCPtr queue $ \raw_queue ->
                  sendMsg mtrDevice (mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_commandID :: Ptr ()), argPtr (castPtr raw_commandFields :: Ptr ()), argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueInterval :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completion :: (IsMTRDevice mtrDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID, IsNSArray expectedValues, IsNSNumber expectedValueInterval, IsNSNumber timeout, IsNSObject queue) => mtrDevice -> endpointID -> clusterID -> commandID -> RawId -> expectedValues -> expectedValueInterval -> timeout -> queue -> Ptr () -> IO ()
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completion mtrDevice  endpointID clusterID commandID commandFields expectedValues expectedValueInterval timeout queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr commandID $ \raw_commandID ->
        withObjCPtr expectedValues $ \raw_expectedValues ->
          withObjCPtr expectedValueInterval $ \raw_expectedValueInterval ->
            withObjCPtr timeout $ \raw_timeout ->
              withObjCPtr queue $ \raw_queue ->
                  sendMsg mtrDevice (mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:queue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_commandID :: Ptr ()), argPtr (castPtr (unRawId commandFields) :: Ptr ()), argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueInterval :: Ptr ()), argPtr (castPtr raw_timeout :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Invoke one or more groups of commands.
--
-- For any given group, if any command in any preceding group failed, the group will be skipped.  If all commands in all preceding groups succeeded, the commands within the group will be invoked, with no ordering guarantees within that group.
--
-- Results from all commands that were invoked will be passed to the provided completion as an array of response-value dictionaries.  Each of these will have the command path of the command (see MTRCommandPathKey) and one of three things:
--
-- 1) No other fields, indicating that the command invoke returned a succcess    status. 2) A field for MTRErrorKey, indicating that the invoke returned a failure    status (which is the value of the field). 3) A field for MTRDataKey, indicating that the invoke returned a data    response.  In this case the data-value representing the response will be    the value of this field.
--
-- ObjC selector: @- invokeCommands:queue:completion:@
invokeCommands_queue_completion :: (IsMTRDevice mtrDevice, IsNSArray commands, IsNSObject queue) => mtrDevice -> commands -> queue -> Ptr () -> IO ()
invokeCommands_queue_completion mtrDevice  commands queue completion =
  withObjCPtr commands $ \raw_commands ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrDevice (mkSelector "invokeCommands:queue:completion:") retVoid [argPtr (castPtr raw_commands :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Open a commissioning window on the device.
--
-- On success, completion will be called on queue with the MTRSetupPayload that can be used to commission the device.
--
-- @setupPasscode@ — The setup passcode to use for the commissioning window.                      See MTRSetupPayload's generateRandomSetupPasscode for                      generating a valid random passcode.
--
-- @discriminator@ — The discriminator to use for the commissionable                      advertisement.
--
-- @duration@ — Duration, in seconds, during which the commissioning                      window will be open.
--
-- ObjC selector: @- openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:@
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion :: (IsMTRDevice mtrDevice, IsNSNumber setupPasscode, IsNSNumber discriminator, IsNSNumber duration, IsNSObject queue) => mtrDevice -> setupPasscode -> discriminator -> duration -> queue -> Ptr () -> IO ()
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completion mtrDevice  setupPasscode discriminator duration queue completion =
  withObjCPtr setupPasscode $ \raw_setupPasscode ->
    withObjCPtr discriminator $ \raw_discriminator ->
      withObjCPtr duration $ \raw_duration ->
        withObjCPtr queue $ \raw_queue ->
            sendMsg mtrDevice (mkSelector "openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:") retVoid [argPtr (castPtr raw_setupPasscode :: Ptr ()), argPtr (castPtr raw_discriminator :: Ptr ()), argPtr (castPtr raw_duration :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Open a commissioning window on the device, using a random setup passcode.
--
-- On success, completion will be called on queue with the MTRSetupPayload that can be used to commission the device.
--
-- @discriminator@ — The discriminator to use for the commissionable                      advertisement.
--
-- @duration@ — Duration, in seconds, during which the commissioning                      window will be open.
--
-- ObjC selector: @- openCommissioningWindowWithDiscriminator:duration:queue:completion:@
openCommissioningWindowWithDiscriminator_duration_queue_completion :: (IsMTRDevice mtrDevice, IsNSNumber discriminator, IsNSNumber duration, IsNSObject queue) => mtrDevice -> discriminator -> duration -> queue -> Ptr () -> IO ()
openCommissioningWindowWithDiscriminator_duration_queue_completion mtrDevice  discriminator duration queue completion =
  withObjCPtr discriminator $ \raw_discriminator ->
    withObjCPtr duration $ \raw_duration ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrDevice (mkSelector "openCommissioningWindowWithDiscriminator:duration:queue:completion:") retVoid [argPtr (castPtr raw_discriminator :: Ptr ()), argPtr (castPtr raw_duration :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Download log of the desired type from the device.
--
-- Note: The consumer of this API should move the file that the url points to or open it for reading before the completion handler returns. Otherwise, the file will be deleted, and the data will be lost.
--
-- @type@ — The type of log being requested. This should correspond to a value in the enum MTRDiagnosticLogType.
--
-- @timeout@ — The timeout for getting the log. If the timeout expires, completion will be called with whatever                   has been retrieved by that point (which might be none or a partial log).                   If the timeout is set to 0, the request will not expire and completion will not be called until                   the log is fully retrieved or an error occurs.
--
-- @queue@ — The queue on which completion will be called.
--
-- @completion@ — The completion handler that is called after attempting to retrieve the requested log.                     - In case of success, the completion handler is called with a non-nil URL and a nil error.                     - If there is an error, a non-nil error is used and the url can be non-nil too if some logs have already been downloaded.
--
-- ObjC selector: @- downloadLogOfType:timeout:queue:completion:@
downloadLogOfType_timeout_queue_completion :: (IsMTRDevice mtrDevice, IsNSObject queue) => mtrDevice -> MTRDiagnosticLogType -> CDouble -> queue -> Ptr () -> IO ()
downloadLogOfType_timeout_queue_completion mtrDevice  type_ timeout queue completion =
  withObjCPtr queue $ \raw_queue ->
      sendMsg mtrDevice (mkSelector "downloadLogOfType:timeout:queue:completion:") retVoid [argCLong (coerce type_), argCDouble timeout, argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Sets up the provided completion to be called when any of the following happens:
--
-- 1) A set of attributes reaches certain values: completion called with nil. 2) The provided timeout expires: completion called with MTRErrorCodeTimeout error. 3) The wait is canceled: completion called with MTRErrorCodeCancelled error.
--
-- If the MTRAttributeValueWaiter is destroyed before the completion is called, that is treated the same as canceling the waiter.
--
-- The attributes and values to wait for are represented as a dictionary which has the attribute paths as keys and the expected data-values as values.
--
-- ObjC selector: @- waitForAttributeValues:timeout:queue:completion:@
waitForAttributeValues_timeout_queue_completion :: (IsMTRDevice mtrDevice, IsNSDictionary values, IsNSObject queue) => mtrDevice -> values -> CDouble -> queue -> Ptr () -> IO (Id MTRAttributeValueWaiter)
waitForAttributeValues_timeout_queue_completion mtrDevice  values timeout queue completion =
  withObjCPtr values $ \raw_values ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrDevice (mkSelector "waitForAttributeValues:timeout:queue:completion:") (retPtr retVoid) [argPtr (castPtr raw_values :: Ptr ()), argCDouble timeout, argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())] >>= retainedObject . castPtr

-- | Deprecated MTRDevice APIs.
--
-- ObjC selector: @+ deviceWithNodeID:deviceController:@
deviceWithNodeID_deviceController :: IsMTRDeviceController deviceController => CULong -> deviceController -> IO (Id MTRDevice)
deviceWithNodeID_deviceController nodeID deviceController =
  do
    cls' <- getRequiredClass "MTRDevice"
    withObjCPtr deviceController $ \raw_deviceController ->
      sendClassMsg cls' (mkSelector "deviceWithNodeID:deviceController:") (retPtr retVoid) [argCULong nodeID, argPtr (castPtr raw_deviceController :: Ptr ())] >>= retainedObject . castPtr

-- | @- invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:clientQueue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completion :: (IsMTRDevice mtrDevice, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID, IsNSArray expectedValues, IsNSNumber expectedValueInterval, IsNSNumber timeout, IsNSObject queue) => mtrDevice -> endpointID -> clusterID -> commandID -> RawId -> expectedValues -> expectedValueInterval -> timeout -> queue -> Ptr () -> IO ()
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completion mtrDevice  endpointID clusterID commandID commandFields expectedValues expectedValueInterval timeout queue completion =
  withObjCPtr endpointID $ \raw_endpointID ->
    withObjCPtr clusterID $ \raw_clusterID ->
      withObjCPtr commandID $ \raw_commandID ->
        withObjCPtr expectedValues $ \raw_expectedValues ->
          withObjCPtr expectedValueInterval $ \raw_expectedValueInterval ->
            withObjCPtr timeout $ \raw_timeout ->
              withObjCPtr queue $ \raw_queue ->
                  sendMsg mtrDevice (mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:clientQueue:completion:") retVoid [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ()), argPtr (castPtr raw_commandID :: Ptr ()), argPtr (castPtr (unRawId commandFields) :: Ptr ()), argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueInterval :: Ptr ()), argPtr (castPtr raw_timeout :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | The current state of the device.
--
-- The three states:   MTRDeviceStateUnknown      Unable to determine the state of the device at the moment.
--
-- MTRDeviceStateReachable      Communication with the device is expected to succeed.
--
-- MTRDeviceStateUnreachable      The device is currently unreachable.
--
-- ObjC selector: @- state@
state :: IsMTRDevice mtrDevice => mtrDevice -> IO MTRDeviceState
state mtrDevice  =
    fmap (coerce :: CULong -> MTRDeviceState) $ sendMsg mtrDevice (mkSelector "state") retCULong []

-- | Is the device cache primed for this device?
--
-- This will be true after the deviceCachePrimed: delegate callback has been called, false if not.
--
-- Please note if you have a storage delegate implemented, the cache is then stored persistently, so the delegate would then only be called once, ever - and this property would basically always be true if a subscription has ever been established at any point in the past.
--
-- ObjC selector: @- deviceCachePrimed@
deviceCachePrimed :: IsMTRDevice mtrDevice => mtrDevice -> IO Bool
deviceCachePrimed mtrDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDevice (mkSelector "deviceCachePrimed") retCULong []

-- | The estimated device system start time.
--
-- A device can report its events with either calendar time or time since system start time. When events are reported with time since system start time, this property will return an estimation of the device system start time. Because a device may report timestamps this way due to the lack of a wall clock, system start time can only be estimated based on event receive time and the timestamp value, and this estimation may change over time.
--
-- Device reboots may also cause the estimated device start time to jump forward.
--
-- If events are always reported with calendar time, then this property will return nil.
--
-- ObjC selector: @- estimatedStartTime@
estimatedStartTime :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSDate)
estimatedStartTime mtrDevice  =
    sendMsg mtrDevice (mkSelector "estimatedStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The controller this device was created for.  May return nil if that controller has been shut down.
--
-- ObjC selector: @- deviceController@
deviceController :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id MTRDeviceController)
deviceController mtrDevice  =
    sendMsg mtrDevice (mkSelector "deviceController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The node ID of the node this device corresponds to.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSNumber)
nodeID mtrDevice  =
    sendMsg mtrDevice (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An estimate of how much time is likely to elapse between setDelegate being called and the current device state (attributes, stored events) being known.
--
-- nil if no such estimate is available.  Otherwise, the NSNumber stores an NSTimeInterval.
--
-- ObjC selector: @- estimatedSubscriptionLatency@
estimatedSubscriptionLatency :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSNumber)
estimatedSubscriptionLatency mtrDevice  =
    sendMsg mtrDevice (mkSelector "estimatedSubscriptionLatency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Vendor Identifier associated with the device.
--
-- A non-nil value if the vendor identifier has been determined from the device, nil if unknown.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSNumber)
vendorID mtrDevice  =
    sendMsg mtrDevice (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Product Identifier associated with the device.
--
-- A non-nil value if the product identifier has been determined from the device, nil if unknown.
--
-- ObjC selector: @- productID@
productID :: IsMTRDevice mtrDevice => mtrDevice -> IO (Id NSNumber)
productID mtrDevice  =
    sendMsg mtrDevice (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Network commissioning features supported by the device.
--
-- ObjC selector: @- networkCommissioningFeatures@
networkCommissioningFeatures :: IsMTRDevice mtrDevice => mtrDevice -> IO MTRNetworkCommissioningFeature
networkCommissioningFeatures mtrDevice  =
    fmap (coerce :: CUInt -> MTRNetworkCommissioningFeature) $ sendMsg mtrDevice (mkSelector "networkCommissioningFeatures") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @deviceWithNodeID:controller:@
deviceWithNodeID_controllerSelector :: Selector
deviceWithNodeID_controllerSelector = mkSelector "deviceWithNodeID:controller:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @addDelegate:queue:@
addDelegate_queueSelector :: Selector
addDelegate_queueSelector = mkSelector "addDelegate:queue:"

-- | @Selector@ for @addDelegate:queue:interestedPathsForAttributes:interestedPathsForEvents:@
addDelegate_queue_interestedPathsForAttributes_interestedPathsForEventsSelector :: Selector
addDelegate_queue_interestedPathsForAttributes_interestedPathsForEventsSelector = mkSelector "addDelegate:queue:interestedPathsForAttributes:interestedPathsForEvents:"

-- | @Selector@ for @removeDelegate:@
removeDelegateSelector :: Selector
removeDelegateSelector = mkSelector "removeDelegate:"

-- | @Selector@ for @readAttributeWithEndpointID:clusterID:attributeID:params:@
readAttributeWithEndpointID_clusterID_attributeID_paramsSelector :: Selector
readAttributeWithEndpointID_clusterID_attributeID_paramsSelector = mkSelector "readAttributeWithEndpointID:clusterID:attributeID:params:"

-- | @Selector@ for @writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:@
writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeoutSelector :: Selector
writeAttributeWithEndpointID_clusterID_attributeID_value_expectedValueInterval_timedWriteTimeoutSelector = mkSelector "writeAttributeWithEndpointID:clusterID:attributeID:value:expectedValueInterval:timedWriteTimeout:"

-- | @Selector@ for @readAttributePaths:@
readAttributePathsSelector :: Selector
readAttributePathsSelector = mkSelector "readAttributePaths:"

-- | @Selector@ for @descriptorClusters@
descriptorClustersSelector :: Selector
descriptorClustersSelector = mkSelector "descriptorClusters"

-- | @Selector@ for @invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completionSelector :: Selector
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_queue_completionSelector = mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:queue:completion:"

-- | @Selector@ for @invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:queue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completionSelector :: Selector
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_queue_completionSelector = mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:queue:completion:"

-- | @Selector@ for @invokeCommands:queue:completion:@
invokeCommands_queue_completionSelector :: Selector
invokeCommands_queue_completionSelector = mkSelector "invokeCommands:queue:completion:"

-- | @Selector@ for @openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:@
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector :: Selector
openCommissioningWindowWithSetupPasscode_discriminator_duration_queue_completionSelector = mkSelector "openCommissioningWindowWithSetupPasscode:discriminator:duration:queue:completion:"

-- | @Selector@ for @openCommissioningWindowWithDiscriminator:duration:queue:completion:@
openCommissioningWindowWithDiscriminator_duration_queue_completionSelector :: Selector
openCommissioningWindowWithDiscriminator_duration_queue_completionSelector = mkSelector "openCommissioningWindowWithDiscriminator:duration:queue:completion:"

-- | @Selector@ for @downloadLogOfType:timeout:queue:completion:@
downloadLogOfType_timeout_queue_completionSelector :: Selector
downloadLogOfType_timeout_queue_completionSelector = mkSelector "downloadLogOfType:timeout:queue:completion:"

-- | @Selector@ for @waitForAttributeValues:timeout:queue:completion:@
waitForAttributeValues_timeout_queue_completionSelector :: Selector
waitForAttributeValues_timeout_queue_completionSelector = mkSelector "waitForAttributeValues:timeout:queue:completion:"

-- | @Selector@ for @deviceWithNodeID:deviceController:@
deviceWithNodeID_deviceControllerSelector :: Selector
deviceWithNodeID_deviceControllerSelector = mkSelector "deviceWithNodeID:deviceController:"

-- | @Selector@ for @invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:clientQueue:completion:@
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completionSelector :: Selector
invokeCommandWithEndpointID_clusterID_commandID_commandFields_expectedValues_expectedValueInterval_timedInvokeTimeout_clientQueue_completionSelector = mkSelector "invokeCommandWithEndpointID:clusterID:commandID:commandFields:expectedValues:expectedValueInterval:timedInvokeTimeout:clientQueue:completion:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @deviceCachePrimed@
deviceCachePrimedSelector :: Selector
deviceCachePrimedSelector = mkSelector "deviceCachePrimed"

-- | @Selector@ for @estimatedStartTime@
estimatedStartTimeSelector :: Selector
estimatedStartTimeSelector = mkSelector "estimatedStartTime"

-- | @Selector@ for @deviceController@
deviceControllerSelector :: Selector
deviceControllerSelector = mkSelector "deviceController"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @estimatedSubscriptionLatency@
estimatedSubscriptionLatencySelector :: Selector
estimatedSubscriptionLatencySelector = mkSelector "estimatedSubscriptionLatency"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @networkCommissioningFeatures@
networkCommissioningFeaturesSelector :: Selector
networkCommissioningFeaturesSelector = mkSelector "networkCommissioningFeatures"

