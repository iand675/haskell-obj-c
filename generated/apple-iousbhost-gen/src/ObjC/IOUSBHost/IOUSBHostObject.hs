{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostObject
--
-- The Abstract class IOUSBHostDevice and IOUSBHostInterface derive from.
--
-- Defines common methods that are shared between IOUSBHostDevice and IOUSBHostInterface including instance          management.
--
-- Generated bindings for @IOUSBHostObject@.
module ObjC.IOUSBHost.IOUSBHostObject
  ( IOUSBHostObject
  , IsIOUSBHostObject(..)
  , init_
  , initWithIOService_options_queue_error_interestHandler
  , initWithIOService_queue_error_interestHandler
  , destroy
  , destroyWithOptions
  , sendDeviceRequest_data_bytesTransferred_completionTimeout_error
  , sendDeviceRequest_data_bytesTransferred_error
  , sendDeviceRequest_error
  , enqueueDeviceRequest_data_completionTimeout_error_completionHandler
  , enqueueDeviceRequest_data_error_completionHandler
  , enqueueDeviceRequest_error_completionHandler
  , abortDeviceRequestsWithOption_error
  , abortDeviceRequestsWithError
  , descriptorWithType_length_index_languageID_requestType_requestRecipient_error
  , descriptorWithType_length_index_languageID_error
  , descriptorWithType_length_error
  , configurationDescriptorWithIndex_error
  , configurationDescriptorWithConfigurationValue_error
  , stringWithIndex_languageID_error
  , stringWithIndex_error
  , frameNumberWithTime
  , currentMicroframeWithTime_error
  , referenceMicroframeWithTime_error
  , ioDataWithCapacity_error
  , ioService
  , queue
  , deviceDescriptor
  , capabilityDescriptors
  , deviceAddress
  , initSelector
  , initWithIOService_options_queue_error_interestHandlerSelector
  , initWithIOService_queue_error_interestHandlerSelector
  , destroySelector
  , destroyWithOptionsSelector
  , sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector
  , sendDeviceRequest_data_bytesTransferred_errorSelector
  , sendDeviceRequest_errorSelector
  , enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector
  , enqueueDeviceRequest_data_error_completionHandlerSelector
  , enqueueDeviceRequest_error_completionHandlerSelector
  , abortDeviceRequestsWithOption_errorSelector
  , abortDeviceRequestsWithErrorSelector
  , descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector
  , descriptorWithType_length_index_languageID_errorSelector
  , descriptorWithType_length_errorSelector
  , configurationDescriptorWithIndex_errorSelector
  , configurationDescriptorWithConfigurationValue_errorSelector
  , stringWithIndex_languageID_errorSelector
  , stringWithIndex_errorSelector
  , frameNumberWithTimeSelector
  , currentMicroframeWithTime_errorSelector
  , referenceMicroframeWithTime_errorSelector
  , ioDataWithCapacity_errorSelector
  , ioServiceSelector
  , queueSelector
  , deviceDescriptorSelector
  , capabilityDescriptorsSelector
  , deviceAddressSelector

  -- * Enum types
  , IOUSBHostAbortOption(IOUSBHostAbortOption)
  , pattern IOUSBHostAbortOptionAsynchronous
  , pattern IOUSBHostAbortOptionSynchronous
  , IOUSBHostObjectDestroyOptions(IOUSBHostObjectDestroyOptions)
  , pattern IOUSBHostObjectDestroyOptionsNone
  , pattern IOUSBHostObjectDestroyOptionsDeviceSurrender
  , IOUSBHostObjectInitOptions(IOUSBHostObjectInitOptions)
  , pattern IOUSBHostObjectInitOptionsNone
  , pattern IOUSBHostObjectInitOptionsDeviceCapture
  , pattern IOUSBHostObjectInitOptionsDeviceSeize

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

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOKit.Internal.Structs
import ObjC.IOUSBHost.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Id IOUSBHostObject)
init_ iousbHostObject  =
    sendMsg iousbHostObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes IOUSBHostObject object along with user client
--
-- If the io_service_t is not found nil will be returned. If an IOUSBHostDevice or             IOUSBHostInterface user client is already created, nil will be returned.             Upon creation, exclusive ownership of the IOService will be established. When done             using the object destroy must be called on the object.
--
-- @ioService@ — io_service_t of the IOUSBHostDevice or IOUSBHostInterface             the user client is for. The IOUSBHostObject will keep a reference to the io_service_t             and release it after the IOUSBHostObject has been released.
--
-- @options@ — IOUSBHostObjectInitOptions. Default value is IOUSBHostObjectInitOptionsNone
--
-- @queue@ — A serial queue that all asynchronous io will be serviced. By             default a serial queue will be created on behalf of the client. Setting             a queue will create a dispatch source event handler for the target queue to service             all underlying io.
--
-- @interestHandler@ — IOUSBHostInterestHandler a generalInterest IOService handler. This is             to handle underlying service state changes such as termination. See             IOServiceAddInterestNotification in IOKitLib for more details. All notifications will be serviced             on an internal serial queue separate from the IO queue.
--
-- Returns: An IOUSBHostDevice or IOUSBHostInterface. The object is to be released by the caller.             An IOReturn error code will be reported on failure.
--
-- ObjC selector: @- initWithIOService:options:queue:error:interestHandler:@
initWithIOService_options_queue_error_interestHandler :: (IsIOUSBHostObject iousbHostObject, IsNSObject queue, IsNSError error_) => iousbHostObject -> CUInt -> IOUSBHostObjectInitOptions -> queue -> error_ -> Ptr () -> IO (Id IOUSBHostObject)
initWithIOService_options_queue_error_interestHandler iousbHostObject  ioService options queue error_ interestHandler =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg iousbHostObject (mkSelector "initWithIOService:options:queue:error:interestHandler:") (retPtr retVoid) [argCUInt ioService, argCULong (coerce options), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr interestHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes IOUSBHostObject object along with user client
--
-- This method should be called from either IOUSBHostDevice or IOUSBHostInterface.             If the io_service_t is not found nil will be returned. If an IOUSBHostDevice or             IOUSBHostInterface user client is already created, nil will be returned.             Upon creation, exclusive ownership of the IOService will be established. When done             using the object destroy must be called on the object.
--
-- @ioService@ — io_service_t of the IOUSBHostDevice or IOUSBHostInterface             the user client is for. The IOUSBHostObject will keep a reference to the io_service_t             and release it after the IOUSBHostObject has been released.
--
-- @queue@ — A serial queue that all asynchronous io will be serviced. By             default a serial queue will be created on behalf of the client. Setting             a queue will create a dispatch source event handler for the target queue to service             all underlying io.
--
-- @interestHandler@ — IOUSBHostInterestHandler a generalInterest IOService handler. This is             to handle underlying service state changes such as termination. See             IOServiceAddInterestNotification in IOKitLib for more details. All notifications will be serviced             on an internal serial queue separate from the IO queue.
--
-- Returns: An IOUSBHostDevice or IOUSBHostInterface. The object is to be released by the caller.             An IOReturn error code will be reported on failure.
--
-- ObjC selector: @- initWithIOService:queue:error:interestHandler:@
initWithIOService_queue_error_interestHandler :: (IsIOUSBHostObject iousbHostObject, IsNSObject queue, IsNSError error_) => iousbHostObject -> CUInt -> queue -> error_ -> Ptr () -> IO (Id IOUSBHostObject)
initWithIOService_queue_error_interestHandler iousbHostObject  ioService queue error_ interestHandler =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg iousbHostObject (mkSelector "initWithIOService:queue:error:interestHandler:") (retPtr retVoid) [argCUInt ioService, argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr interestHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Removes underlying allocations of the IOUSBHostObject object along with user client
--
-- When the IOUSBHostObject is no longer needed, destroy must be called. This will destroy             the connection with the user client and de-register interest on the service. If the object             is free'd destroy will be called automatically. Calling destroy multiple times has no effect.
--
-- ObjC selector: @- destroy@
destroy :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO ()
destroy iousbHostObject  =
    sendMsg iousbHostObject (mkSelector "destroy") retVoid []

-- | Removes underlying allocations of the IOUSBHostObject object along with user client
--
-- Extends destroy to take an options to modify the destroy behavior.  Currently only the             IOUSBHostObjectDestroyOptionsDeviceSurrender is defined to support surrendering ownersip of             the kernel service.  To be used when accepting the kUSBHostMessageDeviceIsRequestingClose message.
--
-- ObjC selector: @- destroyWithOptions:@
destroyWithOptions :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IOUSBHostObjectDestroyOptions -> IO ()
destroyWithOptions iousbHostObject  options =
    sendMsg iousbHostObject (mkSelector "destroyWithOptions:") retVoid [argCULong (coerce options)]

-- | Send a request on the default control endpoint
--
-- This method will send a synchronous request on the default control endpoint, and             will not return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @bytesTransferred@ — Optional NSUInteger reference which will be updated with the byte             count of the completed data phase.
--
-- @completionTimeout@ — Timeout of the request. If 0, the request will never timeout. By             default this value is IOUSBHostDefaultControlCompletionTimeout.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- sendDeviceRequest:data:bytesTransferred:completionTimeout:error:@
sendDeviceRequest_data_bytesTransferred_completionTimeout_error :: (IsIOUSBHostObject iousbHostObject, IsNSMutableData data_, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> data_ -> Ptr CULong -> CDouble -> error_ -> IO Bool
sendDeviceRequest_data_bytesTransferred_completionTimeout_error iousbHostObject  request data_ bytesTransferred completionTimeout error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "sendDeviceRequest:data:bytesTransferred:completionTimeout:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on the default control endpoint
--
-- This method will send a synchronous request on the default control endpoint, and             will not return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @bytesTransferred@ — Optional NSUInteger reference which will be updated with the byte             count of the completed data phase.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- sendDeviceRequest:data:bytesTransferred:error:@
sendDeviceRequest_data_bytesTransferred_error :: (IsIOUSBHostObject iousbHostObject, IsNSMutableData data_, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> data_ -> Ptr CULong -> error_ -> IO Bool
sendDeviceRequest_data_bytesTransferred_error iousbHostObject  request data_ bytesTransferred error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "sendDeviceRequest:data:bytesTransferred:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr bytesTransferred, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Send a request on the default control endpoint
--
-- This method will send a synchronous request on the default control endpoint, and             will not return until the request is complete.
--
-- @request@ — IOUSBDeviceRequest structure.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- sendDeviceRequest:error:@
sendDeviceRequest_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> error_ -> IO Bool
sendDeviceRequest_error iousbHostObject  request error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "sendDeviceRequest:error:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Enqueue a request on the default control endpoint
--
-- This method will enqueue an asynchronous request on the default control endpoint.             If successful, the provided completionHandler will be called to report the status             of the completed IO.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @completionTimeout@ — Timeout of the request. If 0, the request will never timeout. By             default this value is IOUSBHostDefaultControlCompletionTimeout
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueDeviceRequest:data:completionTimeout:error:completionHandler:@
enqueueDeviceRequest_data_completionTimeout_error_completionHandler :: (IsIOUSBHostObject iousbHostObject, IsNSMutableData data_, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> data_ -> CDouble -> error_ -> Ptr () -> IO Bool
enqueueDeviceRequest_data_completionTimeout_error_completionHandler iousbHostObject  request data_ completionTimeout error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "enqueueDeviceRequest:data:completionTimeout:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argCDouble completionTimeout, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Enqueue a request on the default control endpoint
--
-- This method will enqueue an asynchronous request on the default control endpoint.             If successful, the provided completionHandler will be called to report the status             of the completed IO.
--
-- @data@ — An NSMutableData* defining the memory to use for the request's data phase.
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueDeviceRequest:data:error:completionHandler:@
enqueueDeviceRequest_data_error_completionHandler :: (IsIOUSBHostObject iousbHostObject, IsNSMutableData data_, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> data_ -> error_ -> Ptr () -> IO Bool
enqueueDeviceRequest_data_error_completionHandler iousbHostObject  request data_ error_ completionHandler =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "enqueueDeviceRequest:data:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Enqueue a request on the default control endpoint
--
-- This method will enqueue an asynchronous request on the default control endpoint.             If successful, the provided completionHandler will be called to report the status             of the completed IO.             default this value is IOUSBHostDefaultControlCompletionTimeout
--
-- @completionHandler@ — an IOUSBHostCompletionHandler
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- enqueueDeviceRequest:error:completionHandler:@
enqueueDeviceRequest_error_completionHandler :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> IOUSBDeviceRequest -> error_ -> Ptr () -> IO Bool
enqueueDeviceRequest_error_completionHandler iousbHostObject  request error_ completionHandler =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "enqueueDeviceRequest:error:completionHandler:") retCULong [argIOUSBDeviceRequest request, argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Abort device requests
--
-- This method will abort any requests made via the
--
-- sendDeviceRequest
--
-- and
--
-- enqueueDeviceRequest
--
-- methods.
--
-- @option@ — IOUSBHostAbortOption by default IOUSBHostAbortOptionSynchronous is used
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortDeviceRequestsWithOption:error:@
abortDeviceRequestsWithOption_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> IOUSBHostAbortOption -> error_ -> IO Bool
abortDeviceRequestsWithOption_error iousbHostObject  option error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "abortDeviceRequestsWithOption:error:") retCULong [argCULong (coerce option), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Abort device requests
--
-- This method will abort any requests made via the
--
-- sendDeviceRequest
--
-- and
--
-- enqueueDeviceRequest
--
-- methods.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure
--
-- ObjC selector: @- abortDeviceRequestsWithError:@
abortDeviceRequestsWithError :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> error_ -> IO Bool
abortDeviceRequestsWithError iousbHostObject  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostObject (mkSelector "abortDeviceRequestsWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Retrieve a descriptor from the cache or the device
--
-- This method will search the descriptor cache for the descriptor that matches the              input arguments.  If the descriptor is not in the cache, a GET_DESCRIPTOR control              request (USB 2.0 9.4.3) will be issued to retrieve the descriptor from the device.              If the device request is successful, the retrieved descriptor will be added to the              cache.
--
-- @type@ — bDescriptorType of the descriptor to find.
--
-- @length@ — Reference to a NSUInteger which will be updated with the length of the              descriptor. As input, used as wLength when fetching variable-length              configuration or BOS descriptors, or when fetching nonstandard descriptor types.
--
-- @index@ — Descriptor index value.  Low byte of wValue of the SET_DESCRIPTOR              control request (USB 2.0 9.4.8). By default the value is 0
--
-- @languageID@ — Descriptor language ID.  wIndex of the SET_DESCRIPTOR              control request (USB 2.0 9.4.8).  By default the value is 0
--
-- @requestType@ — tDeviceRequestType to be used for a GET_DESCRIPTOR control request.              By default the value is IOUSBRequestTypeStandard
--
-- @requestRecipient@ — tDeviceRequestRecipient to be used for a GET_DESCRIPTOR control              request. By default the value is IOUSBRequestRecipientDevice
--
-- Returns: Pointer to the cached descriptor if found, otherwise nil. An IOReturn error code              will be reported on failure.
--
-- ObjC selector: @- descriptorWithType:length:index:languageID:requestType:requestRecipient:error:@
descriptorWithType_length_index_languageID_requestType_requestRecipient_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CInt -> Ptr CULong -> CULong -> CULong -> CInt -> CInt -> error_ -> IO (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_index_languageID_requestType_requestRecipient_error iousbHostObject  type_ length_ index languageID requestType requestRecipient error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "descriptorWithType:length:index:languageID:requestType:requestRecipient:error:") (retPtr retVoid) [argCInt (fromIntegral type_), argPtr length_, argCULong index, argCULong languageID, argCInt (fromIntegral requestType), argCInt (fromIntegral requestRecipient), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Retrieve a descriptor from the cache or the device
--
-- This method will search the descriptor cache for the descriptor that matches the              input arguments.  If the descriptor is not in the cache, a GET_DESCRIPTOR control              request (USB 2.0 9.4.3) will be issued to retrieve the descriptor from the device.              If the device request is successful, the retrieved descriptor will be added to the              cache.
--
-- @type@ — bDescriptorType of the descriptor to find.
--
-- @length@ — Reference to a NSUInteger which will be updated with the length of the              descriptor. As input, used as wLength when fetching variable-length              configuration or BOS descriptors, or when fetching nonstandard descriptor types.
--
-- @index@ — Descriptor index value.  Low byte of wValue of the SET_DESCRIPTOR              control request (USB 2.0 9.4.8). By default the value is 0
--
-- @languageID@ — Descriptor language ID.  wIndex of the SET_DESCRIPTOR              control request (USB 2.0 9.4.8).  By default the value is 0
--
-- Returns: Pointer to the cached descriptor if found, otherwise nil. An IOReturn error code              will be reported on failure.
--
-- ObjC selector: @- descriptorWithType:length:index:languageID:error:@
descriptorWithType_length_index_languageID_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CInt -> Ptr CULong -> CULong -> CULong -> error_ -> IO (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_index_languageID_error iousbHostObject  type_ length_ index languageID error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "descriptorWithType:length:index:languageID:error:") (retPtr retVoid) [argCInt (fromIntegral type_), argPtr length_, argCULong index, argCULong languageID, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Retrieve a descriptor from the cache or the device
--
-- This method will search the descriptor cache for the descriptor that matches the              input arguments.  If the descriptor is not in the cache, a GET_DESCRIPTOR control              request (USB 2.0 9.4.3) will be issued to retrieve the descriptor from the device.              If the device request is successful, the retrieved descriptor will be added to the              cache.
--
-- @type@ — bDescriptorType of the descriptor to find.
--
-- @length@ — Reference to a NSUInteger which will be updated with the length of the              descriptor. As input, used as wLength when fetching variable-length              configuration or BOS descriptors, or when fetching nonstandard descriptor types.              By default the value is 0
--
-- Returns: Pointer to the cached descriptor if found, otherwise nil. An IOReturn error code              will be reported on failure.
--
-- ObjC selector: @- descriptorWithType:length:error:@
descriptorWithType_length_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CInt -> Ptr CULong -> error_ -> IO (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_error iousbHostObject  type_ length_ error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "descriptorWithType:length:error:") (retPtr retVoid) [argCInt (fromIntegral type_), argPtr length_, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return the configuration descriptor at a specified index
--
-- This method uses descriptorWithType to retrieve the configuration descriptor.
--
-- @index@ — Descriptor index value
--
-- Returns: Pointer of the configuration descriptor if found, otherwise nil. An IOReturn error              code will be reported on failure.
--
-- ObjC selector: @- configurationDescriptorWithIndex:error:@
configurationDescriptorWithIndex_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CULong -> error_ -> IO (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorWithIndex_error iousbHostObject  index error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "configurationDescriptorWithIndex:error:") (retPtr retVoid) [argCULong index, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return the configuration descriptor with a specified value
--
-- This method uses descriptorWithType to search for a configuration descriptor with a              specific bConfigurationValue field.
--
-- @configurationValue@ — Value to match
--
-- Returns: Pointer of the configuration descriptor if found, otherwise nil. An IOReturn error              code will be reported on failure.
--
-- ObjC selector: @- configurationDescriptorWithConfigurationValue:error:@
configurationDescriptorWithConfigurationValue_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CULong -> error_ -> IO (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorWithConfigurationValue_error iousbHostObject  configurationValue error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "configurationDescriptorWithConfigurationValue:error:") (retPtr retVoid) [argCULong configurationValue, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Returns the string from a string descriptor
--
-- This method uses descriptorWithType to retrieve the string descriptor.
--
-- @index@ — Descriptor index value.  Low byte of wValue of the              SET_DESCRIPTOR control request (USB 2.0 9.4.8).
--
-- @languageID@ — Descriptor language ID.  wIndex of the SET_DESCRIPTOR              control request (USB 2.0 9.4.8). By default this value is kLanguageIDEnglishUS
--
-- Returns: NSString reference to string from descriptor, an IOReturn error code will be reported on              failure.
--
-- ObjC selector: @- stringWithIndex:languageID:error:@
stringWithIndex_languageID_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CULong -> CULong -> error_ -> IO (Const (Id NSString))
stringWithIndex_languageID_error iousbHostObject  index languageID error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostObject (mkSelector "stringWithIndex:languageID:error:") (retPtr retVoid) [argCULong index, argCULong languageID, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr >>= \x -> pure (Const x)

-- | Returns the string from a string descriptor
--
-- This method uses descriptorWithType to retrieve the string descriptor.
--
-- @index@ — Descriptor index value.  Low byte of wValue of the              SET_DESCRIPTOR control request (USB 2.0 9.4.8).
--
-- Returns: NSString reference to string from descriptor, an IOReturn error code will be reported on              failure.
--
-- ObjC selector: @- stringWithIndex:error:@
stringWithIndex_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CULong -> error_ -> IO (Const (Id NSString))
stringWithIndex_error iousbHostObject  index error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostObject (mkSelector "stringWithIndex:error:") (retPtr retVoid) [argCULong index, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr >>= \x -> pure (Const x)

-- | Return the current frame number of the USB controller
--
-- This method will return the current frame number of the USB controller,              omitting micro frame.  This is most useful for scheduling future isochronous              requests.
--
-- @time@ — If not nil, this will be updated with the current system time
--
-- Returns: The current frame number
--
-- ObjC selector: @- frameNumberWithTime:@
frameNumberWithTime :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> RawId -> IO CULong
frameNumberWithTime iousbHostObject  time =
    sendMsg iousbHostObject (mkSelector "frameNumberWithTime:") retCULong [argPtr (castPtr (unRawId time) :: Ptr ())]

-- | Return the current microframe number of the USB controller
--
-- This method will return the current microframe number of the USB controller.              This is most useful for scheduling future isochronous requests.
--
-- @time@ — If not nil, this will be updated with system time associated with the microframe.
--
-- Returns: The current microframe number. Returns 0 on failure, with NSError populated with the IOReturn error code.
--
-- ObjC selector: @- currentMicroframeWithTime:error:@
currentMicroframeWithTime_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> RawId -> error_ -> IO CULong
currentMicroframeWithTime_error iousbHostObject  time error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostObject (mkSelector "currentMicroframeWithTime:error:") retCULong [argPtr (castPtr (unRawId time) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return a recent microframe number of the USB controller
--
-- This method will return a recent microframe number of the USB controller.              This is most useful for scheduling future isochronous requests.
--
-- @time@ — If not nil, this will be updated with system time associated with the microframe.
--
-- Returns: A recent microframe number. Returns 0 on failure, with NSError populated with the IOReturn error code.
--
-- ObjC selector: @- referenceMicroframeWithTime:error:@
referenceMicroframeWithTime_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> RawId -> error_ -> IO CULong
referenceMicroframeWithTime_error iousbHostObject  time error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostObject (mkSelector "referenceMicroframeWithTime:error:") retCULong [argPtr (castPtr (unRawId time) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Allocate a buffer to be used for I/O
--
-- This method will allocate and map an IOBufferMemoryDescriptor optimized for use              by the underlying controller hardware. A buffer allocated by this method will not              be bounced to perform DMA operations.              Because the NSMutableData is backed by kernel memory, the length and capacity are              not mutable. Any changes to the length or capacity will cause an exception to be              thrown.
--
-- @capacity@ — Size of the buffer to allocate
--
-- Returns: NSMutableData of memory mapped to user space of an IOBufferMemoryDescriptor if successful,              otherwise nil. An IOReturn error code will be reported on failure. The result is              to be released by the caller
--
-- ObjC selector: @- ioDataWithCapacity:error:@
ioDataWithCapacity_error :: (IsIOUSBHostObject iousbHostObject, IsNSError error_) => iousbHostObject -> CULong -> error_ -> IO (Id NSMutableData)
ioDataWithCapacity_error iousbHostObject  capacity error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg iousbHostObject (mkSelector "ioDataWithCapacity:error:") (retPtr retVoid) [argCULong capacity, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Retrieve the IOUSBHostObject's io_service_t.
--
-- ObjC selector: @- ioService@
ioService :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO CUInt
ioService iousbHostObject  =
    sendMsg iousbHostObject (mkSelector "ioService") retCUInt []

-- | The dispatch queue that all asynchronous io will be serviced.
--
-- ObjC selector: @- queue@
queue :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Id NSObject)
queue iousbHostObject  =
    sendMsg iousbHostObject (mkSelector "queue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return the device descriptor
--
-- This method uses descriptorWithType to retrieve the device descriptor.
--
-- Returns: Pointer to the device descriptor.
--
-- ObjC selector: @- deviceDescriptor@
deviceDescriptor :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Const (Ptr IOUSBDeviceDescriptor))
deviceDescriptor iousbHostObject  =
    fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "deviceDescriptor") (retPtr retVoid) []

-- | Return the capability descriptors of the device
--
-- This method uses descriptorWithType to return the device's BOS descriptors
--
-- Returns: Pointer to the BOS descriptor if found, otherwise nil.
--
-- ObjC selector: @- capabilityDescriptors@
capabilityDescriptors :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Const (Ptr IOUSBBOSDescriptor))
capabilityDescriptors iousbHostObject  =
    fmap Const $ fmap castPtr $ sendMsg iousbHostObject (mkSelector "capabilityDescriptors") (retPtr retVoid) []

-- | Retrieve the current address of the device.
--
-- ObjC selector: @- deviceAddress@
deviceAddress :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO CULong
deviceAddress iousbHostObject  =
    sendMsg iousbHostObject (mkSelector "deviceAddress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIOService:options:queue:error:interestHandler:@
initWithIOService_options_queue_error_interestHandlerSelector :: Selector
initWithIOService_options_queue_error_interestHandlerSelector = mkSelector "initWithIOService:options:queue:error:interestHandler:"

-- | @Selector@ for @initWithIOService:queue:error:interestHandler:@
initWithIOService_queue_error_interestHandlerSelector :: Selector
initWithIOService_queue_error_interestHandlerSelector = mkSelector "initWithIOService:queue:error:interestHandler:"

-- | @Selector@ for @destroy@
destroySelector :: Selector
destroySelector = mkSelector "destroy"

-- | @Selector@ for @destroyWithOptions:@
destroyWithOptionsSelector :: Selector
destroyWithOptionsSelector = mkSelector "destroyWithOptions:"

-- | @Selector@ for @sendDeviceRequest:data:bytesTransferred:completionTimeout:error:@
sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector :: Selector
sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector = mkSelector "sendDeviceRequest:data:bytesTransferred:completionTimeout:error:"

-- | @Selector@ for @sendDeviceRequest:data:bytesTransferred:error:@
sendDeviceRequest_data_bytesTransferred_errorSelector :: Selector
sendDeviceRequest_data_bytesTransferred_errorSelector = mkSelector "sendDeviceRequest:data:bytesTransferred:error:"

-- | @Selector@ for @sendDeviceRequest:error:@
sendDeviceRequest_errorSelector :: Selector
sendDeviceRequest_errorSelector = mkSelector "sendDeviceRequest:error:"

-- | @Selector@ for @enqueueDeviceRequest:data:completionTimeout:error:completionHandler:@
enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector :: Selector
enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:data:completionTimeout:error:completionHandler:"

-- | @Selector@ for @enqueueDeviceRequest:data:error:completionHandler:@
enqueueDeviceRequest_data_error_completionHandlerSelector :: Selector
enqueueDeviceRequest_data_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:data:error:completionHandler:"

-- | @Selector@ for @enqueueDeviceRequest:error:completionHandler:@
enqueueDeviceRequest_error_completionHandlerSelector :: Selector
enqueueDeviceRequest_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:error:completionHandler:"

-- | @Selector@ for @abortDeviceRequestsWithOption:error:@
abortDeviceRequestsWithOption_errorSelector :: Selector
abortDeviceRequestsWithOption_errorSelector = mkSelector "abortDeviceRequestsWithOption:error:"

-- | @Selector@ for @abortDeviceRequestsWithError:@
abortDeviceRequestsWithErrorSelector :: Selector
abortDeviceRequestsWithErrorSelector = mkSelector "abortDeviceRequestsWithError:"

-- | @Selector@ for @descriptorWithType:length:index:languageID:requestType:requestRecipient:error:@
descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector :: Selector
descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector = mkSelector "descriptorWithType:length:index:languageID:requestType:requestRecipient:error:"

-- | @Selector@ for @descriptorWithType:length:index:languageID:error:@
descriptorWithType_length_index_languageID_errorSelector :: Selector
descriptorWithType_length_index_languageID_errorSelector = mkSelector "descriptorWithType:length:index:languageID:error:"

-- | @Selector@ for @descriptorWithType:length:error:@
descriptorWithType_length_errorSelector :: Selector
descriptorWithType_length_errorSelector = mkSelector "descriptorWithType:length:error:"

-- | @Selector@ for @configurationDescriptorWithIndex:error:@
configurationDescriptorWithIndex_errorSelector :: Selector
configurationDescriptorWithIndex_errorSelector = mkSelector "configurationDescriptorWithIndex:error:"

-- | @Selector@ for @configurationDescriptorWithConfigurationValue:error:@
configurationDescriptorWithConfigurationValue_errorSelector :: Selector
configurationDescriptorWithConfigurationValue_errorSelector = mkSelector "configurationDescriptorWithConfigurationValue:error:"

-- | @Selector@ for @stringWithIndex:languageID:error:@
stringWithIndex_languageID_errorSelector :: Selector
stringWithIndex_languageID_errorSelector = mkSelector "stringWithIndex:languageID:error:"

-- | @Selector@ for @stringWithIndex:error:@
stringWithIndex_errorSelector :: Selector
stringWithIndex_errorSelector = mkSelector "stringWithIndex:error:"

-- | @Selector@ for @frameNumberWithTime:@
frameNumberWithTimeSelector :: Selector
frameNumberWithTimeSelector = mkSelector "frameNumberWithTime:"

-- | @Selector@ for @currentMicroframeWithTime:error:@
currentMicroframeWithTime_errorSelector :: Selector
currentMicroframeWithTime_errorSelector = mkSelector "currentMicroframeWithTime:error:"

-- | @Selector@ for @referenceMicroframeWithTime:error:@
referenceMicroframeWithTime_errorSelector :: Selector
referenceMicroframeWithTime_errorSelector = mkSelector "referenceMicroframeWithTime:error:"

-- | @Selector@ for @ioDataWithCapacity:error:@
ioDataWithCapacity_errorSelector :: Selector
ioDataWithCapacity_errorSelector = mkSelector "ioDataWithCapacity:error:"

-- | @Selector@ for @ioService@
ioServiceSelector :: Selector
ioServiceSelector = mkSelector "ioService"

-- | @Selector@ for @queue@
queueSelector :: Selector
queueSelector = mkSelector "queue"

-- | @Selector@ for @deviceDescriptor@
deviceDescriptorSelector :: Selector
deviceDescriptorSelector = mkSelector "deviceDescriptor"

-- | @Selector@ for @capabilityDescriptors@
capabilityDescriptorsSelector :: Selector
capabilityDescriptorsSelector = mkSelector "capabilityDescriptors"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector
deviceAddressSelector = mkSelector "deviceAddress"

