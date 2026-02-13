{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , abortDeviceRequestsWithErrorSelector
  , abortDeviceRequestsWithOption_errorSelector
  , capabilityDescriptorsSelector
  , configurationDescriptorWithConfigurationValue_errorSelector
  , configurationDescriptorWithIndex_errorSelector
  , currentMicroframeWithTime_errorSelector
  , descriptorWithType_length_errorSelector
  , descriptorWithType_length_index_languageID_errorSelector
  , descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector
  , destroySelector
  , destroyWithOptionsSelector
  , deviceAddressSelector
  , deviceDescriptorSelector
  , enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector
  , enqueueDeviceRequest_data_error_completionHandlerSelector
  , enqueueDeviceRequest_error_completionHandlerSelector
  , frameNumberWithTimeSelector
  , initSelector
  , initWithIOService_options_queue_error_interestHandlerSelector
  , initWithIOService_queue_error_interestHandlerSelector
  , ioDataWithCapacity_errorSelector
  , ioServiceSelector
  , queueSelector
  , referenceMicroframeWithTime_errorSelector
  , sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector
  , sendDeviceRequest_data_bytesTransferred_errorSelector
  , sendDeviceRequest_errorSelector
  , stringWithIndex_errorSelector
  , stringWithIndex_languageID_errorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOKit.Internal.Structs
import ObjC.IOUSBHost.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Id IOUSBHostObject)
init_ iousbHostObject =
  sendOwnedMessage iousbHostObject initSelector

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
initWithIOService_options_queue_error_interestHandler iousbHostObject ioService options queue error_ interestHandler =
  sendOwnedMessage iousbHostObject initWithIOService_options_queue_error_interestHandlerSelector ioService options (toNSObject queue) (toNSError error_) interestHandler

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
initWithIOService_queue_error_interestHandler iousbHostObject ioService queue error_ interestHandler =
  sendOwnedMessage iousbHostObject initWithIOService_queue_error_interestHandlerSelector ioService (toNSObject queue) (toNSError error_) interestHandler

-- | Removes underlying allocations of the IOUSBHostObject object along with user client
--
-- When the IOUSBHostObject is no longer needed, destroy must be called. This will destroy             the connection with the user client and de-register interest on the service. If the object             is free'd destroy will be called automatically. Calling destroy multiple times has no effect.
--
-- ObjC selector: @- destroy@
destroy :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO ()
destroy iousbHostObject =
  sendMessage iousbHostObject destroySelector

-- | Removes underlying allocations of the IOUSBHostObject object along with user client
--
-- Extends destroy to take an options to modify the destroy behavior.  Currently only the             IOUSBHostObjectDestroyOptionsDeviceSurrender is defined to support surrendering ownersip of             the kernel service.  To be used when accepting the kUSBHostMessageDeviceIsRequestingClose message.
--
-- ObjC selector: @- destroyWithOptions:@
destroyWithOptions :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IOUSBHostObjectDestroyOptions -> IO ()
destroyWithOptions iousbHostObject options =
  sendMessage iousbHostObject destroyWithOptionsSelector options

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
sendDeviceRequest_data_bytesTransferred_completionTimeout_error iousbHostObject request data_ bytesTransferred completionTimeout error_ =
  sendMessage iousbHostObject sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector request (toNSMutableData data_) bytesTransferred completionTimeout (toNSError error_)

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
sendDeviceRequest_data_bytesTransferred_error iousbHostObject request data_ bytesTransferred error_ =
  sendMessage iousbHostObject sendDeviceRequest_data_bytesTransferred_errorSelector request (toNSMutableData data_) bytesTransferred (toNSError error_)

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
sendDeviceRequest_error iousbHostObject request error_ =
  sendMessage iousbHostObject sendDeviceRequest_errorSelector request (toNSError error_)

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
enqueueDeviceRequest_data_completionTimeout_error_completionHandler iousbHostObject request data_ completionTimeout error_ completionHandler =
  sendMessage iousbHostObject enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector request (toNSMutableData data_) completionTimeout (toNSError error_) completionHandler

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
enqueueDeviceRequest_data_error_completionHandler iousbHostObject request data_ error_ completionHandler =
  sendMessage iousbHostObject enqueueDeviceRequest_data_error_completionHandlerSelector request (toNSMutableData data_) (toNSError error_) completionHandler

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
enqueueDeviceRequest_error_completionHandler iousbHostObject request error_ completionHandler =
  sendMessage iousbHostObject enqueueDeviceRequest_error_completionHandlerSelector request (toNSError error_) completionHandler

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
abortDeviceRequestsWithOption_error iousbHostObject option error_ =
  sendMessage iousbHostObject abortDeviceRequestsWithOption_errorSelector option (toNSError error_)

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
abortDeviceRequestsWithError iousbHostObject error_ =
  sendMessage iousbHostObject abortDeviceRequestsWithErrorSelector (toNSError error_)

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
descriptorWithType_length_index_languageID_requestType_requestRecipient_error iousbHostObject type_ length_ index languageID requestType requestRecipient error_ =
  sendMessage iousbHostObject descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector type_ length_ index languageID requestType requestRecipient (toNSError error_)

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
descriptorWithType_length_index_languageID_error iousbHostObject type_ length_ index languageID error_ =
  sendMessage iousbHostObject descriptorWithType_length_index_languageID_errorSelector type_ length_ index languageID (toNSError error_)

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
descriptorWithType_length_error iousbHostObject type_ length_ error_ =
  sendMessage iousbHostObject descriptorWithType_length_errorSelector type_ length_ (toNSError error_)

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
configurationDescriptorWithIndex_error iousbHostObject index error_ =
  sendMessage iousbHostObject configurationDescriptorWithIndex_errorSelector index (toNSError error_)

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
configurationDescriptorWithConfigurationValue_error iousbHostObject configurationValue error_ =
  sendMessage iousbHostObject configurationDescriptorWithConfigurationValue_errorSelector configurationValue (toNSError error_)

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
stringWithIndex_languageID_error iousbHostObject index languageID error_ =
  sendMessage iousbHostObject stringWithIndex_languageID_errorSelector index languageID (toNSError error_)

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
stringWithIndex_error iousbHostObject index error_ =
  sendMessage iousbHostObject stringWithIndex_errorSelector index (toNSError error_)

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
frameNumberWithTime iousbHostObject time =
  sendMessage iousbHostObject frameNumberWithTimeSelector time

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
currentMicroframeWithTime_error iousbHostObject time error_ =
  sendMessage iousbHostObject currentMicroframeWithTime_errorSelector time (toNSError error_)

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
referenceMicroframeWithTime_error iousbHostObject time error_ =
  sendMessage iousbHostObject referenceMicroframeWithTime_errorSelector time (toNSError error_)

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
ioDataWithCapacity_error iousbHostObject capacity error_ =
  sendMessage iousbHostObject ioDataWithCapacity_errorSelector capacity (toNSError error_)

-- | Retrieve the IOUSBHostObject's io_service_t.
--
-- ObjC selector: @- ioService@
ioService :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO CUInt
ioService iousbHostObject =
  sendMessage iousbHostObject ioServiceSelector

-- | The dispatch queue that all asynchronous io will be serviced.
--
-- ObjC selector: @- queue@
queue :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Id NSObject)
queue iousbHostObject =
  sendMessage iousbHostObject queueSelector

-- | Return the device descriptor
--
-- This method uses descriptorWithType to retrieve the device descriptor.
--
-- Returns: Pointer to the device descriptor.
--
-- ObjC selector: @- deviceDescriptor@
deviceDescriptor :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Const (Ptr IOUSBDeviceDescriptor))
deviceDescriptor iousbHostObject =
  sendMessage iousbHostObject deviceDescriptorSelector

-- | Return the capability descriptors of the device
--
-- This method uses descriptorWithType to return the device's BOS descriptors
--
-- Returns: Pointer to the BOS descriptor if found, otherwise nil.
--
-- ObjC selector: @- capabilityDescriptors@
capabilityDescriptors :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO (Const (Ptr IOUSBBOSDescriptor))
capabilityDescriptors iousbHostObject =
  sendMessage iousbHostObject capabilityDescriptorsSelector

-- | Retrieve the current address of the device.
--
-- ObjC selector: @- deviceAddress@
deviceAddress :: IsIOUSBHostObject iousbHostObject => iousbHostObject -> IO CULong
deviceAddress iousbHostObject =
  sendMessage iousbHostObject deviceAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id IOUSBHostObject)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIOService:options:queue:error:interestHandler:@
initWithIOService_options_queue_error_interestHandlerSelector :: Selector '[CUInt, IOUSBHostObjectInitOptions, Id NSObject, Id NSError, Ptr ()] (Id IOUSBHostObject)
initWithIOService_options_queue_error_interestHandlerSelector = mkSelector "initWithIOService:options:queue:error:interestHandler:"

-- | @Selector@ for @initWithIOService:queue:error:interestHandler:@
initWithIOService_queue_error_interestHandlerSelector :: Selector '[CUInt, Id NSObject, Id NSError, Ptr ()] (Id IOUSBHostObject)
initWithIOService_queue_error_interestHandlerSelector = mkSelector "initWithIOService:queue:error:interestHandler:"

-- | @Selector@ for @destroy@
destroySelector :: Selector '[] ()
destroySelector = mkSelector "destroy"

-- | @Selector@ for @destroyWithOptions:@
destroyWithOptionsSelector :: Selector '[IOUSBHostObjectDestroyOptions] ()
destroyWithOptionsSelector = mkSelector "destroyWithOptions:"

-- | @Selector@ for @sendDeviceRequest:data:bytesTransferred:completionTimeout:error:@
sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector :: Selector '[IOUSBDeviceRequest, Id NSMutableData, Ptr CULong, CDouble, Id NSError] Bool
sendDeviceRequest_data_bytesTransferred_completionTimeout_errorSelector = mkSelector "sendDeviceRequest:data:bytesTransferred:completionTimeout:error:"

-- | @Selector@ for @sendDeviceRequest:data:bytesTransferred:error:@
sendDeviceRequest_data_bytesTransferred_errorSelector :: Selector '[IOUSBDeviceRequest, Id NSMutableData, Ptr CULong, Id NSError] Bool
sendDeviceRequest_data_bytesTransferred_errorSelector = mkSelector "sendDeviceRequest:data:bytesTransferred:error:"

-- | @Selector@ for @sendDeviceRequest:error:@
sendDeviceRequest_errorSelector :: Selector '[IOUSBDeviceRequest, Id NSError] Bool
sendDeviceRequest_errorSelector = mkSelector "sendDeviceRequest:error:"

-- | @Selector@ for @enqueueDeviceRequest:data:completionTimeout:error:completionHandler:@
enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector :: Selector '[IOUSBDeviceRequest, Id NSMutableData, CDouble, Id NSError, Ptr ()] Bool
enqueueDeviceRequest_data_completionTimeout_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:data:completionTimeout:error:completionHandler:"

-- | @Selector@ for @enqueueDeviceRequest:data:error:completionHandler:@
enqueueDeviceRequest_data_error_completionHandlerSelector :: Selector '[IOUSBDeviceRequest, Id NSMutableData, Id NSError, Ptr ()] Bool
enqueueDeviceRequest_data_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:data:error:completionHandler:"

-- | @Selector@ for @enqueueDeviceRequest:error:completionHandler:@
enqueueDeviceRequest_error_completionHandlerSelector :: Selector '[IOUSBDeviceRequest, Id NSError, Ptr ()] Bool
enqueueDeviceRequest_error_completionHandlerSelector = mkSelector "enqueueDeviceRequest:error:completionHandler:"

-- | @Selector@ for @abortDeviceRequestsWithOption:error:@
abortDeviceRequestsWithOption_errorSelector :: Selector '[IOUSBHostAbortOption, Id NSError] Bool
abortDeviceRequestsWithOption_errorSelector = mkSelector "abortDeviceRequestsWithOption:error:"

-- | @Selector@ for @abortDeviceRequestsWithError:@
abortDeviceRequestsWithErrorSelector :: Selector '[Id NSError] Bool
abortDeviceRequestsWithErrorSelector = mkSelector "abortDeviceRequestsWithError:"

-- | @Selector@ for @descriptorWithType:length:index:languageID:requestType:requestRecipient:error:@
descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector :: Selector '[CInt, Ptr CULong, CULong, CULong, CInt, CInt, Id NSError] (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_index_languageID_requestType_requestRecipient_errorSelector = mkSelector "descriptorWithType:length:index:languageID:requestType:requestRecipient:error:"

-- | @Selector@ for @descriptorWithType:length:index:languageID:error:@
descriptorWithType_length_index_languageID_errorSelector :: Selector '[CInt, Ptr CULong, CULong, CULong, Id NSError] (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_index_languageID_errorSelector = mkSelector "descriptorWithType:length:index:languageID:error:"

-- | @Selector@ for @descriptorWithType:length:error:@
descriptorWithType_length_errorSelector :: Selector '[CInt, Ptr CULong, Id NSError] (Const (Ptr IOUSBDescriptor))
descriptorWithType_length_errorSelector = mkSelector "descriptorWithType:length:error:"

-- | @Selector@ for @configurationDescriptorWithIndex:error:@
configurationDescriptorWithIndex_errorSelector :: Selector '[CULong, Id NSError] (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorWithIndex_errorSelector = mkSelector "configurationDescriptorWithIndex:error:"

-- | @Selector@ for @configurationDescriptorWithConfigurationValue:error:@
configurationDescriptorWithConfigurationValue_errorSelector :: Selector '[CULong, Id NSError] (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptorWithConfigurationValue_errorSelector = mkSelector "configurationDescriptorWithConfigurationValue:error:"

-- | @Selector@ for @stringWithIndex:languageID:error:@
stringWithIndex_languageID_errorSelector :: Selector '[CULong, CULong, Id NSError] (Const (Id NSString))
stringWithIndex_languageID_errorSelector = mkSelector "stringWithIndex:languageID:error:"

-- | @Selector@ for @stringWithIndex:error:@
stringWithIndex_errorSelector :: Selector '[CULong, Id NSError] (Const (Id NSString))
stringWithIndex_errorSelector = mkSelector "stringWithIndex:error:"

-- | @Selector@ for @frameNumberWithTime:@
frameNumberWithTimeSelector :: Selector '[RawId] CULong
frameNumberWithTimeSelector = mkSelector "frameNumberWithTime:"

-- | @Selector@ for @currentMicroframeWithTime:error:@
currentMicroframeWithTime_errorSelector :: Selector '[RawId, Id NSError] CULong
currentMicroframeWithTime_errorSelector = mkSelector "currentMicroframeWithTime:error:"

-- | @Selector@ for @referenceMicroframeWithTime:error:@
referenceMicroframeWithTime_errorSelector :: Selector '[RawId, Id NSError] CULong
referenceMicroframeWithTime_errorSelector = mkSelector "referenceMicroframeWithTime:error:"

-- | @Selector@ for @ioDataWithCapacity:error:@
ioDataWithCapacity_errorSelector :: Selector '[CULong, Id NSError] (Id NSMutableData)
ioDataWithCapacity_errorSelector = mkSelector "ioDataWithCapacity:error:"

-- | @Selector@ for @ioService@
ioServiceSelector :: Selector '[] CUInt
ioServiceSelector = mkSelector "ioService"

-- | @Selector@ for @queue@
queueSelector :: Selector '[] (Id NSObject)
queueSelector = mkSelector "queue"

-- | @Selector@ for @deviceDescriptor@
deviceDescriptorSelector :: Selector '[] (Const (Ptr IOUSBDeviceDescriptor))
deviceDescriptorSelector = mkSelector "deviceDescriptor"

-- | @Selector@ for @capabilityDescriptors@
capabilityDescriptorsSelector :: Selector '[] (Const (Ptr IOUSBBOSDescriptor))
capabilityDescriptorsSelector = mkSelector "capabilityDescriptors"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector '[] CULong
deviceAddressSelector = mkSelector "deviceAddress"

