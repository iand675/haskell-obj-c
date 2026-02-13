{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OBEXSession@.
module ObjC.IOBluetooth.OBEXSession
  ( OBEXSession
  , IsOBEXSession(..)
  , obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refCon
  , obexGet_headers_headersLength_eventSelector_selectorTarget_refCon
  , obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon
  , getAvailableCommandPayloadLength
  , getAvailableCommandResponsePayloadLength
  , getMaxPacketLength
  , hasOpenOBEXConnection
  , setEventCallback
  , setEventRefCon
  , setEventSelector_target_refCon
  , serverHandleIncomingData
  , clientHandleIncomingData
  , sendDataToTransport_dataLength
  , openTransportConnection_selectorTarget_refCon
  , hasOpenTransportConnection
  , closeTransportConnection
  , clientHandleIncomingDataSelector
  , closeTransportConnectionSelector
  , getAvailableCommandPayloadLengthSelector
  , getAvailableCommandResponsePayloadLengthSelector
  , getMaxPacketLengthSelector
  , hasOpenOBEXConnectionSelector
  , hasOpenTransportConnectionSelector
  , obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexGet_headers_headersLength_eventSelector_selectorTarget_refConSelector
  , obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refConSelector
  , obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector
  , openTransportConnection_selectorTarget_refConSelector
  , sendDataToTransport_dataLengthSelector
  , serverHandleIncomingDataSelector
  , setEventCallbackSelector
  , setEventRefConSelector
  , setEventSelector_target_refConSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.IOBluetooth.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | OBEXConnect
--
-- Initiate an OBEX connection to a device. Causes underlying transport (Bluetooth, et al) to attempt to connect				to a remote device. After success, an OBEX connect packet is sent to establish the OBEX Connection.
--
-- @inFlags@ — OBEX connect flags. See OBEX.h for possibilities.
--
-- @inMaxPacketLength@ — Maximum packet size you want to support. May be negotiated down, depending on										target device.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use the										provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXConnectHandler:(const OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector. If you have already established an OBEX				connection and you call this again you will get an 'kOBEXSessionAlreadyConnectedError' as a result.
--
-- ObjC selector: @- OBEXConnect:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> CUShort -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inFlags inMaxPacketLength inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inFlags inMaxPacketLength inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXDisconnect
--
-- Send an OBEX Disconnect command to the session's target. THIS DOES NOT necessarily close the underlying transport				connection. Deleting the session will ensure that closure.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXDisconnectHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector.				Be careful not to exceed the max packet length in your optional headers, or your command will be rejected.				It is recommended that you call getMaxPacketLength on your session before issuing this command so				you know how much data the session's target will accept in a single transaction.
--
-- ObjC selector: @- OBEXDisconnect:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXPut
--
-- Send an OBEX Put command to the session's target.
--
-- @isFinalChunk@ — Specify if this request is complete in one shot - that all the headers you are										supplying will fit in the negotiated max packet length.
--
-- @inHeadersData@ — Can be NULL. Ptr to some data you want to send as your headers, such as Length,										Name, etc. Use the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void)										for convenience.
--
-- @inHeadersDataLength@ — Length of data in ptr passed in above.
--
-- @inBodyData@ — Can be NULL. Ptr to some data you want to send as your BODY header. Do not construct a										real OBEX header here, it will be done for you - just pass a pointer to your										data, we'll do the rest. HOWEVER, be aware that some overhead (3 bytes) will be added										to the data in constructing the BODY header for you.
--
-- @inBodyDataLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXPutHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector.
--
-- ObjC selector: @- OBEXPut:headersData:headersDataLength:bodyData:bodyDataLength:eventSelector:selectorTarget:refCon:@
obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refCon obexSession isFinalChunk inHeadersData inHeadersDataLength inBodyData inBodyDataLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refConSelector isFinalChunk inHeadersData inHeadersDataLength inBodyData inBodyDataLength inSelector inTarget inUserRefCon

-- | OBEXGet
--
-- Send an OBEX Get command to the session's target.
--
-- @isFinalChunk@ — Specify if this request is complete in one shot - that all the headers you are										supplying will fit in the negotiated max packet length.
--
-- @inHeadersData@ — Can be NULL. Ptr to some data you want to send as your headers, such as Length,										Name, etc. Use the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void)										for your convenience.
--
-- @inHeadersDataLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXGetHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector.
--
-- ObjC selector: @- OBEXGet:headers:headersLength:eventSelector:selectorTarget:refCon:@
obexGet_headers_headersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexGet_headers_headersLength_eventSelector_selectorTarget_refCon obexSession isFinalChunk inHeaders inHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexGet_headers_headersLength_eventSelector_selectorTarget_refConSelector isFinalChunk inHeaders inHeadersLength inSelector inTarget inUserRefCon

-- | OBEXAbort
--
-- Send an OBEX Abort command to the session's target.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXAbortHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector.
--
-- ObjC selector: @- OBEXAbort:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXSetPath
--
-- Send an OBEX SetPath command to the session's target.
--
-- @inFlags@ — OBEX setpath flags. See OBEX.h for possibilities.
--
-- @inConstants@ — OBEX setpath constants. See OBEX.h for possibilities.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXSetPathHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				transport. You will receive a response to your command on your selector.
--
-- ObjC selector: @- OBEXSetPath:constants:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inFlags inConstants inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inFlags inConstants inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXConnectResponse
--
-- Send a connect response to a session's target.
--
-- @inFlags@ — OBEX connect flags. See OBEX.h for possibilities.
--
-- @inConstants@ — OBEX connect constants. See OBEX.h for possibilities.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXConnectResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXConnectResponse:flags:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> CUChar -> CUShort -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inFlags inMaxPacketLength inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inFlags inMaxPacketLength inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXDisconnectResponse
--
-- Send a disconnect response to a session's target.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXDisconnectResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXDisconnectResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXPutResponse
--
-- Send a put response to a session's target.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXPutResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXPutResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXGetResponse
--
-- Send a get response to a session's target.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXGetResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXGetResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXAbortResponse
--
-- Send a abort response to a session's target.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXAbortResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXAbortResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | OBEXSetPathResponse
--
-- Send a set path response to a session's target.
--
-- @inMaxPacketLength@ — Maximum packet size you want your OBEX session to communicate with. This MUST be										lower than the max packet size the client has reported to you in the connect command										you received from it.
--
-- @inOptionalHeaders@ — Can be NULL. Ptr to some data you want to send as your optional headers. Use										the provided header contruction kit in OBEX.h and OBEXHeadersToBytes(void) for convenience.
--
-- @inOptionalHeadersLength@ — Length of data in ptr passed in above.
--
-- @inSelector@ — A VALID selector to be called when something interesting happens due to this call.										Selector in your target object MUST have the following signature, or it										will not be called properly (look for error messages in Console.app):
--
-- - (void)OBEXSetPathResponseHandler:(const  OBEXSessionEvent*)inSessionEvent;
--
-- @inTarget@ — A VALID target object for the selector.
--
-- @inUserRefCon@ — Whatever you want to pass here. It will be passed back to you in the refCon portion of the										OBEXSessionEvent struct. nil is, of course, OK here.
--
-- Returns: An error code value on failure (see OBEX.h and IOReturn.h for possible return values). 0 (kOBEXSuccess) if successful.
--
-- A NULL selector or target will result in an error. After return, the data passed in will have been sent over the				underlying OBEX transport. You will receive any responses to your command response on your selector.
--
-- ObjC selector: @- OBEXSetPathResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> CUChar -> Ptr () -> CULong -> Sel -> RawId -> Ptr () -> IO CInt
obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refCon obexSession inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon =
  sendMessage obexSession obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector inResponseOpCode inOptionalHeaders inOptionalHeadersLength inSelector inTarget inUserRefCon

-- | getAvailableCommandPayloadLength
--
-- Determine the maximum amount of data you can send in a particular command as an OBEX client session.
--
-- @inOpCode@ — The opcode you are interested in sending (as a client).
--
-- Returns: The maximum amount of data a particular packet can handle, after accounting for any command overhead.
--
-- Each OBEX Command has a certain amount of overhead. Since the negotiated max packet length does				not indicate what the maximum data amount you can send in a particular command's packet, you can				use this function to determine how much data to provide in optional headers or body data headers.
--
-- ObjC selector: @- getAvailableCommandPayloadLength:@
getAvailableCommandPayloadLength :: IsOBEXSession obexSession => obexSession -> CUChar -> IO CUShort
getAvailableCommandPayloadLength obexSession inOpCode =
  sendMessage obexSession getAvailableCommandPayloadLengthSelector inOpCode

-- | getAvailableCommandResponsePayloadLength
--
-- Determine the maximum amount of data you can send in a particular command response as an OBEX server session.
--
-- @inOpCode@ — The opcode you are interested in responding to (as a server).
--
-- Returns: The maximum amount of data a particular packet can handle, after accounting for any command response overhead.
--
-- Each OBEX Command response has a certain amount of overhead. Since the negotiated max packet length does				not indicate what the maximum data amount you can send in a particular response's packet, you can				use this function to determine how much data to provide in optional headers or body data headers.
--
-- ObjC selector: @- getAvailableCommandResponsePayloadLength:@
getAvailableCommandResponsePayloadLength :: IsOBEXSession obexSession => obexSession -> CUChar -> IO CUShort
getAvailableCommandResponsePayloadLength obexSession inOpCode =
  sendMessage obexSession getAvailableCommandResponsePayloadLengthSelector inOpCode

-- | getMaxPacketLength
--
-- Gets current max packet length.
--
-- Returns: Max packet length.
--
-- This value *could* change before and after a connect command has been sent or a connect				command response has been received, since the recipient could negotiate a lower max packet size.
--
-- ObjC selector: @- getMaxPacketLength@
getMaxPacketLength :: IsOBEXSession obexSession => obexSession -> IO CUShort
getMaxPacketLength obexSession =
  sendMessage obexSession getMaxPacketLengthSelector

-- | hasOpenOBEXConnection
--
-- Has a successful connect packet been sent and received? This API tells you so.
--
-- Returns: True or false, we are OBEX-connected to another OBEX entity.
--
-- A "transport" connection may exist (such as a Bluetooth baseband connection), but the OBEX connection				may not be established over that transport. If it has been, this function returns true.
--
-- ObjC selector: @- hasOpenOBEXConnection@
hasOpenOBEXConnection :: IsOBEXSession obexSession => obexSession -> IO Bool
hasOpenOBEXConnection obexSession =
  sendMessage obexSession hasOpenOBEXConnectionSelector

-- | setEventCallback
--
-- Sets the C-API callback used when the session recieves data.
--
-- @inEventCallback@ — Function to callback. Should be non-NULL, unless you are attempting to clear the									callback, but doing that doesn't make much sense.
--
-- This is really not intended for client sessions. Only subclasses would really be interested in using this. They				should set these when their subclass object is created, because otherwise they will have no way of receiving				the initial command data packet. This is a partner to setEventRefCon, described below.
--
-- ObjC selector: @- setEventCallback:@
setEventCallback :: IsOBEXSession obexSession => obexSession -> Ptr () -> IO ()
setEventCallback obexSession inEventCallback =
  sendMessage obexSession setEventCallbackSelector inEventCallback

-- | setEventRefCon
--
-- Sets the C-API callback refCon used when the session recieves data.
--
-- @inRefCon@ — User's refCon that will get passed when their event callback is invoked.
--
-- This is really not intended for client sessions. Only subclasses would really be interested in using this. They				should set these when their subclass object is created, because otherwise they will have no context in their				callback.
--
-- ObjC selector: @- setEventRefCon:@
setEventRefCon :: IsOBEXSession obexSession => obexSession -> Ptr () -> IO ()
setEventRefCon obexSession inRefCon =
  sendMessage obexSession setEventRefConSelector inRefCon

-- | setEventSelector
--
-- Allow you to set a selector to be called when events occur on the OBEX session.
--
-- @inEventSelector@ — Selector to call on the target.
--
-- @inEventSelectorTarget@ — Target to be called with the selector.
--
-- @inUserRefCon@ — User's refCon that will get passed when their event callback is invoked.
--
-- Really not needed to be used, since the event selector will get set when an OBEX command is sent out.
--
-- ObjC selector: @- setEventSelector:target:refCon:@
setEventSelector_target_refCon :: IsOBEXSession obexSession => obexSession -> Sel -> RawId -> Ptr () -> IO ()
setEventSelector_target_refCon obexSession inEventSelector inEventSelectorTarget inUserRefCon =
  sendMessage obexSession setEventSelector_target_refConSelector inEventSelector inEventSelectorTarget inUserRefCon

-- | serverHandleIncomingData
--
-- Tranport subclasses need to invoke this from their own data-receive handlers. For example, when data is				received over a Bluetooth RFCOMM channel in the IOBluetoothOBEXSession, it in turn calls this to dispatch				the data. If you do not handle this case, your server session will not work, guaranteed.
--
-- @event@ — New event received from the transport.
--
-- Tranport subclasses must call this for OBEX server sessions to work!
--
-- ObjC selector: @- serverHandleIncomingData:@
serverHandleIncomingData :: IsOBEXSession obexSession => obexSession -> Ptr OBEXTransportEvent -> IO ()
serverHandleIncomingData obexSession event =
  sendMessage obexSession serverHandleIncomingDataSelector event

-- | clientHandleIncomingData
--
-- Tranport subclasses need to invoke this from their own data-receive handlers. For example, when data is				received over a Bluetooth RFCOMM channel in the IOBluetoothOBEXSession, it in turn calls this to dispatch				the data. If you do not handle this case, your server session will not work, guaranteed.
--
-- @event@ — New event received from the transport.
--
-- Tranport subclasses must call this for OBEX client sessions to work!
--
-- ObjC selector: @- clientHandleIncomingData:@
clientHandleIncomingData :: IsOBEXSession obexSession => obexSession -> Ptr OBEXTransportEvent -> IO ()
clientHandleIncomingData obexSession event =
  sendMessage obexSession clientHandleIncomingDataSelector event

-- | sendDataToTransport
--
-- You must override this to send data over your transport. This does nothing by default, it will				return a kOBEXUnsupportedError.
--
-- @inDataToSend@ — Data to shove over the transport to a remote OBEX session.
--
-- @inDataLength@ — Length of data passed in.
--
-- Returns: Return whether or not the transport could send the  data or not. If you are successful, return kOBEXSuccess,				otherwise an interesting error code.
--
-- Tranport subclasses must override this! When called you should send the data over the transport to				the remote session.
--
-- ObjC selector: @- sendDataToTransport:dataLength:@
sendDataToTransport_dataLength :: IsOBEXSession obexSession => obexSession -> Ptr () -> CULong -> IO CInt
sendDataToTransport_dataLength obexSession inDataToSend inDataLength =
  sendMessage obexSession sendDataToTransport_dataLengthSelector inDataToSend inDataLength

-- | openTransportConnection
--
-- Opens a transport connection to a device. A Bluetooth connection is one example of a transport.
--
-- @inSelector@ — Selector to call for success, failure or timeout.
--
-- @inTarget@ — Target on which to call the selector.
--
-- @inUserRefCon@ — Caller's reference constant.
--
-- Returns: Should return kOBEXSuccess ( 0 ) on success, otherwise an error code.
--
-- Tranport subclasses must override this! when called you should attempt to open your transport				connection, and if you are successful, return kOBEXSuccess, otherwise an interesting error code.
--
-- ObjC selector: @- openTransportConnection:selectorTarget:refCon:@
openTransportConnection_selectorTarget_refCon :: IsOBEXSession obexSession => obexSession -> Sel -> RawId -> Ptr () -> IO CInt
openTransportConnection_selectorTarget_refCon obexSession inSelector inTarget inUserRefCon =
  sendMessage obexSession openTransportConnection_selectorTarget_refConSelector inSelector inTarget inUserRefCon

-- | hasOpenTransportConnection
--
-- You must override this - it will be called periodically to determine if a transport connection is open or not.
--
-- Returns: Return whether or not the transport connection is still open.
--
-- Tranport subclasses must override this! When called you simply return if the transport connection is still				open or not.
--
-- ObjC selector: @- hasOpenTransportConnection@
hasOpenTransportConnection :: IsOBEXSession obexSession => obexSession -> IO CUChar
hasOpenTransportConnection obexSession =
  sendMessage obexSession hasOpenTransportConnectionSelector

-- | closeTransportConnection
--
-- You must override this - it will be called when the transport connection should be shutdown.
--
-- Returns: Return whether or not the transport connection was closed successfully or not. Return OBEXSuccess ( 0 ) on				success, otherwise an error code.
--
-- Tranport subclasses must override this! When called you should take whatever steps are necessary to				actually close down the transport connection.
--
-- ObjC selector: @- closeTransportConnection@
closeTransportConnection :: IsOBEXSession obexSession => obexSession -> IO CInt
closeTransportConnection obexSession =
  sendMessage obexSession closeTransportConnectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @OBEXConnect:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, CUShort, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexConnect_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXConnect:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXDisconnect:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexDisconnect_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXDisconnect:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXPut:headersData:headersDataLength:bodyData:bodyDataLength:eventSelector:selectorTarget:refCon:@
obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexPut_headersData_headersDataLength_bodyData_bodyDataLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXPut:headersData:headersDataLength:bodyData:bodyDataLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXGet:headers:headersLength:eventSelector:selectorTarget:refCon:@
obexGet_headers_headersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexGet_headers_headersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXGet:headers:headersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXAbort:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexAbort_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXAbort:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXSetPath:constants:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexSetPath_constants_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXSetPath:constants:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXConnectResponse:flags:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, CUChar, CUShort, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexConnectResponse_flags_maxPacketLength_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXConnectResponse:flags:maxPacketLength:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXDisconnectResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexDisconnectResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXDisconnectResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXPutResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexPutResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXPutResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXGetResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexGetResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXGetResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXAbortResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexAbortResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXAbortResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @OBEXSetPathResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:@
obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector :: Selector '[CUChar, Ptr (), CULong, Sel, RawId, Ptr ()] CInt
obexSetPathResponse_optionalHeaders_optionalHeadersLength_eventSelector_selectorTarget_refConSelector = mkSelector "OBEXSetPathResponse:optionalHeaders:optionalHeadersLength:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @getAvailableCommandPayloadLength:@
getAvailableCommandPayloadLengthSelector :: Selector '[CUChar] CUShort
getAvailableCommandPayloadLengthSelector = mkSelector "getAvailableCommandPayloadLength:"

-- | @Selector@ for @getAvailableCommandResponsePayloadLength:@
getAvailableCommandResponsePayloadLengthSelector :: Selector '[CUChar] CUShort
getAvailableCommandResponsePayloadLengthSelector = mkSelector "getAvailableCommandResponsePayloadLength:"

-- | @Selector@ for @getMaxPacketLength@
getMaxPacketLengthSelector :: Selector '[] CUShort
getMaxPacketLengthSelector = mkSelector "getMaxPacketLength"

-- | @Selector@ for @hasOpenOBEXConnection@
hasOpenOBEXConnectionSelector :: Selector '[] Bool
hasOpenOBEXConnectionSelector = mkSelector "hasOpenOBEXConnection"

-- | @Selector@ for @setEventCallback:@
setEventCallbackSelector :: Selector '[Ptr ()] ()
setEventCallbackSelector = mkSelector "setEventCallback:"

-- | @Selector@ for @setEventRefCon:@
setEventRefConSelector :: Selector '[Ptr ()] ()
setEventRefConSelector = mkSelector "setEventRefCon:"

-- | @Selector@ for @setEventSelector:target:refCon:@
setEventSelector_target_refConSelector :: Selector '[Sel, RawId, Ptr ()] ()
setEventSelector_target_refConSelector = mkSelector "setEventSelector:target:refCon:"

-- | @Selector@ for @serverHandleIncomingData:@
serverHandleIncomingDataSelector :: Selector '[Ptr OBEXTransportEvent] ()
serverHandleIncomingDataSelector = mkSelector "serverHandleIncomingData:"

-- | @Selector@ for @clientHandleIncomingData:@
clientHandleIncomingDataSelector :: Selector '[Ptr OBEXTransportEvent] ()
clientHandleIncomingDataSelector = mkSelector "clientHandleIncomingData:"

-- | @Selector@ for @sendDataToTransport:dataLength:@
sendDataToTransport_dataLengthSelector :: Selector '[Ptr (), CULong] CInt
sendDataToTransport_dataLengthSelector = mkSelector "sendDataToTransport:dataLength:"

-- | @Selector@ for @openTransportConnection:selectorTarget:refCon:@
openTransportConnection_selectorTarget_refConSelector :: Selector '[Sel, RawId, Ptr ()] CInt
openTransportConnection_selectorTarget_refConSelector = mkSelector "openTransportConnection:selectorTarget:refCon:"

-- | @Selector@ for @hasOpenTransportConnection@
hasOpenTransportConnectionSelector :: Selector '[] CUChar
hasOpenTransportConnectionSelector = mkSelector "hasOpenTransportConnection"

-- | @Selector@ for @closeTransportConnection@
closeTransportConnectionSelector :: Selector '[] CInt
closeTransportConnectionSelector = mkSelector "closeTransportConnection"

