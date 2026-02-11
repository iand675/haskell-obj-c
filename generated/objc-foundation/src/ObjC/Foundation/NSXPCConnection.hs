{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXPCConnection@.
module ObjC.Foundation.NSXPCConnection
  ( NSXPCConnection
  , IsNSXPCConnection(..)
  , initWithServiceName
  , initWithMachServiceName_options
  , initWithListenerEndpoint
  , remoteObjectProxyWithErrorHandler
  , synchronousRemoteObjectProxyWithErrorHandler
  , resume
  , suspend
  , activate
  , invalidate
  , currentConnection
  , scheduleSendBarrierBlock
  , setCodeSigningRequirement
  , serviceName
  , endpoint
  , exportedInterface
  , setExportedInterface
  , exportedObject
  , setExportedObject
  , remoteObjectInterface
  , setRemoteObjectInterface
  , remoteObjectProxy
  , interruptionHandler
  , setInterruptionHandler
  , invalidationHandler
  , setInvalidationHandler
  , auditSessionIdentifier
  , processIdentifier
  , effectiveUserIdentifier
  , effectiveGroupIdentifier
  , initWithServiceNameSelector
  , initWithMachServiceName_optionsSelector
  , initWithListenerEndpointSelector
  , remoteObjectProxyWithErrorHandlerSelector
  , synchronousRemoteObjectProxyWithErrorHandlerSelector
  , resumeSelector
  , suspendSelector
  , activateSelector
  , invalidateSelector
  , currentConnectionSelector
  , scheduleSendBarrierBlockSelector
  , setCodeSigningRequirementSelector
  , serviceNameSelector
  , endpointSelector
  , exportedInterfaceSelector
  , setExportedInterfaceSelector
  , exportedObjectSelector
  , setExportedObjectSelector
  , remoteObjectInterfaceSelector
  , setRemoteObjectInterfaceSelector
  , remoteObjectProxySelector
  , interruptionHandlerSelector
  , setInterruptionHandlerSelector
  , invalidationHandlerSelector
  , setInvalidationHandlerSelector
  , auditSessionIdentifierSelector
  , processIdentifierSelector
  , effectiveUserIdentifierSelector
  , effectiveGroupIdentifierSelector

  -- * Enum types
  , NSXPCConnectionOptions(NSXPCConnectionOptions)
  , pattern NSXPCConnectionPrivileged

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithServiceName:@
initWithServiceName :: (IsNSXPCConnection nsxpcConnection, IsNSString serviceName) => nsxpcConnection -> serviceName -> IO (Id NSXPCConnection)
initWithServiceName nsxpcConnection  serviceName =
withObjCPtr serviceName $ \raw_serviceName ->
    sendMsg nsxpcConnection (mkSelector "initWithServiceName:") (retPtr retVoid) [argPtr (castPtr raw_serviceName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMachServiceName:options:@
initWithMachServiceName_options :: (IsNSXPCConnection nsxpcConnection, IsNSString name) => nsxpcConnection -> name -> NSXPCConnectionOptions -> IO (Id NSXPCConnection)
initWithMachServiceName_options nsxpcConnection  name options =
withObjCPtr name $ \raw_name ->
    sendMsg nsxpcConnection (mkSelector "initWithMachServiceName:options:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- initWithListenerEndpoint:@
initWithListenerEndpoint :: (IsNSXPCConnection nsxpcConnection, IsNSXPCListenerEndpoint endpoint) => nsxpcConnection -> endpoint -> IO (Id NSXPCConnection)
initWithListenerEndpoint nsxpcConnection  endpoint =
withObjCPtr endpoint $ \raw_endpoint ->
    sendMsg nsxpcConnection (mkSelector "initWithListenerEndpoint:") (retPtr retVoid) [argPtr (castPtr raw_endpoint :: Ptr ())] >>= ownedObject . castPtr

-- | @- remoteObjectProxyWithErrorHandler:@
remoteObjectProxyWithErrorHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO RawId
remoteObjectProxyWithErrorHandler nsxpcConnection  handler =
  fmap (RawId . castPtr) $ sendMsg nsxpcConnection (mkSelector "remoteObjectProxyWithErrorHandler:") (retPtr retVoid) [argPtr (castPtr handler :: Ptr ())]

-- | @- synchronousRemoteObjectProxyWithErrorHandler:@
synchronousRemoteObjectProxyWithErrorHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO RawId
synchronousRemoteObjectProxyWithErrorHandler nsxpcConnection  handler =
  fmap (RawId . castPtr) $ sendMsg nsxpcConnection (mkSelector "synchronousRemoteObjectProxyWithErrorHandler:") (retPtr retVoid) [argPtr (castPtr handler :: Ptr ())]

-- | @- resume@
resume :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
resume nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "resume") retVoid []

-- | @- suspend@
suspend :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
suspend nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "suspend") retVoid []

-- | @- activate@
activate :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
activate nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "activate") retVoid []

-- | @- invalidate@
invalidate :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
invalidate nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "invalidate") retVoid []

-- | @+ currentConnection@
currentConnection :: IO (Id NSXPCConnection)
currentConnection  =
  do
    cls' <- getRequiredClass "NSXPCConnection"
    sendClassMsg cls' (mkSelector "currentConnection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scheduleSendBarrierBlock:@
scheduleSendBarrierBlock :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
scheduleSendBarrierBlock nsxpcConnection  block =
  sendMsg nsxpcConnection (mkSelector "scheduleSendBarrierBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | Sets the code signing requirement for this connection. If the requirement is malformed, an exception is thrown. If new messages do not match the requirement, the connection is invalidated. It is recommended to set this before calling @resume@, as it is an XPC error to call it more than once. See https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/RequirementLang/RequirementLang.html for more information on the format.
--
-- ObjC selector: @- setCodeSigningRequirement:@
setCodeSigningRequirement :: (IsNSXPCConnection nsxpcConnection, IsNSString requirement) => nsxpcConnection -> requirement -> IO ()
setCodeSigningRequirement nsxpcConnection  requirement =
withObjCPtr requirement $ \raw_requirement ->
    sendMsg nsxpcConnection (mkSelector "setCodeSigningRequirement:") retVoid [argPtr (castPtr raw_requirement :: Ptr ())]

-- | @- serviceName@
serviceName :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSString)
serviceName nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "serviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endpoint@
endpoint :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCListenerEndpoint)
endpoint nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- exportedInterface@
exportedInterface :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCInterface)
exportedInterface nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "exportedInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExportedInterface:@
setExportedInterface :: (IsNSXPCConnection nsxpcConnection, IsNSXPCInterface value) => nsxpcConnection -> value -> IO ()
setExportedInterface nsxpcConnection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxpcConnection (mkSelector "setExportedInterface:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exportedObject@
exportedObject :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO RawId
exportedObject nsxpcConnection  =
  fmap (RawId . castPtr) $ sendMsg nsxpcConnection (mkSelector "exportedObject") (retPtr retVoid) []

-- | @- setExportedObject:@
setExportedObject :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> RawId -> IO ()
setExportedObject nsxpcConnection  value =
  sendMsg nsxpcConnection (mkSelector "setExportedObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- remoteObjectInterface@
remoteObjectInterface :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCInterface)
remoteObjectInterface nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "remoteObjectInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRemoteObjectInterface:@
setRemoteObjectInterface :: (IsNSXPCConnection nsxpcConnection, IsNSXPCInterface value) => nsxpcConnection -> value -> IO ()
setRemoteObjectInterface nsxpcConnection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxpcConnection (mkSelector "setRemoteObjectInterface:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- remoteObjectProxy@
remoteObjectProxy :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO RawId
remoteObjectProxy nsxpcConnection  =
  fmap (RawId . castPtr) $ sendMsg nsxpcConnection (mkSelector "remoteObjectProxy") (retPtr retVoid) []

-- | @- interruptionHandler@
interruptionHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Ptr ())
interruptionHandler nsxpcConnection  =
  fmap castPtr $ sendMsg nsxpcConnection (mkSelector "interruptionHandler") (retPtr retVoid) []

-- | @- setInterruptionHandler:@
setInterruptionHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
setInterruptionHandler nsxpcConnection  value =
  sendMsg nsxpcConnection (mkSelector "setInterruptionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- invalidationHandler@
invalidationHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Ptr ())
invalidationHandler nsxpcConnection  =
  fmap castPtr $ sendMsg nsxpcConnection (mkSelector "invalidationHandler") (retPtr retVoid) []

-- | @- setInvalidationHandler:@
setInvalidationHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
setInvalidationHandler nsxpcConnection  value =
  sendMsg nsxpcConnection (mkSelector "setInvalidationHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- auditSessionIdentifier@
auditSessionIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CInt
auditSessionIdentifier nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "auditSessionIdentifier") retCInt []

-- | @- processIdentifier@
processIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CInt
processIdentifier nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "processIdentifier") retCInt []

-- | @- effectiveUserIdentifier@
effectiveUserIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CUInt
effectiveUserIdentifier nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "effectiveUserIdentifier") retCUInt []

-- | @- effectiveGroupIdentifier@
effectiveGroupIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CUInt
effectiveGroupIdentifier nsxpcConnection  =
  sendMsg nsxpcConnection (mkSelector "effectiveGroupIdentifier") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceName:@
initWithServiceNameSelector :: Selector
initWithServiceNameSelector = mkSelector "initWithServiceName:"

-- | @Selector@ for @initWithMachServiceName:options:@
initWithMachServiceName_optionsSelector :: Selector
initWithMachServiceName_optionsSelector = mkSelector "initWithMachServiceName:options:"

-- | @Selector@ for @initWithListenerEndpoint:@
initWithListenerEndpointSelector :: Selector
initWithListenerEndpointSelector = mkSelector "initWithListenerEndpoint:"

-- | @Selector@ for @remoteObjectProxyWithErrorHandler:@
remoteObjectProxyWithErrorHandlerSelector :: Selector
remoteObjectProxyWithErrorHandlerSelector = mkSelector "remoteObjectProxyWithErrorHandler:"

-- | @Selector@ for @synchronousRemoteObjectProxyWithErrorHandler:@
synchronousRemoteObjectProxyWithErrorHandlerSelector :: Selector
synchronousRemoteObjectProxyWithErrorHandlerSelector = mkSelector "synchronousRemoteObjectProxyWithErrorHandler:"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @suspend@
suspendSelector :: Selector
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @currentConnection@
currentConnectionSelector :: Selector
currentConnectionSelector = mkSelector "currentConnection"

-- | @Selector@ for @scheduleSendBarrierBlock:@
scheduleSendBarrierBlockSelector :: Selector
scheduleSendBarrierBlockSelector = mkSelector "scheduleSendBarrierBlock:"

-- | @Selector@ for @setCodeSigningRequirement:@
setCodeSigningRequirementSelector :: Selector
setCodeSigningRequirementSelector = mkSelector "setCodeSigningRequirement:"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @exportedInterface@
exportedInterfaceSelector :: Selector
exportedInterfaceSelector = mkSelector "exportedInterface"

-- | @Selector@ for @setExportedInterface:@
setExportedInterfaceSelector :: Selector
setExportedInterfaceSelector = mkSelector "setExportedInterface:"

-- | @Selector@ for @exportedObject@
exportedObjectSelector :: Selector
exportedObjectSelector = mkSelector "exportedObject"

-- | @Selector@ for @setExportedObject:@
setExportedObjectSelector :: Selector
setExportedObjectSelector = mkSelector "setExportedObject:"

-- | @Selector@ for @remoteObjectInterface@
remoteObjectInterfaceSelector :: Selector
remoteObjectInterfaceSelector = mkSelector "remoteObjectInterface"

-- | @Selector@ for @setRemoteObjectInterface:@
setRemoteObjectInterfaceSelector :: Selector
setRemoteObjectInterfaceSelector = mkSelector "setRemoteObjectInterface:"

-- | @Selector@ for @remoteObjectProxy@
remoteObjectProxySelector :: Selector
remoteObjectProxySelector = mkSelector "remoteObjectProxy"

-- | @Selector@ for @interruptionHandler@
interruptionHandlerSelector :: Selector
interruptionHandlerSelector = mkSelector "interruptionHandler"

-- | @Selector@ for @setInterruptionHandler:@
setInterruptionHandlerSelector :: Selector
setInterruptionHandlerSelector = mkSelector "setInterruptionHandler:"

-- | @Selector@ for @invalidationHandler@
invalidationHandlerSelector :: Selector
invalidationHandlerSelector = mkSelector "invalidationHandler"

-- | @Selector@ for @setInvalidationHandler:@
setInvalidationHandlerSelector :: Selector
setInvalidationHandlerSelector = mkSelector "setInvalidationHandler:"

-- | @Selector@ for @auditSessionIdentifier@
auditSessionIdentifierSelector :: Selector
auditSessionIdentifierSelector = mkSelector "auditSessionIdentifier"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @effectiveUserIdentifier@
effectiveUserIdentifierSelector :: Selector
effectiveUserIdentifierSelector = mkSelector "effectiveUserIdentifier"

-- | @Selector@ for @effectiveGroupIdentifier@
effectiveGroupIdentifierSelector :: Selector
effectiveGroupIdentifierSelector = mkSelector "effectiveGroupIdentifier"

