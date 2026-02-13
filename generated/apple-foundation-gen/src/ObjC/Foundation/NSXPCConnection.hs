{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activateSelector
  , auditSessionIdentifierSelector
  , currentConnectionSelector
  , effectiveGroupIdentifierSelector
  , effectiveUserIdentifierSelector
  , endpointSelector
  , exportedInterfaceSelector
  , exportedObjectSelector
  , initWithListenerEndpointSelector
  , initWithMachServiceName_optionsSelector
  , initWithServiceNameSelector
  , interruptionHandlerSelector
  , invalidateSelector
  , invalidationHandlerSelector
  , processIdentifierSelector
  , remoteObjectInterfaceSelector
  , remoteObjectProxySelector
  , remoteObjectProxyWithErrorHandlerSelector
  , resumeSelector
  , scheduleSendBarrierBlockSelector
  , serviceNameSelector
  , setCodeSigningRequirementSelector
  , setExportedInterfaceSelector
  , setExportedObjectSelector
  , setInterruptionHandlerSelector
  , setInvalidationHandlerSelector
  , setRemoteObjectInterfaceSelector
  , suspendSelector
  , synchronousRemoteObjectProxyWithErrorHandlerSelector

  -- * Enum types
  , NSXPCConnectionOptions(NSXPCConnectionOptions)
  , pattern NSXPCConnectionPrivileged

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithServiceName:@
initWithServiceName :: (IsNSXPCConnection nsxpcConnection, IsNSString serviceName) => nsxpcConnection -> serviceName -> IO (Id NSXPCConnection)
initWithServiceName nsxpcConnection serviceName =
  sendOwnedMessage nsxpcConnection initWithServiceNameSelector (toNSString serviceName)

-- | @- initWithMachServiceName:options:@
initWithMachServiceName_options :: (IsNSXPCConnection nsxpcConnection, IsNSString name) => nsxpcConnection -> name -> NSXPCConnectionOptions -> IO (Id NSXPCConnection)
initWithMachServiceName_options nsxpcConnection name options =
  sendOwnedMessage nsxpcConnection initWithMachServiceName_optionsSelector (toNSString name) options

-- | @- initWithListenerEndpoint:@
initWithListenerEndpoint :: (IsNSXPCConnection nsxpcConnection, IsNSXPCListenerEndpoint endpoint) => nsxpcConnection -> endpoint -> IO (Id NSXPCConnection)
initWithListenerEndpoint nsxpcConnection endpoint =
  sendOwnedMessage nsxpcConnection initWithListenerEndpointSelector (toNSXPCListenerEndpoint endpoint)

-- | @- remoteObjectProxyWithErrorHandler:@
remoteObjectProxyWithErrorHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO RawId
remoteObjectProxyWithErrorHandler nsxpcConnection handler =
  sendMessage nsxpcConnection remoteObjectProxyWithErrorHandlerSelector handler

-- | @- synchronousRemoteObjectProxyWithErrorHandler:@
synchronousRemoteObjectProxyWithErrorHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO RawId
synchronousRemoteObjectProxyWithErrorHandler nsxpcConnection handler =
  sendMessage nsxpcConnection synchronousRemoteObjectProxyWithErrorHandlerSelector handler

-- | @- resume@
resume :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
resume nsxpcConnection =
  sendMessage nsxpcConnection resumeSelector

-- | @- suspend@
suspend :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
suspend nsxpcConnection =
  sendMessage nsxpcConnection suspendSelector

-- | @- activate@
activate :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
activate nsxpcConnection =
  sendMessage nsxpcConnection activateSelector

-- | @- invalidate@
invalidate :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO ()
invalidate nsxpcConnection =
  sendMessage nsxpcConnection invalidateSelector

-- | @+ currentConnection@
currentConnection :: IO (Id NSXPCConnection)
currentConnection  =
  do
    cls' <- getRequiredClass "NSXPCConnection"
    sendClassMessage cls' currentConnectionSelector

-- | @- scheduleSendBarrierBlock:@
scheduleSendBarrierBlock :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
scheduleSendBarrierBlock nsxpcConnection block =
  sendMessage nsxpcConnection scheduleSendBarrierBlockSelector block

-- | Sets the code signing requirement for this connection. If the requirement is malformed, an exception is thrown. If new messages do not match the requirement, the connection is invalidated. It is recommended to set this before calling @resume@, as it is an XPC error to call it more than once. See https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/RequirementLang/RequirementLang.html for more information on the format.
--
-- ObjC selector: @- setCodeSigningRequirement:@
setCodeSigningRequirement :: (IsNSXPCConnection nsxpcConnection, IsNSString requirement) => nsxpcConnection -> requirement -> IO ()
setCodeSigningRequirement nsxpcConnection requirement =
  sendMessage nsxpcConnection setCodeSigningRequirementSelector (toNSString requirement)

-- | @- serviceName@
serviceName :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSString)
serviceName nsxpcConnection =
  sendMessage nsxpcConnection serviceNameSelector

-- | @- endpoint@
endpoint :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCListenerEndpoint)
endpoint nsxpcConnection =
  sendMessage nsxpcConnection endpointSelector

-- | @- exportedInterface@
exportedInterface :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCInterface)
exportedInterface nsxpcConnection =
  sendMessage nsxpcConnection exportedInterfaceSelector

-- | @- setExportedInterface:@
setExportedInterface :: (IsNSXPCConnection nsxpcConnection, IsNSXPCInterface value) => nsxpcConnection -> value -> IO ()
setExportedInterface nsxpcConnection value =
  sendMessage nsxpcConnection setExportedInterfaceSelector (toNSXPCInterface value)

-- | @- exportedObject@
exportedObject :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO RawId
exportedObject nsxpcConnection =
  sendMessage nsxpcConnection exportedObjectSelector

-- | @- setExportedObject:@
setExportedObject :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> RawId -> IO ()
setExportedObject nsxpcConnection value =
  sendMessage nsxpcConnection setExportedObjectSelector value

-- | @- remoteObjectInterface@
remoteObjectInterface :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Id NSXPCInterface)
remoteObjectInterface nsxpcConnection =
  sendMessage nsxpcConnection remoteObjectInterfaceSelector

-- | @- setRemoteObjectInterface:@
setRemoteObjectInterface :: (IsNSXPCConnection nsxpcConnection, IsNSXPCInterface value) => nsxpcConnection -> value -> IO ()
setRemoteObjectInterface nsxpcConnection value =
  sendMessage nsxpcConnection setRemoteObjectInterfaceSelector (toNSXPCInterface value)

-- | @- remoteObjectProxy@
remoteObjectProxy :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO RawId
remoteObjectProxy nsxpcConnection =
  sendMessage nsxpcConnection remoteObjectProxySelector

-- | @- interruptionHandler@
interruptionHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Ptr ())
interruptionHandler nsxpcConnection =
  sendMessage nsxpcConnection interruptionHandlerSelector

-- | @- setInterruptionHandler:@
setInterruptionHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
setInterruptionHandler nsxpcConnection value =
  sendMessage nsxpcConnection setInterruptionHandlerSelector value

-- | @- invalidationHandler@
invalidationHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO (Ptr ())
invalidationHandler nsxpcConnection =
  sendMessage nsxpcConnection invalidationHandlerSelector

-- | @- setInvalidationHandler:@
setInvalidationHandler :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> Ptr () -> IO ()
setInvalidationHandler nsxpcConnection value =
  sendMessage nsxpcConnection setInvalidationHandlerSelector value

-- | @- auditSessionIdentifier@
auditSessionIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CInt
auditSessionIdentifier nsxpcConnection =
  sendMessage nsxpcConnection auditSessionIdentifierSelector

-- | @- processIdentifier@
processIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CInt
processIdentifier nsxpcConnection =
  sendMessage nsxpcConnection processIdentifierSelector

-- | @- effectiveUserIdentifier@
effectiveUserIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CUInt
effectiveUserIdentifier nsxpcConnection =
  sendMessage nsxpcConnection effectiveUserIdentifierSelector

-- | @- effectiveGroupIdentifier@
effectiveGroupIdentifier :: IsNSXPCConnection nsxpcConnection => nsxpcConnection -> IO CUInt
effectiveGroupIdentifier nsxpcConnection =
  sendMessage nsxpcConnection effectiveGroupIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceName:@
initWithServiceNameSelector :: Selector '[Id NSString] (Id NSXPCConnection)
initWithServiceNameSelector = mkSelector "initWithServiceName:"

-- | @Selector@ for @initWithMachServiceName:options:@
initWithMachServiceName_optionsSelector :: Selector '[Id NSString, NSXPCConnectionOptions] (Id NSXPCConnection)
initWithMachServiceName_optionsSelector = mkSelector "initWithMachServiceName:options:"

-- | @Selector@ for @initWithListenerEndpoint:@
initWithListenerEndpointSelector :: Selector '[Id NSXPCListenerEndpoint] (Id NSXPCConnection)
initWithListenerEndpointSelector = mkSelector "initWithListenerEndpoint:"

-- | @Selector@ for @remoteObjectProxyWithErrorHandler:@
remoteObjectProxyWithErrorHandlerSelector :: Selector '[Ptr ()] RawId
remoteObjectProxyWithErrorHandlerSelector = mkSelector "remoteObjectProxyWithErrorHandler:"

-- | @Selector@ for @synchronousRemoteObjectProxyWithErrorHandler:@
synchronousRemoteObjectProxyWithErrorHandlerSelector :: Selector '[Ptr ()] RawId
synchronousRemoteObjectProxyWithErrorHandlerSelector = mkSelector "synchronousRemoteObjectProxyWithErrorHandler:"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @suspend@
suspendSelector :: Selector '[] ()
suspendSelector = mkSelector "suspend"

-- | @Selector@ for @activate@
activateSelector :: Selector '[] ()
activateSelector = mkSelector "activate"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @currentConnection@
currentConnectionSelector :: Selector '[] (Id NSXPCConnection)
currentConnectionSelector = mkSelector "currentConnection"

-- | @Selector@ for @scheduleSendBarrierBlock:@
scheduleSendBarrierBlockSelector :: Selector '[Ptr ()] ()
scheduleSendBarrierBlockSelector = mkSelector "scheduleSendBarrierBlock:"

-- | @Selector@ for @setCodeSigningRequirement:@
setCodeSigningRequirementSelector :: Selector '[Id NSString] ()
setCodeSigningRequirementSelector = mkSelector "setCodeSigningRequirement:"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector '[] (Id NSString)
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSXPCListenerEndpoint)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @exportedInterface@
exportedInterfaceSelector :: Selector '[] (Id NSXPCInterface)
exportedInterfaceSelector = mkSelector "exportedInterface"

-- | @Selector@ for @setExportedInterface:@
setExportedInterfaceSelector :: Selector '[Id NSXPCInterface] ()
setExportedInterfaceSelector = mkSelector "setExportedInterface:"

-- | @Selector@ for @exportedObject@
exportedObjectSelector :: Selector '[] RawId
exportedObjectSelector = mkSelector "exportedObject"

-- | @Selector@ for @setExportedObject:@
setExportedObjectSelector :: Selector '[RawId] ()
setExportedObjectSelector = mkSelector "setExportedObject:"

-- | @Selector@ for @remoteObjectInterface@
remoteObjectInterfaceSelector :: Selector '[] (Id NSXPCInterface)
remoteObjectInterfaceSelector = mkSelector "remoteObjectInterface"

-- | @Selector@ for @setRemoteObjectInterface:@
setRemoteObjectInterfaceSelector :: Selector '[Id NSXPCInterface] ()
setRemoteObjectInterfaceSelector = mkSelector "setRemoteObjectInterface:"

-- | @Selector@ for @remoteObjectProxy@
remoteObjectProxySelector :: Selector '[] RawId
remoteObjectProxySelector = mkSelector "remoteObjectProxy"

-- | @Selector@ for @interruptionHandler@
interruptionHandlerSelector :: Selector '[] (Ptr ())
interruptionHandlerSelector = mkSelector "interruptionHandler"

-- | @Selector@ for @setInterruptionHandler:@
setInterruptionHandlerSelector :: Selector '[Ptr ()] ()
setInterruptionHandlerSelector = mkSelector "setInterruptionHandler:"

-- | @Selector@ for @invalidationHandler@
invalidationHandlerSelector :: Selector '[] (Ptr ())
invalidationHandlerSelector = mkSelector "invalidationHandler"

-- | @Selector@ for @setInvalidationHandler:@
setInvalidationHandlerSelector :: Selector '[Ptr ()] ()
setInvalidationHandlerSelector = mkSelector "setInvalidationHandler:"

-- | @Selector@ for @auditSessionIdentifier@
auditSessionIdentifierSelector :: Selector '[] CInt
auditSessionIdentifierSelector = mkSelector "auditSessionIdentifier"

-- | @Selector@ for @processIdentifier@
processIdentifierSelector :: Selector '[] CInt
processIdentifierSelector = mkSelector "processIdentifier"

-- | @Selector@ for @effectiveUserIdentifier@
effectiveUserIdentifierSelector :: Selector '[] CUInt
effectiveUserIdentifierSelector = mkSelector "effectiveUserIdentifier"

-- | @Selector@ for @effectiveGroupIdentifier@
effectiveGroupIdentifierSelector :: Selector '[] CUInt
effectiveGroupIdentifierSelector = mkSelector "effectiveGroupIdentifier"

