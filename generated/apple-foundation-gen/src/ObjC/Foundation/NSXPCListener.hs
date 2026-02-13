{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXPCListener@.
module ObjC.Foundation.NSXPCListener
  ( NSXPCListener
  , IsNSXPCListener(..)
  , serviceListener
  , anonymousListener
  , initWithMachServiceName
  , resume
  , suspend
  , activate
  , invalidate
  , setConnectionCodeSigningRequirement
  , delegate
  , setDelegate
  , endpoint
  , activateSelector
  , anonymousListenerSelector
  , delegateSelector
  , endpointSelector
  , initWithMachServiceNameSelector
  , invalidateSelector
  , resumeSelector
  , serviceListenerSelector
  , setConnectionCodeSigningRequirementSelector
  , setDelegateSelector
  , suspendSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ serviceListener@
serviceListener :: IO (Id NSXPCListener)
serviceListener  =
  do
    cls' <- getRequiredClass "NSXPCListener"
    sendClassMessage cls' serviceListenerSelector

-- | @+ anonymousListener@
anonymousListener :: IO (Id NSXPCListener)
anonymousListener  =
  do
    cls' <- getRequiredClass "NSXPCListener"
    sendClassMessage cls' anonymousListenerSelector

-- | @- initWithMachServiceName:@
initWithMachServiceName :: (IsNSXPCListener nsxpcListener, IsNSString name) => nsxpcListener -> name -> IO (Id NSXPCListener)
initWithMachServiceName nsxpcListener name =
  sendOwnedMessage nsxpcListener initWithMachServiceNameSelector (toNSString name)

-- | @- resume@
resume :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
resume nsxpcListener =
  sendMessage nsxpcListener resumeSelector

-- | @- suspend@
suspend :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
suspend nsxpcListener =
  sendMessage nsxpcListener suspendSelector

-- | @- activate@
activate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
activate nsxpcListener =
  sendMessage nsxpcListener activateSelector

-- | @- invalidate@
invalidate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
invalidate nsxpcListener =
  sendMessage nsxpcListener invalidateSelector

-- | Sets the code signing requirement for new connections. If the requirement is malformed, an exception is thrown. If new peer connections do not match the requirement, the incoming connection is automatically rejected before consulting the delegate. This method will only work on @anonymousListener@ or @initWithMachServiceName@ listener instances. Use on other types of listeners will result in an assertion failure. See https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/RequirementLang/RequirementLang.html for more information on the format.
--
-- ObjC selector: @- setConnectionCodeSigningRequirement:@
setConnectionCodeSigningRequirement :: (IsNSXPCListener nsxpcListener, IsNSString requirement) => nsxpcListener -> requirement -> IO ()
setConnectionCodeSigningRequirement nsxpcListener requirement =
  sendMessage nsxpcListener setConnectionCodeSigningRequirementSelector (toNSString requirement)

-- | @- delegate@
delegate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO RawId
delegate nsxpcListener =
  sendMessage nsxpcListener delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSXPCListener nsxpcListener => nsxpcListener -> RawId -> IO ()
setDelegate nsxpcListener value =
  sendMessage nsxpcListener setDelegateSelector value

-- | @- endpoint@
endpoint :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO (Id NSXPCListenerEndpoint)
endpoint nsxpcListener =
  sendMessage nsxpcListener endpointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serviceListener@
serviceListenerSelector :: Selector '[] (Id NSXPCListener)
serviceListenerSelector = mkSelector "serviceListener"

-- | @Selector@ for @anonymousListener@
anonymousListenerSelector :: Selector '[] (Id NSXPCListener)
anonymousListenerSelector = mkSelector "anonymousListener"

-- | @Selector@ for @initWithMachServiceName:@
initWithMachServiceNameSelector :: Selector '[Id NSString] (Id NSXPCListener)
initWithMachServiceNameSelector = mkSelector "initWithMachServiceName:"

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

-- | @Selector@ for @setConnectionCodeSigningRequirement:@
setConnectionCodeSigningRequirementSelector :: Selector '[Id NSString] ()
setConnectionCodeSigningRequirementSelector = mkSelector "setConnectionCodeSigningRequirement:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSXPCListenerEndpoint)
endpointSelector = mkSelector "endpoint"

