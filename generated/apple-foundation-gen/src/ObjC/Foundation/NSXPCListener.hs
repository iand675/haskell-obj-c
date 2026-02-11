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
  , serviceListenerSelector
  , anonymousListenerSelector
  , initWithMachServiceNameSelector
  , resumeSelector
  , suspendSelector
  , activateSelector
  , invalidateSelector
  , setConnectionCodeSigningRequirementSelector
  , delegateSelector
  , setDelegateSelector
  , endpointSelector


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

-- | @+ serviceListener@
serviceListener :: IO (Id NSXPCListener)
serviceListener  =
  do
    cls' <- getRequiredClass "NSXPCListener"
    sendClassMsg cls' (mkSelector "serviceListener") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ anonymousListener@
anonymousListener :: IO (Id NSXPCListener)
anonymousListener  =
  do
    cls' <- getRequiredClass "NSXPCListener"
    sendClassMsg cls' (mkSelector "anonymousListener") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithMachServiceName:@
initWithMachServiceName :: (IsNSXPCListener nsxpcListener, IsNSString name) => nsxpcListener -> name -> IO (Id NSXPCListener)
initWithMachServiceName nsxpcListener  name =
  withObjCPtr name $ \raw_name ->
      sendMsg nsxpcListener (mkSelector "initWithMachServiceName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- resume@
resume :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
resume nsxpcListener  =
    sendMsg nsxpcListener (mkSelector "resume") retVoid []

-- | @- suspend@
suspend :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
suspend nsxpcListener  =
    sendMsg nsxpcListener (mkSelector "suspend") retVoid []

-- | @- activate@
activate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
activate nsxpcListener  =
    sendMsg nsxpcListener (mkSelector "activate") retVoid []

-- | @- invalidate@
invalidate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO ()
invalidate nsxpcListener  =
    sendMsg nsxpcListener (mkSelector "invalidate") retVoid []

-- | Sets the code signing requirement for new connections. If the requirement is malformed, an exception is thrown. If new peer connections do not match the requirement, the incoming connection is automatically rejected before consulting the delegate. This method will only work on @anonymousListener@ or @initWithMachServiceName@ listener instances. Use on other types of listeners will result in an assertion failure. See https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/RequirementLang/RequirementLang.html for more information on the format.
--
-- ObjC selector: @- setConnectionCodeSigningRequirement:@
setConnectionCodeSigningRequirement :: (IsNSXPCListener nsxpcListener, IsNSString requirement) => nsxpcListener -> requirement -> IO ()
setConnectionCodeSigningRequirement nsxpcListener  requirement =
  withObjCPtr requirement $ \raw_requirement ->
      sendMsg nsxpcListener (mkSelector "setConnectionCodeSigningRequirement:") retVoid [argPtr (castPtr raw_requirement :: Ptr ())]

-- | @- delegate@
delegate :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO RawId
delegate nsxpcListener  =
    fmap (RawId . castPtr) $ sendMsg nsxpcListener (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSXPCListener nsxpcListener => nsxpcListener -> RawId -> IO ()
setDelegate nsxpcListener  value =
    sendMsg nsxpcListener (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- endpoint@
endpoint :: IsNSXPCListener nsxpcListener => nsxpcListener -> IO (Id NSXPCListenerEndpoint)
endpoint nsxpcListener  =
    sendMsg nsxpcListener (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serviceListener@
serviceListenerSelector :: Selector
serviceListenerSelector = mkSelector "serviceListener"

-- | @Selector@ for @anonymousListener@
anonymousListenerSelector :: Selector
anonymousListenerSelector = mkSelector "anonymousListener"

-- | @Selector@ for @initWithMachServiceName:@
initWithMachServiceNameSelector :: Selector
initWithMachServiceNameSelector = mkSelector "initWithMachServiceName:"

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

-- | @Selector@ for @setConnectionCodeSigningRequirement:@
setConnectionCodeSigningRequirementSelector :: Selector
setConnectionCodeSigningRequirementSelector = mkSelector "setConnectionCodeSigningRequirement:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

