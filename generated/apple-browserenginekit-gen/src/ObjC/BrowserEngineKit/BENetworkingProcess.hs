{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a running network extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
--
-- Generated bindings for @BENetworkingProcess@.
module ObjC.BrowserEngineKit.BENetworkingProcess
  ( BENetworkingProcess
  , IsBENetworkingProcess(..)
  , init_
  , new
  , networkProcessWithInterruptionHandler_completion
  , networkProcessWithBundleID_interruptionHandler_completion
  , invalidate
  , makeLibXPCConnectionError
  , grantCapability_error_invalidationHandler
  , grantCapability_error
  , grantCapability_errorSelector
  , grantCapability_error_invalidationHandlerSelector
  , initSelector
  , invalidateSelector
  , makeLibXPCConnectionErrorSelector
  , networkProcessWithBundleID_interruptionHandler_completionSelector
  , networkProcessWithInterruptionHandler_completionSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBENetworkingProcess beNetworkingProcess => beNetworkingProcess -> IO (Id BENetworkingProcess)
init_ beNetworkingProcess =
  sendOwnedMessage beNetworkingProcess initSelector

-- | @+ new@
new :: IO (Id BENetworkingProcess)
new  =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    sendOwnedClassMessage cls' newSelector

-- | Asynchronously finds an existing network extension process or launches a one.
--
-- This initializer finds an existing networking extension process. If it’s unable to find an existing process, it launches a new extension process.
--
-- - Parameters:   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BENetworkingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ networkProcessWithInterruptionHandler:completion:@
networkProcessWithInterruptionHandler_completion :: Ptr () -> Ptr () -> IO ()
networkProcessWithInterruptionHandler_completion interruptionHandler completion =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    sendClassMessage cls' networkProcessWithInterruptionHandler_completionSelector interruptionHandler completion

-- | Asynchronously launches a network extension process.
--
-- This initializer launches a new network extension process with the provided bundle identifier.
--
-- - Parameters:   - @bundleID@ : The bundle identifier of the network extension process to launch.   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BENetworkingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ networkProcessWithBundleID:interruptionHandler:completion:@
networkProcessWithBundleID_interruptionHandler_completion :: IsNSString bundleID => bundleID -> Ptr () -> Ptr () -> IO ()
networkProcessWithBundleID_interruptionHandler_completion bundleID interruptionHandler completion =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    sendClassMessage cls' networkProcessWithBundleID_interruptionHandler_completionSelector (toNSString bundleID) interruptionHandler completion

-- | Stops the extension process.
--
-- When you call this method, you tell the system your app no longer needs this extension process. The system will terminate the extension process.
--
-- ObjC selector: @- invalidate@
invalidate :: IsBENetworkingProcess beNetworkingProcess => beNetworkingProcess -> IO ()
invalidate beNetworkingProcess =
  sendMessage beNetworkingProcess invalidateSelector

-- | Creates a new libXPC connection to the extension process.
--
-- This method creates a connection to the extension process and returns it. If it is not possible to make an XPC connection, this method will return nil and populate the @error@ out param.
--
-- - Returns: The connection object representing the created libXPC connection or nil.
--
-- ObjC selector: @- makeLibXPCConnectionError:@
makeLibXPCConnectionError :: (IsBENetworkingProcess beNetworkingProcess, IsNSError error_) => beNetworkingProcess -> error_ -> IO (Id NSObject)
makeLibXPCConnectionError beNetworkingProcess error_ =
  sendMessage beNetworkingProcess makeLibXPCConnectionErrorSelector (toNSError error_)

-- | Grants the specified capability to the process with invalidation handler.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.   - invalidationHandler: The invalidation handler
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandler :: (IsBENetworkingProcess beNetworkingProcess, IsBEProcessCapability capability, IsNSError error_) => beNetworkingProcess -> capability -> error_ -> Ptr () -> IO RawId
grantCapability_error_invalidationHandler beNetworkingProcess capability error_ invalidationHandler =
  sendMessage beNetworkingProcess grantCapability_error_invalidationHandlerSelector (toBEProcessCapability capability) (toNSError error_) invalidationHandler

-- | Grants the specified capability to the process.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:@
grantCapability_error :: (IsBENetworkingProcess beNetworkingProcess, IsBEProcessCapability capability, IsNSError error_) => beNetworkingProcess -> capability -> error_ -> IO RawId
grantCapability_error beNetworkingProcess capability error_ =
  sendMessage beNetworkingProcess grantCapability_errorSelector (toBEProcessCapability capability) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BENetworkingProcess)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BENetworkingProcess)
newSelector = mkSelector "new"

-- | @Selector@ for @networkProcessWithInterruptionHandler:completion:@
networkProcessWithInterruptionHandler_completionSelector :: Selector '[Ptr (), Ptr ()] ()
networkProcessWithInterruptionHandler_completionSelector = mkSelector "networkProcessWithInterruptionHandler:completion:"

-- | @Selector@ for @networkProcessWithBundleID:interruptionHandler:completion:@
networkProcessWithBundleID_interruptionHandler_completionSelector :: Selector '[Id NSString, Ptr (), Ptr ()] ()
networkProcessWithBundleID_interruptionHandler_completionSelector = mkSelector "networkProcessWithBundleID:interruptionHandler:completion:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @makeLibXPCConnectionError:@
makeLibXPCConnectionErrorSelector :: Selector '[Id NSError] (Id NSObject)
makeLibXPCConnectionErrorSelector = mkSelector "makeLibXPCConnectionError:"

-- | @Selector@ for @grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandlerSelector :: Selector '[Id BEProcessCapability, Id NSError, Ptr ()] RawId
grantCapability_error_invalidationHandlerSelector = mkSelector "grantCapability:error:invalidationHandler:"

-- | @Selector@ for @grantCapability:error:@
grantCapability_errorSelector :: Selector '[Id BEProcessCapability, Id NSError] RawId
grantCapability_errorSelector = mkSelector "grantCapability:error:"

