{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a running GPU extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
--
-- Generated bindings for @BERenderingProcess@.
module ObjC.BrowserEngineKit.BERenderingProcess
  ( BERenderingProcess
  , IsBERenderingProcess(..)
  , init_
  , new
  , renderingProcessWithInterruptionHandler_completion
  , renderingProcessWithBundleID_interruptionHandler_completion
  , invalidate
  , makeLibXPCConnectionError
  , grantCapability_error_invalidationHandler
  , grantCapability_error
  , grantCapability_errorSelector
  , grantCapability_error_invalidationHandlerSelector
  , initSelector
  , invalidateSelector
  , makeLibXPCConnectionErrorSelector
  , newSelector
  , renderingProcessWithBundleID_interruptionHandler_completionSelector
  , renderingProcessWithInterruptionHandler_completionSelector


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
init_ :: IsBERenderingProcess beRenderingProcess => beRenderingProcess -> IO (Id BERenderingProcess)
init_ beRenderingProcess =
  sendOwnedMessage beRenderingProcess initSelector

-- | @+ new@
new :: IO (Id BERenderingProcess)
new  =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    sendOwnedClassMessage cls' newSelector

-- | Asynchronously finds an existing extension process or launches one.
--
-- This initializer finds an existing extension rendering process. If it’s unable to find an existing process, it launches a new extension process.
--
-- - Parameters:   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BERenderingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ renderingProcessWithInterruptionHandler:completion:@
renderingProcessWithInterruptionHandler_completion :: Ptr () -> Ptr () -> IO ()
renderingProcessWithInterruptionHandler_completion interruptionHandler completion =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    sendClassMessage cls' renderingProcessWithInterruptionHandler_completionSelector interruptionHandler completion

-- | Asynchronously launches a rendering extension process.
--
-- This initializer launches a new rendering extension process with the provided bundle identifier.
--
-- - Parameters:   - @bundleID@ : The bundle identifier of the rendering extension process to launch.   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BERenderingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ renderingProcessWithBundleID:interruptionHandler:completion:@
renderingProcessWithBundleID_interruptionHandler_completion :: IsNSString bundleID => bundleID -> Ptr () -> Ptr () -> IO ()
renderingProcessWithBundleID_interruptionHandler_completion bundleID interruptionHandler completion =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    sendClassMessage cls' renderingProcessWithBundleID_interruptionHandler_completionSelector (toNSString bundleID) interruptionHandler completion

-- | Stops the extension process.
--
-- When you call this method, you tell the system your app no longer needs this extension process. If this is the last connection from the host process to the extension process, the system terminates the extension process.
--
-- ObjC selector: @- invalidate@
invalidate :: IsBERenderingProcess beRenderingProcess => beRenderingProcess -> IO ()
invalidate beRenderingProcess =
  sendMessage beRenderingProcess invalidateSelector

-- | Creates a new libXPC connection to the extension process.
--
-- This method creates a connection to the extension process and returns it. If it is not possible to make an XPC connection, this method will return nil and populate the @error@ out param.
--
-- - Returns: The connection object representing the created libXPC connection or nil.
--
-- ObjC selector: @- makeLibXPCConnectionError:@
makeLibXPCConnectionError :: (IsBERenderingProcess beRenderingProcess, IsNSError error_) => beRenderingProcess -> error_ -> IO (Id NSObject)
makeLibXPCConnectionError beRenderingProcess error_ =
  sendMessage beRenderingProcess makeLibXPCConnectionErrorSelector (toNSError error_)

-- | Grants the specified capability to the process with invalidation handler.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.   - invalidationHandler: The invalidation handler
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandler :: (IsBERenderingProcess beRenderingProcess, IsBEProcessCapability capability, IsNSError error_) => beRenderingProcess -> capability -> error_ -> Ptr () -> IO RawId
grantCapability_error_invalidationHandler beRenderingProcess capability error_ invalidationHandler =
  sendMessage beRenderingProcess grantCapability_error_invalidationHandlerSelector (toBEProcessCapability capability) (toNSError error_) invalidationHandler

-- | Grants the specified capability to the process.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:@
grantCapability_error :: (IsBERenderingProcess beRenderingProcess, IsBEProcessCapability capability, IsNSError error_) => beRenderingProcess -> capability -> error_ -> IO RawId
grantCapability_error beRenderingProcess capability error_ =
  sendMessage beRenderingProcess grantCapability_errorSelector (toBEProcessCapability capability) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BERenderingProcess)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BERenderingProcess)
newSelector = mkSelector "new"

-- | @Selector@ for @renderingProcessWithInterruptionHandler:completion:@
renderingProcessWithInterruptionHandler_completionSelector :: Selector '[Ptr (), Ptr ()] ()
renderingProcessWithInterruptionHandler_completionSelector = mkSelector "renderingProcessWithInterruptionHandler:completion:"

-- | @Selector@ for @renderingProcessWithBundleID:interruptionHandler:completion:@
renderingProcessWithBundleID_interruptionHandler_completionSelector :: Selector '[Id NSString, Ptr (), Ptr ()] ()
renderingProcessWithBundleID_interruptionHandler_completionSelector = mkSelector "renderingProcessWithBundleID:interruptionHandler:completion:"

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

