{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLCaptureManager@.
module ObjC.Metal.MTLCaptureManager
  ( MTLCaptureManager
  , IsMTLCaptureManager(..)
  , sharedCaptureManager
  , init_
  , newCaptureScopeWithDevice
  , newCaptureScopeWithCommandQueue
  , newCaptureScopeWithMTL4CommandQueue
  , supportsDestination
  , startCaptureWithDescriptor_error
  , startCaptureWithDevice
  , startCaptureWithCommandQueue
  , startCaptureWithScope
  , stopCapture
  , defaultCaptureScope
  , setDefaultCaptureScope
  , isCapturing
  , defaultCaptureScopeSelector
  , initSelector
  , isCapturingSelector
  , newCaptureScopeWithCommandQueueSelector
  , newCaptureScopeWithDeviceSelector
  , newCaptureScopeWithMTL4CommandQueueSelector
  , setDefaultCaptureScopeSelector
  , sharedCaptureManagerSelector
  , startCaptureWithCommandQueueSelector
  , startCaptureWithDescriptor_errorSelector
  , startCaptureWithDeviceSelector
  , startCaptureWithScopeSelector
  , stopCaptureSelector
  , supportsDestinationSelector

  -- * Enum types
  , MTLCaptureDestination(MTLCaptureDestination)
  , pattern MTLCaptureDestinationDeveloperTools
  , pattern MTLCaptureDestinationGPUTraceDocument

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Retrieves the shared capture manager for this process. There is only one capture manager per process.    The capture manager allows the user to create capture scopes and trigger captures from code.    When a capture has been completed, it will be displayed in Xcode and the application will be paused.
--
-- : only MTLCommandBuffers created after starting a capture and committed before stopping it are captured.
--
-- ObjC selector: @+ sharedCaptureManager@
sharedCaptureManager :: IO (Id MTLCaptureManager)
sharedCaptureManager  =
  do
    cls' <- getRequiredClass "MTLCaptureManager"
    sendClassMessage cls' sharedCaptureManagerSelector

-- | @- init@
init_ :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO (Id MTLCaptureManager)
init_ mtlCaptureManager =
  sendOwnedMessage mtlCaptureManager initSelector

-- | @- newCaptureScopeWithDevice:@
newCaptureScopeWithDevice :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithDevice mtlCaptureManager device =
  sendOwnedMessage mtlCaptureManager newCaptureScopeWithDeviceSelector device

-- | @- newCaptureScopeWithCommandQueue:@
newCaptureScopeWithCommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithCommandQueue mtlCaptureManager commandQueue =
  sendOwnedMessage mtlCaptureManager newCaptureScopeWithCommandQueueSelector commandQueue

-- | @- newCaptureScopeWithMTL4CommandQueue:@
newCaptureScopeWithMTL4CommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithMTL4CommandQueue mtlCaptureManager commandQueue =
  sendOwnedMessage mtlCaptureManager newCaptureScopeWithMTL4CommandQueueSelector commandQueue

-- | @- supportsDestination:@
supportsDestination :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> MTLCaptureDestination -> IO Bool
supportsDestination mtlCaptureManager destination =
  sendMessage mtlCaptureManager supportsDestinationSelector destination

-- | Start capturing until stopCapture is called.
--
-- @descriptor@ — MTLCaptureDescriptor specifies the parameters.
--
-- @error@ — Optional error output to check why a capture could not be started.
--
-- Returns: true if the capture was successfully started, otherwise false.
--
-- Only MTLCommandBuffer​s created after starting and committed before stopping it are captured.
--
-- ObjC selector: @- startCaptureWithDescriptor:error:@
startCaptureWithDescriptor_error :: (IsMTLCaptureManager mtlCaptureManager, IsMTLCaptureDescriptor descriptor, IsNSError error_) => mtlCaptureManager -> descriptor -> error_ -> IO Bool
startCaptureWithDescriptor_error mtlCaptureManager descriptor error_ =
  sendMessage mtlCaptureManager startCaptureWithDescriptor_errorSelector (toMTLCaptureDescriptor descriptor) (toNSError error_)

-- | @- startCaptureWithDevice:@
startCaptureWithDevice :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithDevice mtlCaptureManager device =
  sendMessage mtlCaptureManager startCaptureWithDeviceSelector device

-- | @- startCaptureWithCommandQueue:@
startCaptureWithCommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithCommandQueue mtlCaptureManager commandQueue =
  sendMessage mtlCaptureManager startCaptureWithCommandQueueSelector commandQueue

-- | @- startCaptureWithScope:@
startCaptureWithScope :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithScope mtlCaptureManager captureScope =
  sendMessage mtlCaptureManager startCaptureWithScopeSelector captureScope

-- | @- stopCapture@
stopCapture :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO ()
stopCapture mtlCaptureManager =
  sendMessage mtlCaptureManager stopCaptureSelector

-- | @- defaultCaptureScope@
defaultCaptureScope :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO RawId
defaultCaptureScope mtlCaptureManager =
  sendMessage mtlCaptureManager defaultCaptureScopeSelector

-- | @- setDefaultCaptureScope:@
setDefaultCaptureScope :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
setDefaultCaptureScope mtlCaptureManager value =
  sendMessage mtlCaptureManager setDefaultCaptureScopeSelector value

-- | @- isCapturing@
isCapturing :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO Bool
isCapturing mtlCaptureManager =
  sendMessage mtlCaptureManager isCapturingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCaptureManager@
sharedCaptureManagerSelector :: Selector '[] (Id MTLCaptureManager)
sharedCaptureManagerSelector = mkSelector "sharedCaptureManager"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTLCaptureManager)
initSelector = mkSelector "init"

-- | @Selector@ for @newCaptureScopeWithDevice:@
newCaptureScopeWithDeviceSelector :: Selector '[RawId] RawId
newCaptureScopeWithDeviceSelector = mkSelector "newCaptureScopeWithDevice:"

-- | @Selector@ for @newCaptureScopeWithCommandQueue:@
newCaptureScopeWithCommandQueueSelector :: Selector '[RawId] RawId
newCaptureScopeWithCommandQueueSelector = mkSelector "newCaptureScopeWithCommandQueue:"

-- | @Selector@ for @newCaptureScopeWithMTL4CommandQueue:@
newCaptureScopeWithMTL4CommandQueueSelector :: Selector '[RawId] RawId
newCaptureScopeWithMTL4CommandQueueSelector = mkSelector "newCaptureScopeWithMTL4CommandQueue:"

-- | @Selector@ for @supportsDestination:@
supportsDestinationSelector :: Selector '[MTLCaptureDestination] Bool
supportsDestinationSelector = mkSelector "supportsDestination:"

-- | @Selector@ for @startCaptureWithDescriptor:error:@
startCaptureWithDescriptor_errorSelector :: Selector '[Id MTLCaptureDescriptor, Id NSError] Bool
startCaptureWithDescriptor_errorSelector = mkSelector "startCaptureWithDescriptor:error:"

-- | @Selector@ for @startCaptureWithDevice:@
startCaptureWithDeviceSelector :: Selector '[RawId] ()
startCaptureWithDeviceSelector = mkSelector "startCaptureWithDevice:"

-- | @Selector@ for @startCaptureWithCommandQueue:@
startCaptureWithCommandQueueSelector :: Selector '[RawId] ()
startCaptureWithCommandQueueSelector = mkSelector "startCaptureWithCommandQueue:"

-- | @Selector@ for @startCaptureWithScope:@
startCaptureWithScopeSelector :: Selector '[RawId] ()
startCaptureWithScopeSelector = mkSelector "startCaptureWithScope:"

-- | @Selector@ for @stopCapture@
stopCaptureSelector :: Selector '[] ()
stopCaptureSelector = mkSelector "stopCapture"

-- | @Selector@ for @defaultCaptureScope@
defaultCaptureScopeSelector :: Selector '[] RawId
defaultCaptureScopeSelector = mkSelector "defaultCaptureScope"

-- | @Selector@ for @setDefaultCaptureScope:@
setDefaultCaptureScopeSelector :: Selector '[RawId] ()
setDefaultCaptureScopeSelector = mkSelector "setDefaultCaptureScope:"

-- | @Selector@ for @isCapturing@
isCapturingSelector :: Selector '[] Bool
isCapturingSelector = mkSelector "isCapturing"

