{-# LANGUAGE PatternSynonyms #-}
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
  , isCapturing
  , sharedCaptureManagerSelector
  , initSelector
  , newCaptureScopeWithDeviceSelector
  , newCaptureScopeWithCommandQueueSelector
  , newCaptureScopeWithMTL4CommandQueueSelector
  , supportsDestinationSelector
  , startCaptureWithDescriptor_errorSelector
  , startCaptureWithDeviceSelector
  , startCaptureWithCommandQueueSelector
  , startCaptureWithScopeSelector
  , stopCaptureSelector
  , isCapturingSelector

  -- * Enum types
  , MTLCaptureDestination(MTLCaptureDestination)
  , pattern MTLCaptureDestinationDeveloperTools
  , pattern MTLCaptureDestinationGPUTraceDocument

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
    sendClassMsg cls' (mkSelector "sharedCaptureManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO (Id MTLCaptureManager)
init_ mtlCaptureManager  =
  sendMsg mtlCaptureManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- newCaptureScopeWithDevice:@
newCaptureScopeWithDevice :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithDevice mtlCaptureManager  device =
  fmap (RawId . castPtr) $ sendMsg mtlCaptureManager (mkSelector "newCaptureScopeWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | @- newCaptureScopeWithCommandQueue:@
newCaptureScopeWithCommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithCommandQueue mtlCaptureManager  commandQueue =
  fmap (RawId . castPtr) $ sendMsg mtlCaptureManager (mkSelector "newCaptureScopeWithCommandQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ())]

-- | @- newCaptureScopeWithMTL4CommandQueue:@
newCaptureScopeWithMTL4CommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO RawId
newCaptureScopeWithMTL4CommandQueue mtlCaptureManager  commandQueue =
  fmap (RawId . castPtr) $ sendMsg mtlCaptureManager (mkSelector "newCaptureScopeWithMTL4CommandQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ())]

-- | @- supportsDestination:@
supportsDestination :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> MTLCaptureDestination -> IO Bool
supportsDestination mtlCaptureManager  destination =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCaptureManager (mkSelector "supportsDestination:") retCULong [argCLong (coerce destination)]

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
startCaptureWithDescriptor_error mtlCaptureManager  descriptor error_ =
withObjCPtr descriptor $ \raw_descriptor ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCaptureManager (mkSelector "startCaptureWithDescriptor:error:") retCULong [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- startCaptureWithDevice:@
startCaptureWithDevice :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithDevice mtlCaptureManager  device =
  sendMsg mtlCaptureManager (mkSelector "startCaptureWithDevice:") retVoid [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | @- startCaptureWithCommandQueue:@
startCaptureWithCommandQueue :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithCommandQueue mtlCaptureManager  commandQueue =
  sendMsg mtlCaptureManager (mkSelector "startCaptureWithCommandQueue:") retVoid [argPtr (castPtr (unRawId commandQueue) :: Ptr ())]

-- | @- startCaptureWithScope:@
startCaptureWithScope :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> RawId -> IO ()
startCaptureWithScope mtlCaptureManager  captureScope =
  sendMsg mtlCaptureManager (mkSelector "startCaptureWithScope:") retVoid [argPtr (castPtr (unRawId captureScope) :: Ptr ())]

-- | @- stopCapture@
stopCapture :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO ()
stopCapture mtlCaptureManager  =
  sendMsg mtlCaptureManager (mkSelector "stopCapture") retVoid []

-- | @- isCapturing@
isCapturing :: IsMTLCaptureManager mtlCaptureManager => mtlCaptureManager -> IO Bool
isCapturing mtlCaptureManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCaptureManager (mkSelector "isCapturing") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCaptureManager@
sharedCaptureManagerSelector :: Selector
sharedCaptureManagerSelector = mkSelector "sharedCaptureManager"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @newCaptureScopeWithDevice:@
newCaptureScopeWithDeviceSelector :: Selector
newCaptureScopeWithDeviceSelector = mkSelector "newCaptureScopeWithDevice:"

-- | @Selector@ for @newCaptureScopeWithCommandQueue:@
newCaptureScopeWithCommandQueueSelector :: Selector
newCaptureScopeWithCommandQueueSelector = mkSelector "newCaptureScopeWithCommandQueue:"

-- | @Selector@ for @newCaptureScopeWithMTL4CommandQueue:@
newCaptureScopeWithMTL4CommandQueueSelector :: Selector
newCaptureScopeWithMTL4CommandQueueSelector = mkSelector "newCaptureScopeWithMTL4CommandQueue:"

-- | @Selector@ for @supportsDestination:@
supportsDestinationSelector :: Selector
supportsDestinationSelector = mkSelector "supportsDestination:"

-- | @Selector@ for @startCaptureWithDescriptor:error:@
startCaptureWithDescriptor_errorSelector :: Selector
startCaptureWithDescriptor_errorSelector = mkSelector "startCaptureWithDescriptor:error:"

-- | @Selector@ for @startCaptureWithDevice:@
startCaptureWithDeviceSelector :: Selector
startCaptureWithDeviceSelector = mkSelector "startCaptureWithDevice:"

-- | @Selector@ for @startCaptureWithCommandQueue:@
startCaptureWithCommandQueueSelector :: Selector
startCaptureWithCommandQueueSelector = mkSelector "startCaptureWithCommandQueue:"

-- | @Selector@ for @startCaptureWithScope:@
startCaptureWithScopeSelector :: Selector
startCaptureWithScopeSelector = mkSelector "startCaptureWithScope:"

-- | @Selector@ for @stopCapture@
stopCaptureSelector :: Selector
stopCaptureSelector = mkSelector "stopCapture"

-- | @Selector@ for @isCapturing@
isCapturingSelector :: Selector
isCapturingSelector = mkSelector "isCapturing"

