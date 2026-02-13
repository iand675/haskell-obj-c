{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLCaptureDescriptor@.
module ObjC.Metal.MTLCaptureDescriptor
  ( MTLCaptureDescriptor
  , IsMTLCaptureDescriptor(..)
  , captureObject
  , setCaptureObject
  , destination
  , setDestination
  , outputURL
  , setOutputURL
  , captureObjectSelector
  , destinationSelector
  , outputURLSelector
  , setCaptureObjectSelector
  , setDestinationSelector
  , setOutputURLSelector

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

-- | The object that is captured.
--
-- Must be one of the following:
--
-- MTLDevice captures all command queues of the device.
--
-- MTLCommandQueue captures a single command queue.
--
-- MTLCaptureScope captures between the next begin and end of the scope.
--
-- ObjC selector: @- captureObject@
captureObject :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> IO RawId
captureObject mtlCaptureDescriptor =
  sendMessage mtlCaptureDescriptor captureObjectSelector

-- | The object that is captured.
--
-- Must be one of the following:
--
-- MTLDevice captures all command queues of the device.
--
-- MTLCommandQueue captures a single command queue.
--
-- MTLCaptureScope captures between the next begin and end of the scope.
--
-- ObjC selector: @- setCaptureObject:@
setCaptureObject :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> RawId -> IO ()
setCaptureObject mtlCaptureDescriptor value =
  sendMessage mtlCaptureDescriptor setCaptureObjectSelector value

-- | The destination you want the GPU trace to be captured to.
--
-- ObjC selector: @- destination@
destination :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> IO MTLCaptureDestination
destination mtlCaptureDescriptor =
  sendMessage mtlCaptureDescriptor destinationSelector

-- | The destination you want the GPU trace to be captured to.
--
-- ObjC selector: @- setDestination:@
setDestination :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> MTLCaptureDestination -> IO ()
setDestination mtlCaptureDescriptor value =
  sendMessage mtlCaptureDescriptor setDestinationSelector value

-- | URL the GPU Trace document will be captured to. Must be specified when destiation is MTLCaptureDestinationGPUTraceDocument.
--
-- ObjC selector: @- outputURL@
outputURL :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> IO (Id NSURL)
outputURL mtlCaptureDescriptor =
  sendMessage mtlCaptureDescriptor outputURLSelector

-- | URL the GPU Trace document will be captured to. Must be specified when destiation is MTLCaptureDestinationGPUTraceDocument.
--
-- ObjC selector: @- setOutputURL:@
setOutputURL :: (IsMTLCaptureDescriptor mtlCaptureDescriptor, IsNSURL value) => mtlCaptureDescriptor -> value -> IO ()
setOutputURL mtlCaptureDescriptor value =
  sendMessage mtlCaptureDescriptor setOutputURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @captureObject@
captureObjectSelector :: Selector '[] RawId
captureObjectSelector = mkSelector "captureObject"

-- | @Selector@ for @setCaptureObject:@
setCaptureObjectSelector :: Selector '[RawId] ()
setCaptureObjectSelector = mkSelector "setCaptureObject:"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] MTLCaptureDestination
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[MTLCaptureDestination] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector '[] (Id NSURL)
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @setOutputURL:@
setOutputURLSelector :: Selector '[Id NSURL] ()
setOutputURLSelector = mkSelector "setOutputURL:"

