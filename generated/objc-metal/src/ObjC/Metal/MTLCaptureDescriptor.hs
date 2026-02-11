{-# LANGUAGE PatternSynonyms #-}
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
  , setCaptureObjectSelector
  , destinationSelector
  , setDestinationSelector
  , outputURLSelector
  , setOutputURLSelector

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
captureObject mtlCaptureDescriptor  =
  fmap (RawId . castPtr) $ sendMsg mtlCaptureDescriptor (mkSelector "captureObject") (retPtr retVoid) []

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
setCaptureObject mtlCaptureDescriptor  value =
  sendMsg mtlCaptureDescriptor (mkSelector "setCaptureObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The destination you want the GPU trace to be captured to.
--
-- ObjC selector: @- destination@
destination :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> IO MTLCaptureDestination
destination mtlCaptureDescriptor  =
  fmap (coerce :: CLong -> MTLCaptureDestination) $ sendMsg mtlCaptureDescriptor (mkSelector "destination") retCLong []

-- | The destination you want the GPU trace to be captured to.
--
-- ObjC selector: @- setDestination:@
setDestination :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> MTLCaptureDestination -> IO ()
setDestination mtlCaptureDescriptor  value =
  sendMsg mtlCaptureDescriptor (mkSelector "setDestination:") retVoid [argCLong (coerce value)]

-- | URL the GPU Trace document will be captured to. Must be specified when destiation is MTLCaptureDestinationGPUTraceDocument.
--
-- ObjC selector: @- outputURL@
outputURL :: IsMTLCaptureDescriptor mtlCaptureDescriptor => mtlCaptureDescriptor -> IO (Id NSURL)
outputURL mtlCaptureDescriptor  =
  sendMsg mtlCaptureDescriptor (mkSelector "outputURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL the GPU Trace document will be captured to. Must be specified when destiation is MTLCaptureDestinationGPUTraceDocument.
--
-- ObjC selector: @- setOutputURL:@
setOutputURL :: (IsMTLCaptureDescriptor mtlCaptureDescriptor, IsNSURL value) => mtlCaptureDescriptor -> value -> IO ()
setOutputURL mtlCaptureDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlCaptureDescriptor (mkSelector "setOutputURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @captureObject@
captureObjectSelector :: Selector
captureObjectSelector = mkSelector "captureObject"

-- | @Selector@ for @setCaptureObject:@
setCaptureObjectSelector :: Selector
setCaptureObjectSelector = mkSelector "setCaptureObject:"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @outputURL@
outputURLSelector :: Selector
outputURLSelector = mkSelector "outputURL"

-- | @Selector@ for @setOutputURL:@
setOutputURLSelector :: Selector
setOutputURLSelector = mkSelector "setOutputURL:"

