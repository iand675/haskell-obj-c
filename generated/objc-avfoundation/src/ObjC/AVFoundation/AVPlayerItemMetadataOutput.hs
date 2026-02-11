{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemMetadataOutput
--
-- A subclass of AVPlayerItemOutput that vends collections of metadata items carried in metadata tracks.
--
-- Setting the value of suppressesPlayerRendering on an instance of AVPlayerItemMetadataOutput has no effect.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemMetadataOutput@.
module ObjC.AVFoundation.AVPlayerItemMetadataOutput
  ( AVPlayerItemMetadataOutput
  , IsAVPlayerItemMetadataOutput(..)
  , initWithIdentifiers
  , setDelegate_queue
  , delegateQueue
  , advanceIntervalForDelegateInvocation
  , setAdvanceIntervalForDelegateInvocation
  , initWithIdentifiersSelector
  , setDelegate_queueSelector
  , delegateQueueSelector
  , advanceIntervalForDelegateInvocationSelector
  , setAdvanceIntervalForDelegateInvocationSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithIdentifiers:
--
-- Creates an instance of AVPlayerItemMetadataOutput.
--
-- @identifiers@ — A array of metadata identifiers indicating the metadata items that the output should provide.
--
-- See AVMetadataIdentifiers.h for publicly defined metadata identifiers. Pass nil to receive all of the timed metadata from all enabled AVPlayerItemTracks that carry timed metadata.
--
-- ObjC selector: @- initWithIdentifiers:@
initWithIdentifiers :: (IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput, IsNSArray identifiers) => avPlayerItemMetadataOutput -> identifiers -> IO (Id AVPlayerItemMetadataOutput)
initWithIdentifiers avPlayerItemMetadataOutput  identifiers =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg avPlayerItemMetadataOutput (mkSelector "initWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ())] >>= ownedObject . castPtr

-- | setDelegate:queue:
--
-- Sets the receiver's delegate and a dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to AVPlayerItemMetadataOutputPushDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput, IsNSObject delegateQueue) => avPlayerItemMetadataOutput -> RawId -> delegateQueue -> IO ()
setDelegate_queue avPlayerItemMetadataOutput  delegate delegateQueue =
withObjCPtr delegateQueue $ \raw_delegateQueue ->
    sendMsg avPlayerItemMetadataOutput (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

-- | delegateQueue
--
-- The dispatch queue on which messages are sent to the delegate.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> IO (Id NSObject)
delegateQueue avPlayerItemMetadataOutput  =
  sendMsg avPlayerItemMetadataOutput (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemMetadataOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemMetadataOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocation :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> IO CDouble
advanceIntervalForDelegateInvocation avPlayerItemMetadataOutput  =
  sendMsg avPlayerItemMetadataOutput (mkSelector "advanceIntervalForDelegateInvocation") retCDouble []

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemMetadataOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemMetadataOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocation :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> CDouble -> IO ()
setAdvanceIntervalForDelegateInvocation avPlayerItemMetadataOutput  value =
  sendMsg avPlayerItemMetadataOutput (mkSelector "setAdvanceIntervalForDelegateInvocation:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:@
initWithIdentifiersSelector :: Selector
initWithIdentifiersSelector = mkSelector "initWithIdentifiers:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocationSelector :: Selector
advanceIntervalForDelegateInvocationSelector = mkSelector "advanceIntervalForDelegateInvocation"

-- | @Selector@ for @setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocationSelector :: Selector
setAdvanceIntervalForDelegateInvocationSelector = mkSelector "setAdvanceIntervalForDelegateInvocation:"

