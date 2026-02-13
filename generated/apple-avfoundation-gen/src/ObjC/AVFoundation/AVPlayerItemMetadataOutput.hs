{-# LANGUAGE DataKinds #-}
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
  , delegate
  , delegateQueue
  , advanceIntervalForDelegateInvocation
  , setAdvanceIntervalForDelegateInvocation
  , advanceIntervalForDelegateInvocationSelector
  , delegateQueueSelector
  , delegateSelector
  , initWithIdentifiersSelector
  , setAdvanceIntervalForDelegateInvocationSelector
  , setDelegate_queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithIdentifiers avPlayerItemMetadataOutput identifiers =
  sendOwnedMessage avPlayerItemMetadataOutput initWithIdentifiersSelector (toNSArray identifiers)

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
setDelegate_queue avPlayerItemMetadataOutput delegate delegateQueue =
  sendMessage avPlayerItemMetadataOutput setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | delegate
--
-- The receiver's delegate.
--
-- The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.  This property is not key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> IO RawId
delegate avPlayerItemMetadataOutput =
  sendMessage avPlayerItemMetadataOutput delegateSelector

-- | delegateQueue
--
-- The dispatch queue on which messages are sent to the delegate.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> IO (Id NSObject)
delegateQueue avPlayerItemMetadataOutput =
  sendMessage avPlayerItemMetadataOutput delegateQueueSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemMetadataOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemMetadataOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocation :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> IO CDouble
advanceIntervalForDelegateInvocation avPlayerItemMetadataOutput =
  sendMessage avPlayerItemMetadataOutput advanceIntervalForDelegateInvocationSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemMetadataOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemMetadataOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocation :: IsAVPlayerItemMetadataOutput avPlayerItemMetadataOutput => avPlayerItemMetadataOutput -> CDouble -> IO ()
setAdvanceIntervalForDelegateInvocation avPlayerItemMetadataOutput value =
  sendMessage avPlayerItemMetadataOutput setAdvanceIntervalForDelegateInvocationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:@
initWithIdentifiersSelector :: Selector '[Id NSArray] (Id AVPlayerItemMetadataOutput)
initWithIdentifiersSelector = mkSelector "initWithIdentifiers:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSObject)
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocationSelector :: Selector '[] CDouble
advanceIntervalForDelegateInvocationSelector = mkSelector "advanceIntervalForDelegateInvocation"

-- | @Selector@ for @setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocationSelector :: Selector '[CDouble] ()
setAdvanceIntervalForDelegateInvocationSelector = mkSelector "setAdvanceIntervalForDelegateInvocation:"

