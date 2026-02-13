{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemMetadataCollector
--
-- A subclass of AVPlayerItemMediaDataCollector that provides AVMetadataGroups for an AVPlayerItem.
--
-- This class can be used to inform clients of the current set of AVMetadataGroups on an AVPlayerItem, and when new AVMetadataGroups become available - e.g. in a Live HLS stream.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemMetadataCollector@.
module ObjC.AVFoundation.AVPlayerItemMetadataCollector
  ( AVPlayerItemMetadataCollector
  , IsAVPlayerItemMetadataCollector(..)
  , initWithIdentifiers_classifyingLabels
  , setDelegate_queue
  , delegate
  , delegateQueue
  , delegateQueueSelector
  , delegateSelector
  , initWithIdentifiers_classifyingLabelsSelector
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

-- | initWithIdentifiers:classifyingLabels:
--
-- Returns an instance of AVPlayerItemMetadataCollector that can provide all available AVMetadataGroups matching a set of criteria.
--
-- @identifiers@ — A array of metadata identifiers indicating the metadata items that the output should provide. See AVMetadataIdentifiers.h for publicly defined metadata identifiers. Pass nil to include metadata with any identifier.
--
-- @classifyingLabels@ — If the metadata format supports labeling each metadata group with a string, supplying an array of group labels indicates that the output should provide metadata groups that match one of the supplied labels. Pass nil to include metadata with any (or no) classifying label.
--
-- Returns: An instance of AVPlayerItemMetadataCollector.
--
-- Some metadata available in some formats - such as timed metadata embedded in HLS segments - is not available for collector output.		The default init method can be used as an alternative to setting both identifiers and classifyingLabels to nil.
--
-- ObjC selector: @- initWithIdentifiers:classifyingLabels:@
initWithIdentifiers_classifyingLabels :: (IsAVPlayerItemMetadataCollector avPlayerItemMetadataCollector, IsNSArray identifiers, IsNSArray classifyingLabels) => avPlayerItemMetadataCollector -> identifiers -> classifyingLabels -> IO (Id AVPlayerItemMetadataCollector)
initWithIdentifiers_classifyingLabels avPlayerItemMetadataCollector identifiers classifyingLabels =
  sendOwnedMessage avPlayerItemMetadataCollector initWithIdentifiers_classifyingLabelsSelector (toNSArray identifiers) (toNSArray classifyingLabels)

-- | setDelegate:queue:
--
-- Sets the receiver's delegate and a dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to AVPlayerItemMetadataCollectorPushDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVPlayerItemMetadataCollector avPlayerItemMetadataCollector, IsNSObject delegateQueue) => avPlayerItemMetadataCollector -> RawId -> delegateQueue -> IO ()
setDelegate_queue avPlayerItemMetadataCollector delegate delegateQueue =
  sendMessage avPlayerItemMetadataCollector setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | delegate
--
-- The receiver's delegate.
--
-- The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.  This property is not key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemMetadataCollector avPlayerItemMetadataCollector => avPlayerItemMetadataCollector -> IO RawId
delegate avPlayerItemMetadataCollector =
  sendMessage avPlayerItemMetadataCollector delegateSelector

-- | delegateQueue
--
-- The dispatch queue on which messages are sent to the delegate.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemMetadataCollector avPlayerItemMetadataCollector => avPlayerItemMetadataCollector -> IO (Id NSObject)
delegateQueue avPlayerItemMetadataCollector =
  sendMessage avPlayerItemMetadataCollector delegateQueueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:classifyingLabels:@
initWithIdentifiers_classifyingLabelsSelector :: Selector '[Id NSArray, Id NSArray] (Id AVPlayerItemMetadataCollector)
initWithIdentifiers_classifyingLabelsSelector = mkSelector "initWithIdentifiers:classifyingLabels:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSObject)
delegateQueueSelector = mkSelector "delegateQueue"

