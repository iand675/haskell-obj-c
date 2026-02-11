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
  , delegateQueue
  , initWithIdentifiers_classifyingLabelsSelector
  , setDelegate_queueSelector
  , delegateQueueSelector


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
initWithIdentifiers_classifyingLabels avPlayerItemMetadataCollector  identifiers classifyingLabels =
withObjCPtr identifiers $ \raw_identifiers ->
  withObjCPtr classifyingLabels $ \raw_classifyingLabels ->
      sendMsg avPlayerItemMetadataCollector (mkSelector "initWithIdentifiers:classifyingLabels:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_classifyingLabels :: Ptr ())] >>= ownedObject . castPtr

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
setDelegate_queue avPlayerItemMetadataCollector  delegate delegateQueue =
withObjCPtr delegateQueue $ \raw_delegateQueue ->
    sendMsg avPlayerItemMetadataCollector (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

-- | delegateQueue
--
-- The dispatch queue on which messages are sent to the delegate.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemMetadataCollector avPlayerItemMetadataCollector => avPlayerItemMetadataCollector -> IO (Id NSObject)
delegateQueue avPlayerItemMetadataCollector  =
  sendMsg avPlayerItemMetadataCollector (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifiers:classifyingLabels:@
initWithIdentifiers_classifyingLabelsSelector :: Selector
initWithIdentifiers_classifyingLabelsSelector = mkSelector "initWithIdentifiers:classifyingLabels:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

