{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVTimedMetadataGroup
--
-- AVTimedMetadataGroup is used to represent a collection of metadata items that are valid for use during a specific range of time. For example, AVTimedMetadataGroups are used to represent chapters, optionally containing metadata items for chapter titles and chapter images.
--
-- Generated bindings for @AVTimedMetadataGroup@.
module ObjC.AVFoundation.AVTimedMetadataGroup
  ( AVTimedMetadataGroup
  , IsAVTimedMetadataGroup(..)
  , initWithSampleBuffer
  , copyFormatDescription
  , items
  , copyFormatDescriptionSelector
  , initWithSampleBufferSelector
  , itemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSampleBuffer:
--
-- Initializes an instance of AVTimedMetadataGroup with a sample buffer.
--
-- @sampleBuffer@ — A CMSampleBuffer with media type kCMMediaType_Metadata.
--
-- Returns: An instance of AVTimedMetadataGroup.
--
-- ObjC selector: @- initWithSampleBuffer:@
initWithSampleBuffer :: IsAVTimedMetadataGroup avTimedMetadataGroup => avTimedMetadataGroup -> Ptr () -> IO (Id AVTimedMetadataGroup)
initWithSampleBuffer avTimedMetadataGroup sampleBuffer =
  sendOwnedMessage avTimedMetadataGroup initWithSampleBufferSelector sampleBuffer

-- | copyFormatDescription
--
-- Creates a format description based on the receiver's items.
--
-- Returns: An instance of CMMetadataFormatDescription sufficient to describe the contents of all the items referenced by the receiver.
--
-- The returned format description is suitable for use as the format hint parameter when creating an instance of AVAssetWriterInput.
--
-- Each item referenced by the receiver must carry a non-nil value for its dataType property.  An exception will be thrown if any item does not have a data type.
--
-- ObjC selector: @- copyFormatDescription@
copyFormatDescription :: IsAVTimedMetadataGroup avTimedMetadataGroup => avTimedMetadataGroup -> IO RawId
copyFormatDescription avTimedMetadataGroup =
  sendOwnedMessage avTimedMetadataGroup copyFormatDescriptionSelector

-- | @- items@
items :: IsAVTimedMetadataGroup avTimedMetadataGroup => avTimedMetadataGroup -> IO (Id NSArray)
items avTimedMetadataGroup =
  sendMessage avTimedMetadataGroup itemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSampleBuffer:@
initWithSampleBufferSelector :: Selector '[Ptr ()] (Id AVTimedMetadataGroup)
initWithSampleBufferSelector = mkSelector "initWithSampleBuffer:"

-- | @Selector@ for @copyFormatDescription@
copyFormatDescriptionSelector :: Selector '[] RawId
copyFormatDescriptionSelector = mkSelector "copyFormatDescription"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

