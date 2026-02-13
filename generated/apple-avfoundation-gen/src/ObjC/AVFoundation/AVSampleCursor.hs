{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSampleCursor@.
module ObjC.AVFoundation.AVSampleCursor
  ( AVSampleCursor
  , IsAVSampleCursor(..)
  , init_
  , new
  , stepInDecodeOrderByCount
  , stepInPresentationOrderByCount
  , copyCurrentSampleFormatDescription
  , comparePositionInDecodeOrderWithPositionOfCursor
  , samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor
  , samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor
  , currentChunkStorageURL
  , currentSampleIndexInChunk
  , currentSampleDependencyAttachments
  , samplesRequiredForDecoderRefresh
  , comparePositionInDecodeOrderWithPositionOfCursorSelector
  , copyCurrentSampleFormatDescriptionSelector
  , currentChunkStorageURLSelector
  , currentSampleDependencyAttachmentsSelector
  , currentSampleIndexInChunkSelector
  , initSelector
  , newSelector
  , samplesRequiredForDecoderRefreshSelector
  , samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector
  , samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector
  , stepInDecodeOrderByCountSelector
  , stepInPresentationOrderByCountSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id AVSampleCursor)
init_ avSampleCursor =
  sendOwnedMessage avSampleCursor initSelector

-- | @+ new@
new :: IO (Id AVSampleCursor)
new  =
  do
    cls' <- getRequiredClass "AVSampleCursor"
    sendOwnedClassMessage cls' newSelector

-- | stepInDecodeOrderByCount:
--
-- Moves the cursor a given number of samples in decode order.
--
-- @stepCount@ — The number of samples to move across. If positive, step forward this many samples. If negative, step backward (-stepCount) samples.
--
-- Returns: The number of samples the cursor traversed. If the beginning or the end of the sample sequence was reached before the requested number of samples was traversed, the absolute value of the result will be less than the absolute value of stepCount.
--
-- ObjC selector: @- stepInDecodeOrderByCount:@
stepInDecodeOrderByCount :: IsAVSampleCursor avSampleCursor => avSampleCursor -> CLong -> IO CLong
stepInDecodeOrderByCount avSampleCursor stepCount =
  sendMessage avSampleCursor stepInDecodeOrderByCountSelector stepCount

-- | stepInPresentationOrderByCount:
--
-- Moves the cursor a given number of samples in presentation order.
--
-- @stepCount@ — The number of samples to move across. If positive, step forward this many samples. If negative, step backward (-stepCount) samples.
--
-- Returns: The number of samples the cursor traversed. If the beginning or the end of the sample sequence was reached before the requested number of samples was traversed, the absolute value of the result will be less than the absolute value of stepCount.
--
-- ObjC selector: @- stepInPresentationOrderByCount:@
stepInPresentationOrderByCount :: IsAVSampleCursor avSampleCursor => avSampleCursor -> CLong -> IO CLong
stepInPresentationOrderByCount avSampleCursor stepCount =
  sendMessage avSampleCursor stepInPresentationOrderByCountSelector stepCount

-- | copyCurrentSampleFormatDescription:
--
-- Provides the format description of the sample at the receiver's current position.
--
-- ObjC selector: @- copyCurrentSampleFormatDescription@
copyCurrentSampleFormatDescription :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO RawId
copyCurrentSampleFormatDescription avSampleCursor =
  sendOwnedMessage avSampleCursor copyCurrentSampleFormatDescriptionSelector

-- | comparePositionInDecodeOrderWithPositionOfCursor:
--
-- Compares the relative positions of two AVSampleCursors.
--
-- @cursor@ — An instance of AVSampleCursor with which to compare positions.
--
-- Returns: kCFCompareLessThan, kCFCompareEqualTo or kCFCompareGreaterThan, depending on whether the receiver points at a sample before, the same as, or after the sample pointed to by the specified AVSampleCursor.
--
-- If the receiver and cursor reference different sequences of samples, as when they're created by different instances of AVAssetTrack, results are undefined.
--
-- ObjC selector: @- comparePositionInDecodeOrderWithPositionOfCursor:@
comparePositionInDecodeOrderWithPositionOfCursor :: (IsAVSampleCursor avSampleCursor, IsAVSampleCursor cursor) => avSampleCursor -> cursor -> IO NSComparisonResult
comparePositionInDecodeOrderWithPositionOfCursor avSampleCursor cursor =
  sendMessage avSampleCursor comparePositionInDecodeOrderWithPositionOfCursorSelector (toAVSampleCursor cursor)

-- | samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:
--
-- This method tests a boundary in the reordering from decode order to presentation order, determining whether it's possible for any sample earlier in decode order than the sample at the position of the receiver can have a presentation timestamp later than that of the specified sample cursor.
--
-- @cursor@ — An instance of AVSampleCursor with which to test the sample reordering boundary.
--
-- Returns: YES if it's possible for any sample earlier in decode order than the sample at the position of the receiver can have a presentation timestamp later than that of the specified sample cursor.
--
-- If the receiver and cursor reference different sequences of samples, as when they're created by different instances of AVAssetTrack, results are undefined.
--
-- ObjC selector: @- samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:@
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor :: (IsAVSampleCursor avSampleCursor, IsAVSampleCursor cursor) => avSampleCursor -> cursor -> IO Bool
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor avSampleCursor cursor =
  sendMessage avSampleCursor samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector (toAVSampleCursor cursor)

-- | samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:
--
-- This method tests a boundary in the reordering from decode order to presentation order, determining whether it's possible for any sample later in decode order than the sample at the position of the receiver can have a presentation timestamp earlier than that of the specified sample cursor.
--
-- @cursor@ — An instance of AVSampleCursor with which to test the sample reordering boundary.
--
-- Returns: YES if it's possible for any sample later in decode order than the sample at the position of the receiver can have a presentation timestamp earlier than that of the specified sample cursor.
--
-- If the receiver and cursor reference different sequences of samples, as when they're created by different instances of AVAssetTrack, results are undefined.
--
-- ObjC selector: @- samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:@
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor :: (IsAVSampleCursor avSampleCursor, IsAVSampleCursor cursor) => avSampleCursor -> cursor -> IO Bool
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor avSampleCursor cursor =
  sendMessage avSampleCursor samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector (toAVSampleCursor cursor)

-- | currentChunkStorageURL
--
-- The URL of the storage container of the current sample, as well as other samples that are intended to be loaded in the same operation as a "chunk".
--
-- May be nil; if nil, the storage location of the chunk is the URL of the sample cursor's track's asset, if it has one.
--
-- ObjC selector: @- currentChunkStorageURL@
currentChunkStorageURL :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id NSURL)
currentChunkStorageURL avSampleCursor =
  sendMessage avSampleCursor currentChunkStorageURLSelector

-- | currentSampleIndexInChunk
--
-- The index of the current sample within the chunk to which it belongs.
--
-- ObjC selector: @- currentSampleIndexInChunk@
currentSampleIndexInChunk :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO CLong
currentSampleIndexInChunk avSampleCursor =
  sendMessage avSampleCursor currentSampleIndexInChunkSelector

-- | currentSampleDependencyAttachments
--
-- Provides a dictionary containing dependency related sample buffer attachments, if known.  See kCMSampleAttachmentKey_... in CoreMedia/CMSampleBuffer.h.
--
-- ObjC selector: @- currentSampleDependencyAttachments@
currentSampleDependencyAttachments :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id NSDictionary)
currentSampleDependencyAttachments avSampleCursor =
  sendMessage avSampleCursor currentSampleDependencyAttachmentsSelector

-- | samplesRequiredForDecoderRefresh
--
-- Count of samples prior to the current sample, in decode order, that the decoder requires in order to achieve fully coherent output at the current decode time, as after a seek. Zero will be returned if no samples are required for decoder refresh or if the track does not contain this information.
--
-- Some sample sequences that do not indicate sample dependencies may instead indicate that in order for a specific sample to be decoded with all available accuracy, samples prior to that sample in decode order must be decoded before the specific sample is decoded.
--
-- In order to position a sample cursor at the first sample that the decoder requires for a full refresh, you can use code like the following:
--
-- NSInteger samplesPriorToCurrentSampleToFeedToDecoder = [mySampleCursor samplesRequiredForDecoderRefresh];			AVSampleCursor *cursorForObtainingRefreshSamples = [mySampleCursor copy];			[cursorForObtainingRefreshSamples stepInDecodeOrderByCount: -samplesPriorToCurrentSampleToFeedToDecoder ];
--
-- // cursorForObtainingRefreshSamples is now positioned at the first sample that must be provided to the decoder			// in order to decode the sample at the position of mySampleCursor in full
--
-- ObjC selector: @- samplesRequiredForDecoderRefresh@
samplesRequiredForDecoderRefresh :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO CLong
samplesRequiredForDecoderRefresh avSampleCursor =
  sendMessage avSampleCursor samplesRequiredForDecoderRefreshSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSampleCursor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSampleCursor)
newSelector = mkSelector "new"

-- | @Selector@ for @stepInDecodeOrderByCount:@
stepInDecodeOrderByCountSelector :: Selector '[CLong] CLong
stepInDecodeOrderByCountSelector = mkSelector "stepInDecodeOrderByCount:"

-- | @Selector@ for @stepInPresentationOrderByCount:@
stepInPresentationOrderByCountSelector :: Selector '[CLong] CLong
stepInPresentationOrderByCountSelector = mkSelector "stepInPresentationOrderByCount:"

-- | @Selector@ for @copyCurrentSampleFormatDescription@
copyCurrentSampleFormatDescriptionSelector :: Selector '[] RawId
copyCurrentSampleFormatDescriptionSelector = mkSelector "copyCurrentSampleFormatDescription"

-- | @Selector@ for @comparePositionInDecodeOrderWithPositionOfCursor:@
comparePositionInDecodeOrderWithPositionOfCursorSelector :: Selector '[Id AVSampleCursor] NSComparisonResult
comparePositionInDecodeOrderWithPositionOfCursorSelector = mkSelector "comparePositionInDecodeOrderWithPositionOfCursor:"

-- | @Selector@ for @samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:@
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector :: Selector '[Id AVSampleCursor] Bool
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector = mkSelector "samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:"

-- | @Selector@ for @samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:@
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector :: Selector '[Id AVSampleCursor] Bool
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector = mkSelector "samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:"

-- | @Selector@ for @currentChunkStorageURL@
currentChunkStorageURLSelector :: Selector '[] (Id NSURL)
currentChunkStorageURLSelector = mkSelector "currentChunkStorageURL"

-- | @Selector@ for @currentSampleIndexInChunk@
currentSampleIndexInChunkSelector :: Selector '[] CLong
currentSampleIndexInChunkSelector = mkSelector "currentSampleIndexInChunk"

-- | @Selector@ for @currentSampleDependencyAttachments@
currentSampleDependencyAttachmentsSelector :: Selector '[] (Id NSDictionary)
currentSampleDependencyAttachmentsSelector = mkSelector "currentSampleDependencyAttachments"

-- | @Selector@ for @samplesRequiredForDecoderRefresh@
samplesRequiredForDecoderRefreshSelector :: Selector '[] CLong
samplesRequiredForDecoderRefreshSelector = mkSelector "samplesRequiredForDecoderRefresh"

