{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , stepInDecodeOrderByCountSelector
  , stepInPresentationOrderByCountSelector
  , copyCurrentSampleFormatDescriptionSelector
  , comparePositionInDecodeOrderWithPositionOfCursorSelector
  , samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector
  , samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector
  , currentChunkStorageURLSelector
  , currentSampleIndexInChunkSelector
  , currentSampleDependencyAttachmentsSelector
  , samplesRequiredForDecoderRefreshSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id AVSampleCursor)
init_ avSampleCursor  =
  sendMsg avSampleCursor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSampleCursor)
new  =
  do
    cls' <- getRequiredClass "AVSampleCursor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
stepInDecodeOrderByCount avSampleCursor  stepCount =
  sendMsg avSampleCursor (mkSelector "stepInDecodeOrderByCount:") retCLong [argCLong (fromIntegral stepCount)]

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
stepInPresentationOrderByCount avSampleCursor  stepCount =
  sendMsg avSampleCursor (mkSelector "stepInPresentationOrderByCount:") retCLong [argCLong (fromIntegral stepCount)]

-- | copyCurrentSampleFormatDescription:
--
-- Provides the format description of the sample at the receiver's current position.
--
-- ObjC selector: @- copyCurrentSampleFormatDescription@
copyCurrentSampleFormatDescription :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO RawId
copyCurrentSampleFormatDescription avSampleCursor  =
  fmap (RawId . castPtr) $ sendMsg avSampleCursor (mkSelector "copyCurrentSampleFormatDescription") (retPtr retVoid) []

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
comparePositionInDecodeOrderWithPositionOfCursor avSampleCursor  cursor =
withObjCPtr cursor $ \raw_cursor ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg avSampleCursor (mkSelector "comparePositionInDecodeOrderWithPositionOfCursor:") retCLong [argPtr (castPtr raw_cursor :: Ptr ())]

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
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor avSampleCursor  cursor =
withObjCPtr cursor $ \raw_cursor ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleCursor (mkSelector "samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:") retCULong [argPtr (castPtr raw_cursor :: Ptr ())]

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
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor avSampleCursor  cursor =
withObjCPtr cursor $ \raw_cursor ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleCursor (mkSelector "samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:") retCULong [argPtr (castPtr raw_cursor :: Ptr ())]

-- | currentChunkStorageURL
--
-- The URL of the storage container of the current sample, as well as other samples that are intended to be loaded in the same operation as a "chunk".
--
-- May be nil; if nil, the storage location of the chunk is the URL of the sample cursor's track's asset, if it has one.
--
-- ObjC selector: @- currentChunkStorageURL@
currentChunkStorageURL :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id NSURL)
currentChunkStorageURL avSampleCursor  =
  sendMsg avSampleCursor (mkSelector "currentChunkStorageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | currentSampleIndexInChunk
--
-- The index of the current sample within the chunk to which it belongs.
--
-- ObjC selector: @- currentSampleIndexInChunk@
currentSampleIndexInChunk :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO CLong
currentSampleIndexInChunk avSampleCursor  =
  sendMsg avSampleCursor (mkSelector "currentSampleIndexInChunk") retCLong []

-- | currentSampleDependencyAttachments
--
-- Provides a dictionary containing dependency related sample buffer attachments, if known.  See kCMSampleAttachmentKey_... in CoreMedia/CMSampleBuffer.h.
--
-- ObjC selector: @- currentSampleDependencyAttachments@
currentSampleDependencyAttachments :: IsAVSampleCursor avSampleCursor => avSampleCursor -> IO (Id NSDictionary)
currentSampleDependencyAttachments avSampleCursor  =
  sendMsg avSampleCursor (mkSelector "currentSampleDependencyAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

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
samplesRequiredForDecoderRefresh avSampleCursor  =
  sendMsg avSampleCursor (mkSelector "samplesRequiredForDecoderRefresh") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @stepInDecodeOrderByCount:@
stepInDecodeOrderByCountSelector :: Selector
stepInDecodeOrderByCountSelector = mkSelector "stepInDecodeOrderByCount:"

-- | @Selector@ for @stepInPresentationOrderByCount:@
stepInPresentationOrderByCountSelector :: Selector
stepInPresentationOrderByCountSelector = mkSelector "stepInPresentationOrderByCount:"

-- | @Selector@ for @copyCurrentSampleFormatDescription@
copyCurrentSampleFormatDescriptionSelector :: Selector
copyCurrentSampleFormatDescriptionSelector = mkSelector "copyCurrentSampleFormatDescription"

-- | @Selector@ for @comparePositionInDecodeOrderWithPositionOfCursor:@
comparePositionInDecodeOrderWithPositionOfCursorSelector :: Selector
comparePositionInDecodeOrderWithPositionOfCursorSelector = mkSelector "comparePositionInDecodeOrderWithPositionOfCursor:"

-- | @Selector@ for @samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:@
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector :: Selector
samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursorSelector = mkSelector "samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:"

-- | @Selector@ for @samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:@
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector :: Selector
samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursorSelector = mkSelector "samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:"

-- | @Selector@ for @currentChunkStorageURL@
currentChunkStorageURLSelector :: Selector
currentChunkStorageURLSelector = mkSelector "currentChunkStorageURL"

-- | @Selector@ for @currentSampleIndexInChunk@
currentSampleIndexInChunkSelector :: Selector
currentSampleIndexInChunkSelector = mkSelector "currentSampleIndexInChunk"

-- | @Selector@ for @currentSampleDependencyAttachments@
currentSampleDependencyAttachmentsSelector :: Selector
currentSampleDependencyAttachmentsSelector = mkSelector "currentSampleDependencyAttachments"

-- | @Selector@ for @samplesRequiredForDecoderRefresh@
samplesRequiredForDecoderRefreshSelector :: Selector
samplesRequiredForDecoderRefreshSelector = mkSelector "samplesRequiredForDecoderRefresh"

