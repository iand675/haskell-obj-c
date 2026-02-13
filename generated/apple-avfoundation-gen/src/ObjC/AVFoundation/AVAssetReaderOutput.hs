{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReaderOutput
--
-- AVAssetReaderOutput is an abstract class that defines an interface for reading a single collection of samples of a common media type from an AVAssetReader.
--
-- Clients can read the media data of an asset by adding one or more concrete instances of AVAssetReaderOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method.
--
-- IMPORTANT PERFORMANCE NOTE: Make sure to set the alwaysCopiesSampleData property to NO if you do not need to modify the sample data in-place, to avoid unnecessary and inefficient copying.
--
-- Generated bindings for @AVAssetReaderOutput@.
module ObjC.AVFoundation.AVAssetReaderOutput
  ( AVAssetReaderOutput
  , IsAVAssetReaderOutput(..)
  , copyNextSampleBuffer
  , resetForReadingTimeRanges
  , markConfigurationAsFinal
  , mediaType
  , alwaysCopiesSampleData
  , setAlwaysCopiesSampleData
  , supportsRandomAccess
  , setSupportsRandomAccess
  , alwaysCopiesSampleDataSelector
  , copyNextSampleBufferSelector
  , markConfigurationAsFinalSelector
  , mediaTypeSelector
  , resetForReadingTimeRangesSelector
  , setAlwaysCopiesSampleDataSelector
  , setSupportsRandomAccessSelector
  , supportsRandomAccessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | copyNextSampleBuffer
--
-- Copies the next sample buffer for the output synchronously.
--
-- Returns: A CMSampleBuffer object referencing the output sample buffer.
--
-- The client is responsible for calling CFRelease on the returned CMSampleBuffer object when finished with it. This method will return NULL if there are no more sample buffers available for the receiver within the time range specified by its AVAssetReader's timeRange property, or if there is an error that prevents the AVAssetReader from reading more media data. When this method returns NULL, clients should check the value of the associated AVAssetReader's status property to determine why no more samples could be read.
--
-- In certain configurations, such as when outputSettings is nil, copyNextSampleBuffer may return marker-only sample buffers as well as sample buffers containing media data. Marker-only sample buffers can be identified by CMSampleBufferGetNumSamples returning 0. Clients who do not need the information attached to marker-only sample buffers may skip them.
--
-- This method throws an exception if this output is not added to an instance of AVAssetReader (using -addOutput:) and -startReading is not called on that asset reader.
--
-- ObjC selector: @- copyNextSampleBuffer@
copyNextSampleBuffer :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> IO (Ptr ())
copyNextSampleBuffer avAssetReaderOutput =
  sendOwnedMessage avAssetReaderOutput copyNextSampleBufferSelector

-- | resetForReadingTimeRanges:
--
-- Starts reading over with a new set of time ranges.
--
-- @timeRanges@ — An NSArray of NSValue objects, each representing a single CMTimeRange structure
--
-- This method may only be used if supportsRandomAccess has been set to YES and may not be called after -markConfigurationAsFinal has been invoked.
--
-- This method is often used in conjunction with AVAssetWriter multi-pass (see AVAssetWriterInput category AVAssetWriterInputMultiPass).  In this usage, the caller will invoke -copyNextSampleBuffer until that method returns NULL and then ask the AVAssetWriterInput for a set of time ranges from which it thinks media data should be re-encoded.  These time ranges are then given to this method to set up the asset reader output for the next pass.
--
-- The time ranges set here override the time range set on AVAssetReader.timeRange.  Just as with that property, for each time range in the array the intersection of that time range and CMTimeRangeMake(kCMTimeZero, asset.duration) will take effect.
--
-- If this method is invoked after the status of the attached AVAssetReader has become AVAssetReaderStatusFailed or AVAssetReaderStatusCancelled, no change in status will occur and the result of the next call to -copyNextSampleBuffer will be NULL.
--
-- This method throws an exception if the following conditions are not honored:		- each item in time ranges must be an NSValue 		- the start of each time range must be numeric - see CMTIME_IS_NUMERIC		- the duration of each time range must be nonnegative and numeric, or kCMTimePositiveInfinity		- the start of each time range must be greater than or equal to the end of the previous time range		- start times must be strictly increasing		- time ranges must not overlap		- cannot be called before -startReading has been invoked on the attached asset reader		- cannot be called until all samples of media data have been read (i.e. copyNextSampleBuffer returns NULL and the asset reader has not entered a failure state)		- cannot be called without setting "supportsRandomAccess" to YES		- cannot be called after calling -markConfigurationAsFinal
--
-- ObjC selector: @- resetForReadingTimeRanges:@
resetForReadingTimeRanges :: (IsAVAssetReaderOutput avAssetReaderOutput, IsNSArray timeRanges) => avAssetReaderOutput -> timeRanges -> IO ()
resetForReadingTimeRanges avAssetReaderOutput timeRanges =
  sendMessage avAssetReaderOutput resetForReadingTimeRangesSelector (toNSArray timeRanges)

-- | markConfigurationAsFinal
--
-- Informs the receiver that no more reconfiguration of time ranges is necessary and allows the attached AVAssetReader to advance to AVAssetReaderStatusCompleted.
--
-- When the value of supportsRandomAccess is YES, the attached asset reader will not advance to AVAssetReaderStatusCompleted until this method is called.
--
-- When the destination of media data vended by the receiver is an AVAssetWriterInput configured for multi-pass encoding, a convenient time to invoke this method is after the asset writer input indicates that no more passes will be performed.
--
-- Once this method has been called, further invocations of -resetForReadingTimeRanges: are disallowed.
--
-- ObjC selector: @- markConfigurationAsFinal@
markConfigurationAsFinal :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> IO ()
markConfigurationAsFinal avAssetReaderOutput =
  sendMessage avAssetReaderOutput markConfigurationAsFinalSelector

-- | mediaType
--
-- The media type of the samples that can be read from the receiver.
--
-- The value of this property is one of the media type strings defined in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> IO (Id NSString)
mediaType avAssetReaderOutput =
  sendMessage avAssetReaderOutput mediaTypeSelector

-- | alwaysCopiesSampleData
--
-- Indicates whether or not the data in buffers gets copied before being vended to the client.
--
-- When the value of this property is YES, the AVAssetReaderOutput will always vend a buffer with copied data to the client.  Data in such buffers can be freely modified by the client. When the value of this property is NO, the buffers vended to the client may not be copied.  Such buffers may still be referenced by other entities. The result of modifying a buffer whose data hasn't been copied is undefined.  Requesting buffers whose data hasn't been copied when possible can lead to performance improvements.
--
-- The default value is YES.
--
-- This property throws an exception if a value is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown).
--
-- ObjC selector: @- alwaysCopiesSampleData@
alwaysCopiesSampleData :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> IO Bool
alwaysCopiesSampleData avAssetReaderOutput =
  sendMessage avAssetReaderOutput alwaysCopiesSampleDataSelector

-- | alwaysCopiesSampleData
--
-- Indicates whether or not the data in buffers gets copied before being vended to the client.
--
-- When the value of this property is YES, the AVAssetReaderOutput will always vend a buffer with copied data to the client.  Data in such buffers can be freely modified by the client. When the value of this property is NO, the buffers vended to the client may not be copied.  Such buffers may still be referenced by other entities. The result of modifying a buffer whose data hasn't been copied is undefined.  Requesting buffers whose data hasn't been copied when possible can lead to performance improvements.
--
-- The default value is YES.
--
-- This property throws an exception if a value is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown).
--
-- ObjC selector: @- setAlwaysCopiesSampleData:@
setAlwaysCopiesSampleData :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> Bool -> IO ()
setAlwaysCopiesSampleData avAssetReaderOutput value =
  sendMessage avAssetReaderOutput setAlwaysCopiesSampleDataSelector value

-- | supportsRandomAccess
--
-- Indicates whether the asset reader output supports reconfiguration of the time ranges to read.
--
-- When the value of this property is YES, the time ranges read by the asset reader output can be reconfigured during reading using the -resetForReadingTimeRanges: method.  This also prevents the attached AVAssetReader from progressing to AVAssetReaderStatusCompleted until -markConfigurationAsFinal has been invoked.
--
-- The default value is NO, which means that the asset reader output may not be reconfigured once reading has begun.  When the value of this property is NO, AVAssetReader may be able to read media data more efficiently, particularly when multiple asset reader outputs are attached.
--
-- This property throws an exception if a value is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown) or after an AVAssetReaderOutput.Provider is attached.
--
-- ObjC selector: @- supportsRandomAccess@
supportsRandomAccess :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> IO Bool
supportsRandomAccess avAssetReaderOutput =
  sendMessage avAssetReaderOutput supportsRandomAccessSelector

-- | supportsRandomAccess
--
-- Indicates whether the asset reader output supports reconfiguration of the time ranges to read.
--
-- When the value of this property is YES, the time ranges read by the asset reader output can be reconfigured during reading using the -resetForReadingTimeRanges: method.  This also prevents the attached AVAssetReader from progressing to AVAssetReaderStatusCompleted until -markConfigurationAsFinal has been invoked.
--
-- The default value is NO, which means that the asset reader output may not be reconfigured once reading has begun.  When the value of this property is NO, AVAssetReader may be able to read media data more efficiently, particularly when multiple asset reader outputs are attached.
--
-- This property throws an exception if a value is set after reading has started (the asset reader has progressed beyond AVAssetReaderStatusUnknown) or after an AVAssetReaderOutput.Provider is attached.
--
-- ObjC selector: @- setSupportsRandomAccess:@
setSupportsRandomAccess :: IsAVAssetReaderOutput avAssetReaderOutput => avAssetReaderOutput -> Bool -> IO ()
setSupportsRandomAccess avAssetReaderOutput value =
  sendMessage avAssetReaderOutput setSupportsRandomAccessSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @copyNextSampleBuffer@
copyNextSampleBufferSelector :: Selector '[] (Ptr ())
copyNextSampleBufferSelector = mkSelector "copyNextSampleBuffer"

-- | @Selector@ for @resetForReadingTimeRanges:@
resetForReadingTimeRangesSelector :: Selector '[Id NSArray] ()
resetForReadingTimeRangesSelector = mkSelector "resetForReadingTimeRanges:"

-- | @Selector@ for @markConfigurationAsFinal@
markConfigurationAsFinalSelector :: Selector '[] ()
markConfigurationAsFinalSelector = mkSelector "markConfigurationAsFinal"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @alwaysCopiesSampleData@
alwaysCopiesSampleDataSelector :: Selector '[] Bool
alwaysCopiesSampleDataSelector = mkSelector "alwaysCopiesSampleData"

-- | @Selector@ for @setAlwaysCopiesSampleData:@
setAlwaysCopiesSampleDataSelector :: Selector '[Bool] ()
setAlwaysCopiesSampleDataSelector = mkSelector "setAlwaysCopiesSampleData:"

-- | @Selector@ for @supportsRandomAccess@
supportsRandomAccessSelector :: Selector '[] Bool
supportsRandomAccessSelector = mkSelector "supportsRandomAccess"

-- | @Selector@ for @setSupportsRandomAccess:@
setSupportsRandomAccessSelector :: Selector '[Bool] ()
setSupportsRandomAccessSelector = mkSelector "setSupportsRandomAccess:"

