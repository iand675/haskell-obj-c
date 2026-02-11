{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRTrack
--
-- The DRTrack class represents a track on the burned disc.
--
-- About tracks
--
-- A DRTrack provides data to the for the burn and contains a description of the 	track on disc (length, block type, data format, etc). 	Data is provided for the burn in a real-time thread. It is up to the track to 	provide this data in a timely manner, otherwise a burn underrun can occur and	ruin a disc.
--
-- Data Production
--
-- DRTracks do not typically store or cache the data to be written to disk, instead the 	data is streamed to the disc from some data producer as it's needed. This is 	accomplished through an object associated with the track when the track is created 	called the track producer. A track producer is a class you create that implements 	the
--
-- DRTrackDataProduction DRTrackDataProduction
--
-- informal protocol. This protocol defines all of 	the methods that a track object will call during a burn to obtain data.
--
-- Track Properties
--
-- A DRTrack object contains several properties which define the track for the burn.	These properties are stored in an NSDictionary and are accessed through the
--
-- //apple_ref/occ/instm/DRTrack/properties properties
--
-- and
--
-- //apple_ref/occ/instm/DRTrack/setProperties: setProperties:
--
-- methods.
--
-- There are several properties that are required to be present and if they are not, will 	cause the burn to fail. These are:
--
-- DRTrackLengthKey DRTrackLengthKey
--
-- Length of the track
--
-- DRBlockSizeKey DRBlockSizeKey
--
-- Size in bytes of each track block
--
-- DRBlockTypeKey DRBlockTypeKey
--
-- Type of each track block
--
-- DRDataFormKey DRDataFormKey
--
-- Data form of each block in the track
--
-- DRSessionFormatKey DRSessionFormatKey
--
-- Session format of the track
--
-- DRTrackModeKey DRTrackModeKey
--
-- Track mode of the track
--
-- The possible values of these properties are defined in the Mt. Fuji (IFF-8090i) 	specification for CD/DVD devices. It's up to you to understand the possible values	and meanings of each.
--
-- All other keys contained in the properties dictionary are optional and can be omitted.
--
-- Generated bindings for @DRTrack@.
module ObjC.DiscRecording.DRTrack
  ( DRTrack
  , IsDRTrack(..)
  , initWithProducer
  , properties
  , setProperties
  , testProductionSpeedForInterval
  , testProductionSpeedForLength
  , estimateLength
  , trackForRootFolder
  , trackForAudioOfLength_producer
  , trackForAudioFile
  , length_
  , preGap
  , setPreGap
  , initWithProducerSelector
  , propertiesSelector
  , setPropertiesSelector
  , testProductionSpeedForIntervalSelector
  , testProductionSpeedForLengthSelector
  , estimateLengthSelector
  , trackForRootFolderSelector
  , trackForAudioOfLength_producerSelector
  , trackForAudioFileSelector
  , lengthSelector
  , preGapSelector
  , setPreGapSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithProducer:
--
-- Initializes a DRTrack with the producer
--
-- @producer@ — The object to use as the data producer
--
-- Returns: A DRTrack
--
-- ObjC selector: @- initWithProducer:@
initWithProducer :: IsDRTrack drTrack => drTrack -> RawId -> IO RawId
initWithProducer drTrack  producer =
  fmap (RawId . castPtr) $ sendMsg drTrack (mkSelector "initWithProducer:") (retPtr retVoid) [argPtr (castPtr (unRawId producer) :: Ptr ())]

-- | properties
--
-- Returns the properties dictionary of the track.
--
-- Returns: An NSDictionary containing the properties of the track.
--
-- ObjC selector: @- properties@
properties :: IsDRTrack drTrack => drTrack -> IO (Id NSDictionary)
properties drTrack  =
  sendMsg drTrack (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setProperties:
--
-- Sets the properties dictionary of the track
--
-- @properties@ — NSDictionary of the properties to set.
--
-- ObjC selector: @- setProperties:@
setProperties :: (IsDRTrack drTrack, IsNSDictionary properties) => drTrack -> properties -> IO ()
setProperties drTrack  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg drTrack (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_properties :: Ptr ())]

-- | testProductionSpeedForInterval:
--
-- Tests the production speed for a specified interval.
--
-- Runs a fake "production" cycle, repeatedly asking the receiver for data by calling					it's producer's
--
-- //apple_ref/occ/intfm/DRTrackDataProduction/produceDataIntoBuffer:length:atAddress:blockSize:ioFlags: produceDataIntoBuffer:length:atAddress:blockSize:ioFlags:
--
-- for the 					specified time interval.
--
-- Use this function to verify that the the production code can produce data fast 					enough to satisfy the data throughput requirements of the burn.
--
-- Returns the calculated maximum speed the at which the receiver can produce data. 					This value should be used when setting up a burn to limit the burn speed
--
-- @interval@ — The length of the test in seconds.
--
-- Returns: The maximum speed data can be produced at.
--
-- ObjC selector: @- testProductionSpeedForInterval:@
testProductionSpeedForInterval :: IsDRTrack drTrack => drTrack -> CDouble -> IO CFloat
testProductionSpeedForInterval drTrack  interval =
  sendMsg drTrack (mkSelector "testProductionSpeedForInterval:") retCFloat [argCDouble (fromIntegral interval)]

-- | testProductionSpeedForLength:
--
-- Tests the production speed for a specified byte count.
--
-- Runs a fake "production" cycle, repeatedly asking the receiver for data by calling					it's producer's
--
-- //apple_ref/occ/intfm/DRTrackDataProduction/produceDataIntoBuffer:length:atAddress:blockSize:ioFlags: produceDataIntoBuffer:length:atAddress:blockSize:ioFlags:
--
-- until 					the specified length number of bytes have been produced.
--
-- Use this function to verify that the the production code can produce data fast 					enough to satisfy the data throughput requirements of the burn.
--
-- Returns the calculated maximum speed the at which the receiver can produce data. 					This value should be used when setting up a burn to limit the burn speed
--
-- @length@ — The length of the test in bytes.
--
-- Returns: The maximum speed data can be produced at.
--
-- ObjC selector: @- testProductionSpeedForLength:@
testProductionSpeedForLength :: IsDRTrack drTrack => drTrack -> CUInt -> IO CFloat
testProductionSpeedForLength drTrack  length_ =
  sendMsg drTrack (mkSelector "testProductionSpeedForLength:") retCFloat [argCUInt (fromIntegral length_)]

-- | estimateLength
--
-- Asks the track producer for a size estimate.
--
-- This method calls the track producer to ask it to estimate the size				needed for its data.
--
-- For some types of track, this call may be very expensive. For example,				a DRFilesystemTrack may need to iterate folders on disk to provide an				accurate estimate, which (if a large number of files and folders are				involved) can cause this call to take 30 seconds or more. Since your				main thread should not be allowed to block for this long, you may wish				to call this function on a separate thread.
--
-- Returns: The estimated length of the track.
--
-- ObjC selector: @- estimateLength@
estimateLength :: IsDRTrack drTrack => drTrack -> IO CULong
estimateLength drTrack  =
  sendMsg drTrack (mkSelector "estimateLength") retCULong []

-- | trackForRootFolder:
--
-- Creates a DRTrack capable of burning a folder to disc.
--
-- Additional track properties can be set controlling the various 					filesystems to be generated. See the documentation for
--
-- //apple_ref/occ/cl/DRTrack DRTrack
--
-- for more info.
--
-- @rootFolder@ — The root of the volume to be created.
--
-- Returns: An autoreleased DRTrack
--
-- ObjC selector: @+ trackForRootFolder:@
trackForRootFolder :: IsDRFolder rootFolder => rootFolder -> IO (Id DRTrack)
trackForRootFolder rootFolder =
  do
    cls' <- getRequiredClass "DRTrack"
    withObjCPtr rootFolder $ \raw_rootFolder ->
      sendClassMsg cls' (mkSelector "trackForRootFolder:") (retPtr retVoid) [argPtr (castPtr raw_rootFolder :: Ptr ())] >>= retainedObject . castPtr

-- | trackForAudioOfLength:producer:
--
-- Creates a DRTrack capable of burning RedBook CD audio.
--
-- This method configures a track object configured to accept standard RedBook audio					CD data. It is up to the client to provide that data to the 					track object through the producer object. The producer is an object					which implements the
--
-- //apple_ref/occ/intf/DRTrackDataProduction DRTrackDataProduction
--
-- protocol.
--
-- @length@ — The length of the track that will be produced.
--
-- @producer@ — The object to use as the data producer
--
-- Returns: An autoreleased DRTrack
--
-- ObjC selector: @+ trackForAudioOfLength:producer:@
trackForAudioOfLength_producer :: IsDRMSF length_ => length_ -> RawId -> IO (Id DRTrack)
trackForAudioOfLength_producer length_ producer =
  do
    cls' <- getRequiredClass "DRTrack"
    withObjCPtr length_ $ \raw_length_ ->
      sendClassMsg cls' (mkSelector "trackForAudioOfLength:producer:") (retPtr retVoid) [argPtr (castPtr raw_length_ :: Ptr ()), argPtr (castPtr (unRawId producer) :: Ptr ())] >>= retainedObject . castPtr

-- | trackForAudioFile:
--
-- Creates a DRTrack capable of burning RedBook CD audio from a QuickTime readable file.
--
-- This method creates a track object configured and primed to output RedBook audio					CD data. It accepts any file readable by QuickTime and extracts the audio data					(if any) from the file, translating that into the correct format for output					to the disc.
--
-- @path@ — The path to the file. This file must be one that can be read by							QuickTime.
--
-- Returns: An autoreleased DRTrack
--
-- ObjC selector: @+ trackForAudioFile:@
trackForAudioFile :: IsNSString path => path -> IO (Id DRTrack)
trackForAudioFile path =
  do
    cls' <- getRequiredClass "DRTrack"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "trackForAudioFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | length
--
-- Returns the length of the track data.
--
-- The length returned does not include the length of the pregap. Only the length   					of the track data itself is returned.
--
-- Returns: A DRMSF representing the length of the track.
--
-- ObjC selector: @- length@
length_ :: IsDRTrack drTrack => drTrack -> IO (Id DRMSF)
length_ drTrack  =
  sendMsg drTrack (mkSelector "length") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preGap
--
-- Returns the length of the pre gap.
--
-- This is a simple wrapper to obtain the
--
-- DRPreGapLengthKey DRPreGapLengthKey
--
-- . If the
--
-- DRPreGapLengthKey DRPreGapLengthKey
--
-- property has not been set for the track this method will return a zero-length
--
-- //apple_ref/occ/cl/DRMSF DRMSF
--
-- object (0m:0s:0f).
--
-- Returns: A DRMSF representing the length of the pre gap.
--
-- ObjC selector: @- preGap@
preGap :: IsDRTrack drTrack => drTrack -> IO (Id DRMSF)
preGap drTrack  =
  sendMsg drTrack (mkSelector "preGap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setPreGap:
--
-- Sets the length of the pre gap.
--
-- This is a simple wrapper to set the
--
-- DRPreGapLengthKey DRPreGapLengthKey
--
-- .
--
-- @preGap@ — the pre gap length.
--
-- ObjC selector: @- setPreGap:@
setPreGap :: (IsDRTrack drTrack, IsDRMSF preGap) => drTrack -> preGap -> IO ()
setPreGap drTrack  preGap =
withObjCPtr preGap $ \raw_preGap ->
    sendMsg drTrack (mkSelector "setPreGap:") retVoid [argPtr (castPtr raw_preGap :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProducer:@
initWithProducerSelector :: Selector
initWithProducerSelector = mkSelector "initWithProducer:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @testProductionSpeedForInterval:@
testProductionSpeedForIntervalSelector :: Selector
testProductionSpeedForIntervalSelector = mkSelector "testProductionSpeedForInterval:"

-- | @Selector@ for @testProductionSpeedForLength:@
testProductionSpeedForLengthSelector :: Selector
testProductionSpeedForLengthSelector = mkSelector "testProductionSpeedForLength:"

-- | @Selector@ for @estimateLength@
estimateLengthSelector :: Selector
estimateLengthSelector = mkSelector "estimateLength"

-- | @Selector@ for @trackForRootFolder:@
trackForRootFolderSelector :: Selector
trackForRootFolderSelector = mkSelector "trackForRootFolder:"

-- | @Selector@ for @trackForAudioOfLength:producer:@
trackForAudioOfLength_producerSelector :: Selector
trackForAudioOfLength_producerSelector = mkSelector "trackForAudioOfLength:producer:"

-- | @Selector@ for @trackForAudioFile:@
trackForAudioFileSelector :: Selector
trackForAudioFileSelector = mkSelector "trackForAudioFile:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @preGap@
preGapSelector :: Selector
preGapSelector = mkSelector "preGap"

-- | @Selector@ for @setPreGap:@
setPreGapSelector :: Selector
setPreGapSelector = mkSelector "setPreGap:"

