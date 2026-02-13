{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReader
--
-- AVAssetReader provides services for obtaining media data from an asset.
--
-- Instances of AVAssetReader read media data from an instance of AVAsset, whether the asset is file-based or represents an assembly of media data from multiple sources, as is the case with AVComposition.
--
-- Clients of AVAssetReader can read data from specific tracks of an asset and in specific formats by adding concrete instances of AVAssetReaderOutput to an AVAssetReader instance.
--
-- AVAssetReaderTrackOutput, a concrete subclass of AVAssetReaderOutput, can either read the track's media samples in the format in which they are stored by the asset or convert the media samples to a different format.
--
-- AVAssetReaderAudioMixOutput mixes multiple audio tracks of the asset after reading them, while AVAssetReaderVideoCompositionOutput composites multiple video tracks after reading them.
--
-- Generated bindings for @AVAssetReader@.
module ObjC.AVFoundation.AVAssetReader
  ( AVAssetReader
  , IsAVAssetReader(..)
  , init_
  , new
  , assetReaderWithAsset_error
  , initWithAsset_error
  , canAddOutput
  , addOutput
  , startReading
  , cancelReading
  , asset
  , status
  , error_
  , outputs
  , addOutputSelector
  , assetReaderWithAsset_errorSelector
  , assetSelector
  , canAddOutputSelector
  , cancelReadingSelector
  , errorSelector
  , initSelector
  , initWithAsset_errorSelector
  , newSelector
  , outputsSelector
  , startReadingSelector
  , statusSelector

  -- * Enum types
  , AVAssetReaderStatus(AVAssetReaderStatus)
  , pattern AVAssetReaderStatusUnknown
  , pattern AVAssetReaderStatusReading
  , pattern AVAssetReaderStatusCompleted
  , pattern AVAssetReaderStatusFailed
  , pattern AVAssetReaderStatusCancelled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetReader avAssetReader => avAssetReader -> IO (Id AVAssetReader)
init_ avAssetReader =
  sendOwnedMessage avAssetReader initSelector

-- | @+ new@
new :: IO (Id AVAssetReader)
new  =
  do
    cls' <- getRequiredClass "AVAssetReader"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderWithAsset:error:
--
-- Returns an instance of AVAssetReader for reading media data from the specified asset.
--
-- @asset@ — The asset from which media data is to be read.
--
-- @outError@ — On return, if initialization of the AVAssetReader fails, points to an NSError describing the nature of the failure.
--
-- Returns: An instance of AVAssetReader.
--
-- If the specified asset belongs to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie, the results of any asset reading operation are undefined if you mutate the asset after invoking -startReading.
--
-- ObjC selector: @+ assetReaderWithAsset:error:@
assetReaderWithAsset_error :: (IsAVAsset asset, IsNSError outError) => asset -> outError -> IO (Id AVAssetReader)
assetReaderWithAsset_error asset outError =
  do
    cls' <- getRequiredClass "AVAssetReader"
    sendClassMessage cls' assetReaderWithAsset_errorSelector (toAVAsset asset) (toNSError outError)

-- | initWithAsset:error:
--
-- Creates an instance of AVAssetReader for reading media data from the specified asset.
--
-- @asset@ — The asset from which media data is to be read.
--
-- @outError@ — On return, if initialization of the AVAssetReader fails, points to an NSError describing the nature of the failure.
--
-- Returns: An instance of AVAssetReader.
--
-- If the specified asset belongs to a mutable subclass of AVAsset, AVMutableComposition or AVMutableMovie, the results of any asset reading operation are undefined if you mutate the asset after invoking -startReading.
--
-- ObjC selector: @- initWithAsset:error:@
initWithAsset_error :: (IsAVAssetReader avAssetReader, IsAVAsset asset, IsNSError outError) => avAssetReader -> asset -> outError -> IO (Id AVAssetReader)
initWithAsset_error avAssetReader asset outError =
  sendOwnedMessage avAssetReader initWithAsset_errorSelector (toAVAsset asset) (toNSError outError)

-- | canAddOutput:
--
-- Tests whether an output can be added to the receiver.
--
-- @output@ — The AVAssetReaderOutput object to be tested.
--
-- Returns: A BOOL indicating whether the output can be added to the receiver.
--
-- An output that reads from a track of an asset other than the asset used to initialize the receiver cannot be added.
--
-- ObjC selector: @- canAddOutput:@
canAddOutput :: (IsAVAssetReader avAssetReader, IsAVAssetReaderOutput output) => avAssetReader -> output -> IO Bool
canAddOutput avAssetReader output =
  sendMessage avAssetReader canAddOutputSelector (toAVAssetReaderOutput output)

-- | addOutput:
--
-- Adds an output to the receiver.
--
-- @output@ — The AVAssetReaderOutput object to be added.
--
-- Outputs are created with a reference to one or more AVAssetTrack objects. These tracks must be owned by the asset returned by the receiver's asset property.
--
-- This method throws an exception if the output has already been added to an AVAssetReader or if reading has started (@status@ has progressed beyond AVAssetReaderStatusUnknown).
--
-- ObjC selector: @- addOutput:@
addOutput :: (IsAVAssetReader avAssetReader, IsAVAssetReaderOutput output) => avAssetReader -> output -> IO ()
addOutput avAssetReader output =
  sendMessage avAssetReader addOutputSelector (toAVAssetReaderOutput output)

-- | startReading
--
-- Prepares the receiver for reading sample buffers from the asset.
--
-- Returns: A BOOL indicating whether reading could be started.
--
-- This method validates the entire collection of settings for outputs for tracks, for audio mixing, and for video composition and initiates reading from the receiver's asset.
--
-- If this method returns NO, clients can determine the nature of the failure by checking the value of the status and error properties.
--
-- This method throws an exception if reading has already started (@status@ has progressed beyond AVAssetReaderStatusUnknown).
--
-- ObjC selector: @- startReading@
startReading :: IsAVAssetReader avAssetReader => avAssetReader -> IO Bool
startReading avAssetReader =
  sendMessage avAssetReader startReadingSelector

-- | cancelReading
--
-- Cancels any background work and prevents the receiver's outputs from reading more samples.
--
-- Clients that want to stop reading samples from the receiver before reaching the end of its time range should call this method to stop any background read ahead operations that the may have been in progress.
--
-- This method should not be called concurrently with any calls to -[AVAssetReaderOutput copyNextSampleBuffer].
--
-- ObjC selector: @- cancelReading@
cancelReading :: IsAVAssetReader avAssetReader => avAssetReader -> IO ()
cancelReading avAssetReader =
  sendMessage avAssetReader cancelReadingSelector

-- | asset
--
-- The asset from which the receiver's outputs read sample buffers.
--
-- The value of this property is an AVAsset. Concrete instances of AVAssetReader that are created with specific AVAssetTrack instances must obtain those tracks from the asset returned by this property.
--
-- ObjC selector: @- asset@
asset :: IsAVAssetReader avAssetReader => avAssetReader -> IO (Id AVAsset)
asset avAssetReader =
  sendMessage avAssetReader assetSelector

-- | status
--
-- The status of reading sample buffers from the receiver's asset.
--
-- The value of this property is an AVAssetReaderStatus that indicates whether reading is in progress, has completed successfully, has been canceled, or has failed. Clients of AVAssetReaderOutput objects should check the value of this property after -[AVAssetReaderOutput copyNextSampleBuffer] returns NULL to determine why no more samples could be read. This property is thread safe.
--
-- ObjC selector: @- status@
status :: IsAVAssetReader avAssetReader => avAssetReader -> IO AVAssetReaderStatus
status avAssetReader =
  sendMessage avAssetReader statusSelector

-- | error
--
-- If the receiver's status is AVAssetReaderStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the receiver to no longer be able to read its asset. If the receiver's status is not AVAssetReaderStatusFailed, the value of this property is nil. This property is thread safe.
--
-- ObjC selector: @- error@
error_ :: IsAVAssetReader avAssetReader => avAssetReader -> IO (Id NSError)
error_ avAssetReader =
  sendMessage avAssetReader errorSelector

-- | outputs
--
-- The outputs from which clients of receiver can read media data.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetReaderOutput. Outputs can be added to the receiver using the addOutput: method.
--
-- ObjC selector: @- outputs@
outputs :: IsAVAssetReader avAssetReader => avAssetReader -> IO (Id NSArray)
outputs avAssetReader =
  sendMessage avAssetReader outputsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReader)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReader)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderWithAsset:error:@
assetReaderWithAsset_errorSelector :: Selector '[Id AVAsset, Id NSError] (Id AVAssetReader)
assetReaderWithAsset_errorSelector = mkSelector "assetReaderWithAsset:error:"

-- | @Selector@ for @initWithAsset:error:@
initWithAsset_errorSelector :: Selector '[Id AVAsset, Id NSError] (Id AVAssetReader)
initWithAsset_errorSelector = mkSelector "initWithAsset:error:"

-- | @Selector@ for @canAddOutput:@
canAddOutputSelector :: Selector '[Id AVAssetReaderOutput] Bool
canAddOutputSelector = mkSelector "canAddOutput:"

-- | @Selector@ for @addOutput:@
addOutputSelector :: Selector '[Id AVAssetReaderOutput] ()
addOutputSelector = mkSelector "addOutput:"

-- | @Selector@ for @startReading@
startReadingSelector :: Selector '[] Bool
startReadingSelector = mkSelector "startReading"

-- | @Selector@ for @cancelReading@
cancelReadingSelector :: Selector '[] ()
cancelReadingSelector = mkSelector "cancelReading"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id AVAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVAssetReaderStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @outputs@
outputsSelector :: Selector '[] (Id NSArray)
outputsSelector = mkSelector "outputs"

