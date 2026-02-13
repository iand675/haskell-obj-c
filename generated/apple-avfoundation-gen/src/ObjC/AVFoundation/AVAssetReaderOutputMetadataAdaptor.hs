{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetReaderOutputMetadataAdaptor@.
module ObjC.AVFoundation.AVAssetReaderOutputMetadataAdaptor
  ( AVAssetReaderOutputMetadataAdaptor
  , IsAVAssetReaderOutputMetadataAdaptor(..)
  , init_
  , new
  , assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput
  , initWithAssetReaderTrackOutput
  , nextTimedMetadataGroup
  , assetReaderTrackOutput
  , assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutputSelector
  , assetReaderTrackOutputSelector
  , initSelector
  , initWithAssetReaderTrackOutputSelector
  , newSelector
  , nextTimedMetadataGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetReaderOutputMetadataAdaptor avAssetReaderOutputMetadataAdaptor => avAssetReaderOutputMetadataAdaptor -> IO (Id AVAssetReaderOutputMetadataAdaptor)
init_ avAssetReaderOutputMetadataAdaptor =
  sendOwnedMessage avAssetReaderOutputMetadataAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderOutputMetadataAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderOutputMetadataAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput:
--
-- Creates a new timed metadata group adaptor for retrieving timed metadata group objects from an asset reader output.
--
-- @assetReaderOutput@ — An instance of AVAssetReaderTrackOutput that vends sample buffers containing metadata, e.g. an AVAssetReaderTrackOutput object initialized with a track of media type AVMediaTypeMetadata and nil outputSettings.
--
-- Returns: An instance of AVAssetReaderOutputMetadataAdaptor
--
-- It is an error to create a timed metadata group adaptor with an asset reader output that does not vend metadata.  It is also an error to create a timed metadata group adaptor with an asset reader output whose asset reader has already started reading, or an asset reader output that already has been used to initialize another timed metadata group adaptor.
--
-- Clients should not mix calls to -[AVAssetReaderTrackOutput copyNextSampleBuffer] and -[AVAssetReaderOutputMetadataAdaptor nextTimedMetadataGroup].  Once an AVAssetReaderTrackOutput instance has been used to initialize an AVAssetReaderOutputMetadataAdaptor, calling -copyNextSampleBuffer on that instance will result in an exception being thrown.
--
-- ObjC selector: @+ assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput:@
assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput :: IsAVAssetReaderTrackOutput trackOutput => trackOutput -> IO (Id AVAssetReaderOutputMetadataAdaptor)
assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput trackOutput =
  do
    cls' <- getRequiredClass "AVAssetReaderOutputMetadataAdaptor"
    sendClassMessage cls' assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutputSelector (toAVAssetReaderTrackOutput trackOutput)

-- | initWithAssetReaderTrackOutput:
--
-- Creates a new timed metadata group adaptor for retrieving timed metadata group objects from an asset reader output.
--
-- @assetReaderOutput@ — An instance of AVAssetReaderTrackOutput that vends sample buffers containing metadata, e.g. an AVAssetReaderTrackOutput object initialized with a track of media type AVMediaTypeMetadata and nil outputSettings.
--
-- Returns: An instance of AVAssetReaderOutputMetadataAdaptor
--
-- It is an error to create a timed metadata group adaptor with an asset reader output that does not vend metadata.  It is also an error to create a timed metadata group adaptor with an asset reader output whose asset reader has already started reading, or an asset reader output that already has been used to initialize another timed metadata group adaptor.
--
-- Clients should not mix calls to -[AVAssetReaderTrackOutput copyNextSampleBuffer] and -[AVAssetReaderOutputMetadataAdaptor nextTimedMetadataGroup].  Once an AVAssetReaderTrackOutput instance has been used to initialize an AVAssetReaderOutputMetadataAdaptor, calling -copyNextSampleBuffer on that instance will result in an exception being thrown.
--
-- This method throws an exception if the track's output was used to initialize another adaptor or if the track output's asset reader has already started reading.
--
-- ObjC selector: @- initWithAssetReaderTrackOutput:@
initWithAssetReaderTrackOutput :: (IsAVAssetReaderOutputMetadataAdaptor avAssetReaderOutputMetadataAdaptor, IsAVAssetReaderTrackOutput trackOutput) => avAssetReaderOutputMetadataAdaptor -> trackOutput -> IO (Id AVAssetReaderOutputMetadataAdaptor)
initWithAssetReaderTrackOutput avAssetReaderOutputMetadataAdaptor trackOutput =
  sendOwnedMessage avAssetReaderOutputMetadataAdaptor initWithAssetReaderTrackOutputSelector (toAVAssetReaderTrackOutput trackOutput)

-- | nextTimedMetadataGroup
--
-- Returns the next timed metadata group for the asset reader output, synchronously.
--
-- Returns: An instance of AVTimedMetadataGroup, representing the next logical segment of metadata coming from the source asset reader output.
--
-- This method will return nil when all timed metadata groups have been read from the asset reader output, or if there is an error that prevents the timed metadata group adaptor from reading more timed metadata groups.  When this method returns nil, clients should check the value of the associated AVAssetReader's status property to determine why no more samples could be read.
--
-- Unlike -[AVAssetReaderTrackOutput copyNextSampleBuffer], this method returns an autoreleased object.
--
-- Before calling this method, you must ensure that the output which underlies the receiver is attached to an AVAssetReader via a prior call to -addOutput: and that -startReading has been called on the asset reader.
--
-- This method throws an exception if track output is not attached to an asset reader and reading has not yet begun.
--
-- ObjC selector: @- nextTimedMetadataGroup@
nextTimedMetadataGroup :: IsAVAssetReaderOutputMetadataAdaptor avAssetReaderOutputMetadataAdaptor => avAssetReaderOutputMetadataAdaptor -> IO (Id AVTimedMetadataGroup)
nextTimedMetadataGroup avAssetReaderOutputMetadataAdaptor =
  sendMessage avAssetReaderOutputMetadataAdaptor nextTimedMetadataGroupSelector

-- | assetReaderTrackOutput
--
-- The asset reader track output from which the receiver pulls timed metadata groups.
--
-- ObjC selector: @- assetReaderTrackOutput@
assetReaderTrackOutput :: IsAVAssetReaderOutputMetadataAdaptor avAssetReaderOutputMetadataAdaptor => avAssetReaderOutputMetadataAdaptor -> IO (Id AVAssetReaderTrackOutput)
assetReaderTrackOutput avAssetReaderOutputMetadataAdaptor =
  sendMessage avAssetReaderOutputMetadataAdaptor assetReaderTrackOutputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderOutputMetadataAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderOutputMetadataAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput:@
assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutputSelector :: Selector '[Id AVAssetReaderTrackOutput] (Id AVAssetReaderOutputMetadataAdaptor)
assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutputSelector = mkSelector "assetReaderOutputMetadataAdaptorWithAssetReaderTrackOutput:"

-- | @Selector@ for @initWithAssetReaderTrackOutput:@
initWithAssetReaderTrackOutputSelector :: Selector '[Id AVAssetReaderTrackOutput] (Id AVAssetReaderOutputMetadataAdaptor)
initWithAssetReaderTrackOutputSelector = mkSelector "initWithAssetReaderTrackOutput:"

-- | @Selector@ for @nextTimedMetadataGroup@
nextTimedMetadataGroupSelector :: Selector '[] (Id AVTimedMetadataGroup)
nextTimedMetadataGroupSelector = mkSelector "nextTimedMetadataGroup"

-- | @Selector@ for @assetReaderTrackOutput@
assetReaderTrackOutputSelector :: Selector '[] (Id AVAssetReaderTrackOutput)
assetReaderTrackOutputSelector = mkSelector "assetReaderTrackOutput"

