{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetReaderSampleReferenceOutput
--
-- AVAssetReaderSampleReferenceOutput is a concrete subclass of AVAssetReaderOutput that defines an interface for reading sample references from a single AVAssetTrack of an AVAssetReader's AVAsset.
--
-- Clients can extract information about the location (file URL and offset) of samples in a track by adding an instance of AVAssetReaderSampleReferenceOutput to an AVAssetReader using the -[AVAssetReader addOutput:] method. No actual sample data can be extracted using this class. The location of the sample data is described by the kCMSampleBufferAttachmentKey_SampleReferenceURL and kCMSampleBufferAttachmentKey_SampleReferenceByteOffset attachments on the extracted sample buffers. More information about sample buffers describing sample references can be found in the CMSampleBuffer documentation.
--
-- Sample buffers extracted using this class can also be appended to an AVAssetWriterInput to create movie tracks that are not self-contained and reference data in the original file instead.  Currently, only instances of AVAssetWriter configured to write files of type AVFileTypeQuickTimeMovie can be used to write tracks that are not self-contained.
--
-- Since no sample data is ever returned by instances of AVAssetReaderSampleReferenceOutput, the value of the alwaysCopiesSampleData property is ignored.
--
-- Generated bindings for @AVAssetReaderSampleReferenceOutput@.
module ObjC.AVFoundation.AVAssetReaderSampleReferenceOutput
  ( AVAssetReaderSampleReferenceOutput
  , IsAVAssetReaderSampleReferenceOutput(..)
  , init_
  , new
  , assetReaderSampleReferenceOutputWithTrack
  , initWithTrack
  , track
  , assetReaderSampleReferenceOutputWithTrackSelector
  , initSelector
  , initWithTrackSelector
  , newSelector
  , trackSelector


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
init_ :: IsAVAssetReaderSampleReferenceOutput avAssetReaderSampleReferenceOutput => avAssetReaderSampleReferenceOutput -> IO (Id AVAssetReaderSampleReferenceOutput)
init_ avAssetReaderSampleReferenceOutput =
  sendOwnedMessage avAssetReaderSampleReferenceOutput initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderSampleReferenceOutput)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderSampleReferenceOutput"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderSampleReferenceOutputWithTrack:
--
-- Returns an instance of AVAssetReaderSampleReferenceOutput for supplying sample references.
--
-- @track@ — The AVAssetTrack for which the resulting AVAssetReaderSampleReferenceOutput should provide sample references.
--
-- Returns: An instance of AVAssetReaderSampleReferenceOutput.
--
-- The track must be one of the tracks contained by the target AVAssetReader's asset.
--
-- ObjC selector: @+ assetReaderSampleReferenceOutputWithTrack:@
assetReaderSampleReferenceOutputWithTrack :: IsAVAssetTrack track => track -> IO (Id AVAssetReaderSampleReferenceOutput)
assetReaderSampleReferenceOutputWithTrack track =
  do
    cls' <- getRequiredClass "AVAssetReaderSampleReferenceOutput"
    sendClassMessage cls' assetReaderSampleReferenceOutputWithTrackSelector (toAVAssetTrack track)

-- | initWithTrack:
--
-- Returns an instance of AVAssetReaderSampleReferenceOutput for supplying sample references.
--
-- @track@ — The AVAssetTrack for which the resulting AVAssetReaderSampleReferenceOutput should provide sample references.
--
-- Returns: An instance of AVAssetReaderTrackOutput.
--
-- The track must be one of the tracks contained by the target AVAssetReader's asset.
--
-- ObjC selector: @- initWithTrack:@
initWithTrack :: (IsAVAssetReaderSampleReferenceOutput avAssetReaderSampleReferenceOutput, IsAVAssetTrack track) => avAssetReaderSampleReferenceOutput -> track -> IO (Id AVAssetReaderSampleReferenceOutput)
initWithTrack avAssetReaderSampleReferenceOutput track =
  sendOwnedMessage avAssetReaderSampleReferenceOutput initWithTrackSelector (toAVAssetTrack track)

-- | track
--
-- The track from which the receiver extracts sample references.
--
-- The value of this property is an AVAssetTrack owned by the target AVAssetReader's asset.
--
-- ObjC selector: @- track@
track :: IsAVAssetReaderSampleReferenceOutput avAssetReaderSampleReferenceOutput => avAssetReaderSampleReferenceOutput -> IO (Id AVAssetTrack)
track avAssetReaderSampleReferenceOutput =
  sendMessage avAssetReaderSampleReferenceOutput trackSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderSampleReferenceOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderSampleReferenceOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderSampleReferenceOutputWithTrack:@
assetReaderSampleReferenceOutputWithTrackSelector :: Selector '[Id AVAssetTrack] (Id AVAssetReaderSampleReferenceOutput)
assetReaderSampleReferenceOutputWithTrackSelector = mkSelector "assetReaderSampleReferenceOutputWithTrack:"

-- | @Selector@ for @initWithTrack:@
initWithTrackSelector :: Selector '[Id AVAssetTrack] (Id AVAssetReaderSampleReferenceOutput)
initWithTrackSelector = mkSelector "initWithTrack:"

-- | @Selector@ for @track@
trackSelector :: Selector '[] (Id AVAssetTrack)
trackSelector = mkSelector "track"

