{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetWriterInputMetadataAdaptor@.
module ObjC.AVFoundation.AVAssetWriterInputMetadataAdaptor
  ( AVAssetWriterInputMetadataAdaptor
  , IsAVAssetWriterInputMetadataAdaptor(..)
  , init_
  , new
  , assetWriterInputMetadataAdaptorWithAssetWriterInput
  , initWithAssetWriterInput
  , appendTimedMetadataGroup
  , assetWriterInput
  , appendTimedMetadataGroupSelector
  , assetWriterInputMetadataAdaptorWithAssetWriterInputSelector
  , assetWriterInputSelector
  , initSelector
  , initWithAssetWriterInputSelector
  , newSelector


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
init_ :: IsAVAssetWriterInputMetadataAdaptor avAssetWriterInputMetadataAdaptor => avAssetWriterInputMetadataAdaptor -> IO (Id AVAssetWriterInputMetadataAdaptor)
init_ avAssetWriterInputMetadataAdaptor =
  sendOwnedMessage avAssetWriterInputMetadataAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputMetadataAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputMetadataAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new timed metadata group adaptor to receive instances of AVTimedMetadataGroup for writing to the output file.
--
-- The instance of AVAssetWriterInput passed in to this method must have been created with a format hint indicating all possible combinations of identifier (or, alternatively, key and keySpace), dataType, and extendedLanguageTag that will be appended to the metadata adaptor. It is an error to append metadata items not represented in the input's format hint.
--
-- This method throws an exception for any of the following reasons: - input is already attached to another instance of AVAssetWriterInputMetadataAdaptor - input's asset writer has already started writing (progressed beyond AVAssetWriterStatusUnknown) - input's asset writer does not carry a source format hint - input's source format hint media subtype is not kCMMetadataFormatType_Boxed
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append groups of timed metadata. Only asset writer inputs that accept media data of type AVMediaTypeMetadata can be used to initialize a timed metadata group adaptor.
--
-- - Returns: An instance of AVAssetWriterInputMetadataAdaptor.
--
-- ObjC selector: @+ assetWriterInputMetadataAdaptorWithAssetWriterInput:@
assetWriterInputMetadataAdaptorWithAssetWriterInput :: IsAVAssetWriterInput input => input -> IO (Id AVAssetWriterInputMetadataAdaptor)
assetWriterInputMetadataAdaptorWithAssetWriterInput input =
  do
    cls' <- getRequiredClass "AVAssetWriterInputMetadataAdaptor"
    sendClassMessage cls' assetWriterInputMetadataAdaptorWithAssetWriterInputSelector (toAVAssetWriterInput input)

-- | Creates a new timed metadator group adaptor to receive instances of AVTimedMetadataGroup for writing to the output file.
--
-- The instance of AVAssetWriterInput passed in to this method must have been created with a format hint indicating all possible combinations of identifier (or, alternatively, key and keySpace), dataType, and extendedLanguageTag that will be appended to the metadata adaptor. It is an error to append metadata items not represented in the input's format hint. For help creating a suitable format hint, see -[AVTimedMetadataGroup copyFormatDescription].
--
-- This method throws an exception for any of the following reasons: - input is already attached to another instance of AVAssetWriterInputMetadataAdaptor - input's asset writer has already started writing (progressed beyond AVAssetWriterStatusUnknown) - input's asset writer does not carry a source format hint - input's source format hint media subtype is not kCMMetadataFormatType_Boxed
--
-- - Parameter input: An instance of AVAssetWriterInput to which the receiver should append groups of timed metadata. Only asset writer inputs that accept media data of type AVMediaTypeMetadata can be used to initialize a timed metadata group adaptor.
--
-- - Returns: An instance of AVAssetWriterInputMetadataAdaptor.
--
-- ObjC selector: @- initWithAssetWriterInput:@
initWithAssetWriterInput :: (IsAVAssetWriterInputMetadataAdaptor avAssetWriterInputMetadataAdaptor, IsAVAssetWriterInput input) => avAssetWriterInputMetadataAdaptor -> input -> IO (Id AVAssetWriterInputMetadataAdaptor)
initWithAssetWriterInput avAssetWriterInputMetadataAdaptor input =
  sendOwnedMessage avAssetWriterInputMetadataAdaptor initWithAssetWriterInputSelector (toAVAssetWriterInput input)

-- | Appends a timed metadata group to the receiver.
--
-- The receiver will retain the AVTimedMetadataGroup until it is done with it, and then release it.
--
-- The timing of the metadata items in the output asset will correspond to the timeRange of the AVTimedMetadataGroup, regardless of the values of the time and duration properties of the individual items.
--
-- Before calling this method, you must ensure that the input that underlies the receiver is attached to an AVAssetWriter via a prior call to -addInput: and that -startWriting has been called on the asset writer. It is an error to invoke this method before starting a session (via -[AVAssetWriter startSessionAtSourceTime:]) or after ending a session (via -[AVAssetWriter endSessionAtSourceTime:]).
--
-- This method throws an exception if the attached asset writer input has not been added to an asset writer or -startWriting has not been called on that asset writer.
--
-- - Parameter timedMetadataGroup: The AVTimedMetadataGroup to be appended.
--
-- - Returns: A BOOL value indicating success of appending the timed metadata group.  If a result of NO is returned, AVAssetWriter.error will contain more information about why apending the timed metadata group failed.
--
-- ObjC selector: @- appendTimedMetadataGroup:@
appendTimedMetadataGroup :: (IsAVAssetWriterInputMetadataAdaptor avAssetWriterInputMetadataAdaptor, IsAVTimedMetadataGroup timedMetadataGroup) => avAssetWriterInputMetadataAdaptor -> timedMetadataGroup -> IO Bool
appendTimedMetadataGroup avAssetWriterInputMetadataAdaptor timedMetadataGroup =
  sendMessage avAssetWriterInputMetadataAdaptor appendTimedMetadataGroupSelector (toAVTimedMetadataGroup timedMetadataGroup)

-- | The asset writer input to which the receiver should append timed metadata groups.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputMetadataAdaptor avAssetWriterInputMetadataAdaptor => avAssetWriterInputMetadataAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputMetadataAdaptor =
  sendMessage avAssetWriterInputMetadataAdaptor assetWriterInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputMetadataAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputMetadataAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputMetadataAdaptorWithAssetWriterInput:@
assetWriterInputMetadataAdaptorWithAssetWriterInputSelector :: Selector '[Id AVAssetWriterInput] (Id AVAssetWriterInputMetadataAdaptor)
assetWriterInputMetadataAdaptorWithAssetWriterInputSelector = mkSelector "assetWriterInputMetadataAdaptorWithAssetWriterInput:"

-- | @Selector@ for @initWithAssetWriterInput:@
initWithAssetWriterInputSelector :: Selector '[Id AVAssetWriterInput] (Id AVAssetWriterInputMetadataAdaptor)
initWithAssetWriterInputSelector = mkSelector "initWithAssetWriterInput:"

-- | @Selector@ for @appendTimedMetadataGroup:@
appendTimedMetadataGroupSelector :: Selector '[Id AVTimedMetadataGroup] Bool
appendTimedMetadataGroupSelector = mkSelector "appendTimedMetadataGroup:"

-- | @Selector@ for @assetWriterInput@
assetWriterInputSelector :: Selector '[] (Id AVAssetWriterInput)
assetWriterInputSelector = mkSelector "assetWriterInput"

