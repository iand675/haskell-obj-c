{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetWriterInputCaptionAdaptor@.
module ObjC.AVFoundation.AVAssetWriterInputCaptionAdaptor
  ( AVAssetWriterInputCaptionAdaptor
  , IsAVAssetWriterInputCaptionAdaptor(..)
  , init_
  , new
  , assetWriterInputCaptionAdaptorWithAssetWriterInput
  , initWithAssetWriterInput
  , appendCaption
  , appendCaptionGroup
  , assetWriterInput
  , appendCaptionGroupSelector
  , appendCaptionSelector
  , assetWriterInputCaptionAdaptorWithAssetWriterInputSelector
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
init_ :: IsAVAssetWriterInputCaptionAdaptor avAssetWriterInputCaptionAdaptor => avAssetWriterInputCaptionAdaptor -> IO (Id AVAssetWriterInputCaptionAdaptor)
init_ avAssetWriterInputCaptionAdaptor =
  sendOwnedMessage avAssetWriterInputCaptionAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputCaptionAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputCaptionAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new caption adaptor for writing to the specified asset writer input.
--
-- ObjC selector: @+ assetWriterInputCaptionAdaptorWithAssetWriterInput:@
assetWriterInputCaptionAdaptorWithAssetWriterInput :: IsAVAssetWriterInput input => input -> IO (Id AVAssetWriterInputCaptionAdaptor)
assetWriterInputCaptionAdaptorWithAssetWriterInput input =
  do
    cls' <- getRequiredClass "AVAssetWriterInputCaptionAdaptor"
    sendClassMessage cls' assetWriterInputCaptionAdaptorWithAssetWriterInputSelector (toAVAssetWriterInput input)

-- | Creates a new caption adaptor for writing to the specified asset writer input.
--
-- This method thows an exception for any of the following reasons: - input is nil - the input's media type is not supported (should use text or closed caption) - the input is already attached to an asset writer caption adaptor - the input has already started writing
--
-- ObjC selector: @- initWithAssetWriterInput:@
initWithAssetWriterInput :: (IsAVAssetWriterInputCaptionAdaptor avAssetWriterInputCaptionAdaptor, IsAVAssetWriterInput input) => avAssetWriterInputCaptionAdaptor -> input -> IO (Id AVAssetWriterInputCaptionAdaptor)
initWithAssetWriterInput avAssetWriterInputCaptionAdaptor input =
  sendOwnedMessage avAssetWriterInputCaptionAdaptor initWithAssetWriterInputSelector (toAVAssetWriterInput input)

-- | Append a single caption to be written.
--
-- If this method returns NO, check the value of AVAssetWriter.status on the attached asset writer to determine why appending failed.
--
-- The start time of each caption's timeRange property must be numeric (see CMTIME_IS_NUMERIC) and must be at least as large as the start time of any previous caption (including any captions present in a group appended via -appendCaptionGroup:). In other words, the sequence of captions appended using this method must have monotonically increasing start times.
--
-- The duration of each caption's timeRange property must be numeric.
--
-- - Parameter caption: The caption to append.
--
-- - Returns: Returns YES if the operation succeeded, NO if it failed.
--
-- ObjC selector: @- appendCaption:@
appendCaption :: (IsAVAssetWriterInputCaptionAdaptor avAssetWriterInputCaptionAdaptor, IsAVCaption caption) => avAssetWriterInputCaptionAdaptor -> caption -> IO Bool
appendCaption avAssetWriterInputCaptionAdaptor caption =
  sendMessage avAssetWriterInputCaptionAdaptor appendCaptionSelector (toAVCaption caption)

-- | Append a group of captions to be written.
--
-- If this method returns NO, check the value of AVAssetWriter.status on the attached asset writer to determine why appending failed. When appending a sequence of captions groups, the start time of each group must be equal to or greater than the end time of any previous group. The easiest way to achieve this is to create the group using a caption whose duration is kCMTimeInvalid, in which case the duration will be determined by subtracting the start time of the group from the start time of the next appended group. When mixing calls to -appendCaptionGroup: and -appendCaption:, the start time of each group must be equal to or greater than the end time of any previous captions. To mark a time range containing no captions, append a group containing an empty caption array.
--
-- - Parameter captionGroup:
--
-- - Returns: Returns YES if the operation succeeded, NO if it failed.
--
-- ObjC selector: @- appendCaptionGroup:@
appendCaptionGroup :: (IsAVAssetWriterInputCaptionAdaptor avAssetWriterInputCaptionAdaptor, IsAVCaptionGroup captionGroup) => avAssetWriterInputCaptionAdaptor -> captionGroup -> IO Bool
appendCaptionGroup avAssetWriterInputCaptionAdaptor captionGroup =
  sendMessage avAssetWriterInputCaptionAdaptor appendCaptionGroupSelector (toAVCaptionGroup captionGroup)

-- | The asset writer input that was used to initialize the receiver.
--
-- ObjC selector: @- assetWriterInput@
assetWriterInput :: IsAVAssetWriterInputCaptionAdaptor avAssetWriterInputCaptionAdaptor => avAssetWriterInputCaptionAdaptor -> IO (Id AVAssetWriterInput)
assetWriterInput avAssetWriterInputCaptionAdaptor =
  sendMessage avAssetWriterInputCaptionAdaptor assetWriterInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputCaptionAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputCaptionAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputCaptionAdaptorWithAssetWriterInput:@
assetWriterInputCaptionAdaptorWithAssetWriterInputSelector :: Selector '[Id AVAssetWriterInput] (Id AVAssetWriterInputCaptionAdaptor)
assetWriterInputCaptionAdaptorWithAssetWriterInputSelector = mkSelector "assetWriterInputCaptionAdaptorWithAssetWriterInput:"

-- | @Selector@ for @initWithAssetWriterInput:@
initWithAssetWriterInputSelector :: Selector '[Id AVAssetWriterInput] (Id AVAssetWriterInputCaptionAdaptor)
initWithAssetWriterInputSelector = mkSelector "initWithAssetWriterInput:"

-- | @Selector@ for @appendCaption:@
appendCaptionSelector :: Selector '[Id AVCaption] Bool
appendCaptionSelector = mkSelector "appendCaption:"

-- | @Selector@ for @appendCaptionGroup:@
appendCaptionGroupSelector :: Selector '[Id AVCaptionGroup] Bool
appendCaptionGroupSelector = mkSelector "appendCaptionGroup:"

-- | @Selector@ for @assetWriterInput@
assetWriterInputSelector :: Selector '[] (Id AVAssetWriterInput)
assetWriterInputSelector = mkSelector "assetWriterInput"

