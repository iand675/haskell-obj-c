{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetReaderOutputCaptionAdaptor@.
module ObjC.AVFoundation.AVAssetReaderOutputCaptionAdaptor
  ( AVAssetReaderOutputCaptionAdaptor
  , IsAVAssetReaderOutputCaptionAdaptor(..)
  , init_
  , new
  , assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput
  , initWithAssetReaderTrackOutput
  , nextCaptionGroup
  , captionsNotPresentInPreviousGroupsInCaptionGroup
  , assetReaderTrackOutput
  , validationDelegate
  , setValidationDelegate
  , assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector
  , assetReaderTrackOutputSelector
  , captionsNotPresentInPreviousGroupsInCaptionGroupSelector
  , initSelector
  , initWithAssetReaderTrackOutputSelector
  , newSelector
  , nextCaptionGroupSelector
  , setValidationDelegateSelector
  , validationDelegateSelector


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
init_ :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO (Id AVAssetReaderOutputCaptionAdaptor)
init_ avAssetReaderOutputCaptionAdaptor =
  sendOwnedMessage avAssetReaderOutputCaptionAdaptor initSelector

-- | @+ new@
new :: IO (Id AVAssetReaderOutputCaptionAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderOutputCaptionAdaptor"
    sendOwnedClassMessage cls' newSelector

-- | assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:
--
-- Creates a new caption adaptor for reading from the given track output.
--
-- @trackOutput@ — The track output from which to read captions.
--
-- Returns: A new instance of AVAssetReaderOutputCaptionAdaptor, configured to read captions from the given AVAssetReaderTrackOutput.
--
-- It is an error to pass nil to this method.
--
-- ObjC selector: @+ assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:@
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput :: IsAVAssetReaderTrackOutput trackOutput => trackOutput -> IO (Id AVAssetReaderOutputCaptionAdaptor)
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput trackOutput =
  do
    cls' <- getRequiredClass "AVAssetReaderOutputCaptionAdaptor"
    sendClassMessage cls' assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector (toAVAssetReaderTrackOutput trackOutput)

-- | initWithAssetReaderTrackOutput:
--
-- Creates a new caption adaptor for reading from the given track output.
--
-- @trackOutput@ — The track output from which to read captions.
--
-- Returns: A new instance of AVAssetReaderOutputCaptionAdaptor, configured to read captions from the given AVAssetReaderTrackOutput.
--
-- It is an error to pass nil to this method.
--
-- ObjC selector: @- initWithAssetReaderTrackOutput:@
initWithAssetReaderTrackOutput :: (IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor, IsAVAssetReaderTrackOutput trackOutput) => avAssetReaderOutputCaptionAdaptor -> trackOutput -> IO (Id AVAssetReaderOutputCaptionAdaptor)
initWithAssetReaderTrackOutput avAssetReaderOutputCaptionAdaptor trackOutput =
  sendOwnedMessage avAssetReaderOutputCaptionAdaptor initWithAssetReaderTrackOutputSelector (toAVAssetReaderTrackOutput trackOutput)

-- | nextCaptionGroup
--
-- Returns the next caption.
--
-- Returns: An instance of AVCaption representing the next caption.
--
-- The method returns the next caption group.
--
-- This method throws an exception if the track output is not attached to an asset reader and reading has not yet begun.
--
-- ObjC selector: @- nextCaptionGroup@
nextCaptionGroup :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO (Id AVCaptionGroup)
nextCaptionGroup avAssetReaderOutputCaptionAdaptor =
  sendMessage avAssetReaderOutputCaptionAdaptor nextCaptionGroupSelector

-- | captionsNotPresentInPreviousGroupsInCaptionGroup:
--
-- Returns the set of captions that are present in the given group but were not present in any group previously vended by calls to -nextCaptionGroup: on the receiver.
--
-- @captionGroup@ — The group containing the captions of interest.
--
-- Returns: An array of AVCaption objects.
--
-- The returned array contains the set of captions in the given group whose time ranges have the same start time as the group.  This method is provided as a convenience for clients who want to process captions one-by-one and do not need a complete view of the set of captions active at a given time.
--
-- ObjC selector: @- captionsNotPresentInPreviousGroupsInCaptionGroup:@
captionsNotPresentInPreviousGroupsInCaptionGroup :: (IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor, IsAVCaptionGroup captionGroup) => avAssetReaderOutputCaptionAdaptor -> captionGroup -> IO (Id NSArray)
captionsNotPresentInPreviousGroupsInCaptionGroup avAssetReaderOutputCaptionAdaptor captionGroup =
  sendMessage avAssetReaderOutputCaptionAdaptor captionsNotPresentInPreviousGroupsInCaptionGroupSelector (toAVCaptionGroup captionGroup)

-- | assetReaderTrackOutput
--
-- The track output used to create the receiver.
--
-- ObjC selector: @- assetReaderTrackOutput@
assetReaderTrackOutput :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO (Id AVAssetReaderTrackOutput)
assetReaderTrackOutput avAssetReaderOutputCaptionAdaptor =
  sendMessage avAssetReaderOutputCaptionAdaptor assetReaderTrackOutputSelector

-- | validationDelegate:
--
-- Register caption validation handling callback protocol to the caption adaptor.
--
-- ObjC selector: @- validationDelegate@
validationDelegate :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO RawId
validationDelegate avAssetReaderOutputCaptionAdaptor =
  sendMessage avAssetReaderOutputCaptionAdaptor validationDelegateSelector

-- | validationDelegate:
--
-- Register caption validation handling callback protocol to the caption adaptor.
--
-- ObjC selector: @- setValidationDelegate:@
setValidationDelegate :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> RawId -> IO ()
setValidationDelegate avAssetReaderOutputCaptionAdaptor value =
  sendMessage avAssetReaderOutputCaptionAdaptor setValidationDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetReaderOutputCaptionAdaptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetReaderOutputCaptionAdaptor)
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:@
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector :: Selector '[Id AVAssetReaderTrackOutput] (Id AVAssetReaderOutputCaptionAdaptor)
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector = mkSelector "assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:"

-- | @Selector@ for @initWithAssetReaderTrackOutput:@
initWithAssetReaderTrackOutputSelector :: Selector '[Id AVAssetReaderTrackOutput] (Id AVAssetReaderOutputCaptionAdaptor)
initWithAssetReaderTrackOutputSelector = mkSelector "initWithAssetReaderTrackOutput:"

-- | @Selector@ for @nextCaptionGroup@
nextCaptionGroupSelector :: Selector '[] (Id AVCaptionGroup)
nextCaptionGroupSelector = mkSelector "nextCaptionGroup"

-- | @Selector@ for @captionsNotPresentInPreviousGroupsInCaptionGroup:@
captionsNotPresentInPreviousGroupsInCaptionGroupSelector :: Selector '[Id AVCaptionGroup] (Id NSArray)
captionsNotPresentInPreviousGroupsInCaptionGroupSelector = mkSelector "captionsNotPresentInPreviousGroupsInCaptionGroup:"

-- | @Selector@ for @assetReaderTrackOutput@
assetReaderTrackOutputSelector :: Selector '[] (Id AVAssetReaderTrackOutput)
assetReaderTrackOutputSelector = mkSelector "assetReaderTrackOutput"

-- | @Selector@ for @validationDelegate@
validationDelegateSelector :: Selector '[] RawId
validationDelegateSelector = mkSelector "validationDelegate"

-- | @Selector@ for @setValidationDelegate:@
setValidationDelegateSelector :: Selector '[RawId] ()
setValidationDelegateSelector = mkSelector "setValidationDelegate:"

