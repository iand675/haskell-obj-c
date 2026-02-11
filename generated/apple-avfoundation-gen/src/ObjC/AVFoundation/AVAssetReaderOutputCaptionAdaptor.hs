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
  , initSelector
  , newSelector
  , assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector
  , initWithAssetReaderTrackOutputSelector
  , nextCaptionGroupSelector
  , captionsNotPresentInPreviousGroupsInCaptionGroupSelector
  , assetReaderTrackOutputSelector
  , validationDelegateSelector
  , setValidationDelegateSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO (Id AVAssetReaderOutputCaptionAdaptor)
init_ avAssetReaderOutputCaptionAdaptor  =
    sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetReaderOutputCaptionAdaptor)
new  =
  do
    cls' <- getRequiredClass "AVAssetReaderOutputCaptionAdaptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr trackOutput $ \raw_trackOutput ->
      sendClassMsg cls' (mkSelector "assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:") (retPtr retVoid) [argPtr (castPtr raw_trackOutput :: Ptr ())] >>= retainedObject . castPtr

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
initWithAssetReaderTrackOutput avAssetReaderOutputCaptionAdaptor  trackOutput =
  withObjCPtr trackOutput $ \raw_trackOutput ->
      sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "initWithAssetReaderTrackOutput:") (retPtr retVoid) [argPtr (castPtr raw_trackOutput :: Ptr ())] >>= ownedObject . castPtr

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
nextCaptionGroup avAssetReaderOutputCaptionAdaptor  =
    sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "nextCaptionGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

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
captionsNotPresentInPreviousGroupsInCaptionGroup avAssetReaderOutputCaptionAdaptor  captionGroup =
  withObjCPtr captionGroup $ \raw_captionGroup ->
      sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "captionsNotPresentInPreviousGroupsInCaptionGroup:") (retPtr retVoid) [argPtr (castPtr raw_captionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | assetReaderTrackOutput
--
-- The track output used to create the receiver.
--
-- ObjC selector: @- assetReaderTrackOutput@
assetReaderTrackOutput :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO (Id AVAssetReaderTrackOutput)
assetReaderTrackOutput avAssetReaderOutputCaptionAdaptor  =
    sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "assetReaderTrackOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | validationDelegate:
--
-- Register caption validation handling callback protocol to the caption adaptor.
--
-- ObjC selector: @- validationDelegate@
validationDelegate :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> IO RawId
validationDelegate avAssetReaderOutputCaptionAdaptor  =
    fmap (RawId . castPtr) $ sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "validationDelegate") (retPtr retVoid) []

-- | validationDelegate:
--
-- Register caption validation handling callback protocol to the caption adaptor.
--
-- ObjC selector: @- setValidationDelegate:@
setValidationDelegate :: IsAVAssetReaderOutputCaptionAdaptor avAssetReaderOutputCaptionAdaptor => avAssetReaderOutputCaptionAdaptor -> RawId -> IO ()
setValidationDelegate avAssetReaderOutputCaptionAdaptor  value =
    sendMsg avAssetReaderOutputCaptionAdaptor (mkSelector "setValidationDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:@
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector :: Selector
assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutputSelector = mkSelector "assetReaderOutputCaptionAdaptorWithAssetReaderTrackOutput:"

-- | @Selector@ for @initWithAssetReaderTrackOutput:@
initWithAssetReaderTrackOutputSelector :: Selector
initWithAssetReaderTrackOutputSelector = mkSelector "initWithAssetReaderTrackOutput:"

-- | @Selector@ for @nextCaptionGroup@
nextCaptionGroupSelector :: Selector
nextCaptionGroupSelector = mkSelector "nextCaptionGroup"

-- | @Selector@ for @captionsNotPresentInPreviousGroupsInCaptionGroup:@
captionsNotPresentInPreviousGroupsInCaptionGroupSelector :: Selector
captionsNotPresentInPreviousGroupsInCaptionGroupSelector = mkSelector "captionsNotPresentInPreviousGroupsInCaptionGroup:"

-- | @Selector@ for @assetReaderTrackOutput@
assetReaderTrackOutputSelector :: Selector
assetReaderTrackOutputSelector = mkSelector "assetReaderTrackOutput"

-- | @Selector@ for @validationDelegate@
validationDelegateSelector :: Selector
validationDelegateSelector = mkSelector "validationDelegate"

-- | @Selector@ for @setValidationDelegate:@
setValidationDelegateSelector :: Selector
setValidationDelegateSelector = mkSelector "setValidationDelegate:"

