{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerVideoOutput
--
-- AVPlayerVideoOutput offers a way to attach to an AVPlayer and receive video frames and video-related data vended through CMTaggedBufferGroups.
--
-- AVPlayerVideoOutput can be attached to an AVPlayer using AVPlayer's method addVideoOutput:				Note:  An AVPlayerVideoOutput can only be attached to a single player at a time, attempting to attach to multiple player will result in an exception being thrown.				Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerVideoOutput@.
module ObjC.AVFoundation.AVPlayerVideoOutput
  ( AVPlayerVideoOutput
  , IsAVPlayerVideoOutput(..)
  , init_
  , new
  , initWithSpecification
  , initSelector
  , initWithSpecificationSelector
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
init_ :: IsAVPlayerVideoOutput avPlayerVideoOutput => avPlayerVideoOutput -> IO (Id AVPlayerVideoOutput)
init_ avPlayerVideoOutput =
  sendOwnedMessage avPlayerVideoOutput initSelector

-- | @+ new@
new :: IO (Id AVPlayerVideoOutput)
new  =
  do
    cls' <- getRequiredClass "AVPlayerVideoOutput"
    sendOwnedClassMessage cls' newSelector

-- | initWithSpecification:
--
-- Creates an instance of AVPlayerVideoOutput, initialized with the specified video output specification.
--
-- @specification@ — An instance of AVVideoOutputSpecification, used to recommend data channels to the AVPlayer associated with this AVPlayerVideoOutput.				The tag collections owned by the AVVideoOutputSpecification will be given a priority based on their position in the array which they are held by AVVideoOutputSpecification, meaning position i takes priority over position i+1.				This means that the player will first check if the tag collection at index 0 matches the shape of the current item's data channels.				If the item's data channels would not be able satisfy the shape of the requested tag collection, it will fall back to the next collection and repeat this process.				This continues until a tag collection or set of tag collection can be selected, otherwise if no collections match the shape of the item’s data channels then samples cannot be vended for that item.
--
-- Returns: An instance of AVPlayerVideoOutput.
--
-- Output settings will be selected from the input AVVideoOutputSpecification based on the data channels selected for an item.				If no output settings were set for the selected tag collection, then the default output settings from the AVVideoOutputSpecification will be used if those were set.
--
-- ObjC selector: @- initWithSpecification:@
initWithSpecification :: (IsAVPlayerVideoOutput avPlayerVideoOutput, IsAVVideoOutputSpecification specification) => avPlayerVideoOutput -> specification -> IO (Id AVPlayerVideoOutput)
initWithSpecification avPlayerVideoOutput specification =
  sendOwnedMessage avPlayerVideoOutput initWithSpecificationSelector (toAVVideoOutputSpecification specification)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerVideoOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerVideoOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSpecification:@
initWithSpecificationSelector :: Selector '[Id AVVideoOutputSpecification] (Id AVPlayerVideoOutput)
initWithSpecificationSelector = mkSelector "initWithSpecification:"

