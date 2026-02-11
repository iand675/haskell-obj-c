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
  , newSelector
  , initWithSpecificationSelector


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
init_ :: IsAVPlayerVideoOutput avPlayerVideoOutput => avPlayerVideoOutput -> IO (Id AVPlayerVideoOutput)
init_ avPlayerVideoOutput  =
  sendMsg avPlayerVideoOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerVideoOutput)
new  =
  do
    cls' <- getRequiredClass "AVPlayerVideoOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithSpecification avPlayerVideoOutput  specification =
withObjCPtr specification $ \raw_specification ->
    sendMsg avPlayerVideoOutput (mkSelector "initWithSpecification:") (retPtr retVoid) [argPtr (castPtr raw_specification :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSpecification:@
initWithSpecificationSelector :: Selector
initWithSpecificationSelector = mkSelector "initWithSpecification:"

