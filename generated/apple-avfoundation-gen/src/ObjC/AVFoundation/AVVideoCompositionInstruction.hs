{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVVideoCompositionInstruction object represents an operation to be performed by a compositor.
--
-- An AVVideoComposition object maintains an array of instructions to perform its composition. This class is not intended to be subclassed; instead, conform to AVVideoCompositionInstructionProtocol ("AVVideoCompositionInstruction" in Objective-C). Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoCompositionInstruction@.
module ObjC.AVFoundation.AVVideoCompositionInstruction
  ( AVVideoCompositionInstruction
  , IsAVVideoCompositionInstruction(..)
  , videoCompositionInstructionWithInstruction
  , backgroundColor
  , layerInstructions
  , enablePostProcessing
  , requiredSourceTrackIDs
  , passthroughTrackID
  , requiredSourceSampleDataTrackIDs
  , backgroundColorSelector
  , enablePostProcessingSelector
  , layerInstructionsSelector
  , passthroughTrackIDSelector
  , requiredSourceSampleDataTrackIDsSelector
  , requiredSourceTrackIDsSelector
  , videoCompositionInstructionWithInstructionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Pass-through initializer, for internal use in AVFoundation only
--
-- ObjC selector: @+ videoCompositionInstructionWithInstruction:@
videoCompositionInstructionWithInstruction :: IsAVVideoCompositionInstruction instruction => instruction -> IO (Id AVVideoCompositionInstruction)
videoCompositionInstructionWithInstruction instruction =
  do
    cls' <- getRequiredClass "AVVideoCompositionInstruction"
    sendClassMessage cls' videoCompositionInstructionWithInstructionSelector (toAVVideoCompositionInstruction instruction)

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Ptr ())
backgroundColor avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction backgroundColorSelector

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- layerInstructions@
layerInstructions :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Id NSArray)
layerInstructions avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction layerInstructionsSelector

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- enablePostProcessing@
enablePostProcessing :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO Bool
enablePostProcessing avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction enablePostProcessingSelector

-- | List of video track IDs required to compose frames for this instruction. The value of this property is computed from the layer instructions.
--
-- ObjC selector: @- requiredSourceTrackIDs@
requiredSourceTrackIDs :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Id NSArray)
requiredSourceTrackIDs avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction requiredSourceTrackIDsSelector

-- | If the video composition result is one of the source frames for the duration of the instruction, this property returns the corresponding track ID. The compositor won't be run for the duration of the instruction and the proper source frame will be used instead. The value of this property is computed from the layer instructions
--
-- ObjC selector: @- passthroughTrackID@
passthroughTrackID :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO CInt
passthroughTrackID avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction passthroughTrackIDSelector

-- | List of track IDs for which sample data should be presented to the compositor for this instruction.
--
-- ObjC selector: @- requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDs :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Id NSArray)
requiredSourceSampleDataTrackIDs avVideoCompositionInstruction =
  sendMessage avVideoCompositionInstruction requiredSourceSampleDataTrackIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionInstructionWithInstruction:@
videoCompositionInstructionWithInstructionSelector :: Selector '[Id AVVideoCompositionInstruction] (Id AVVideoCompositionInstruction)
videoCompositionInstructionWithInstructionSelector = mkSelector "videoCompositionInstructionWithInstruction:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Ptr ())
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @layerInstructions@
layerInstructionsSelector :: Selector '[] (Id NSArray)
layerInstructionsSelector = mkSelector "layerInstructions"

-- | @Selector@ for @enablePostProcessing@
enablePostProcessingSelector :: Selector '[] Bool
enablePostProcessingSelector = mkSelector "enablePostProcessing"

-- | @Selector@ for @requiredSourceTrackIDs@
requiredSourceTrackIDsSelector :: Selector '[] (Id NSArray)
requiredSourceTrackIDsSelector = mkSelector "requiredSourceTrackIDs"

-- | @Selector@ for @passthroughTrackID@
passthroughTrackIDSelector :: Selector '[] CInt
passthroughTrackIDSelector = mkSelector "passthroughTrackID"

-- | @Selector@ for @requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDsSelector :: Selector '[] (Id NSArray)
requiredSourceSampleDataTrackIDsSelector = mkSelector "requiredSourceSampleDataTrackIDs"

