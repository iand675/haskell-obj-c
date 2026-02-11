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
  , passthroughTrackID
  , videoCompositionInstructionWithInstructionSelector
  , backgroundColorSelector
  , layerInstructionsSelector
  , enablePostProcessingSelector
  , passthroughTrackIDSelector


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

-- | Pass-through initializer, for internal use in AVFoundation only
--
-- ObjC selector: @+ videoCompositionInstructionWithInstruction:@
videoCompositionInstructionWithInstruction :: IsAVVideoCompositionInstruction instruction => instruction -> IO (Id AVVideoCompositionInstruction)
videoCompositionInstructionWithInstruction instruction =
  do
    cls' <- getRequiredClass "AVVideoCompositionInstruction"
    withObjCPtr instruction $ \raw_instruction ->
      sendClassMsg cls' (mkSelector "videoCompositionInstructionWithInstruction:") (retPtr retVoid) [argPtr (castPtr raw_instruction :: Ptr ())] >>= retainedObject . castPtr

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Ptr ())
backgroundColor avVideoCompositionInstruction  =
  fmap castPtr $ sendMsg avVideoCompositionInstruction (mkSelector "backgroundColor") (retPtr retVoid) []

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- layerInstructions@
layerInstructions :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO (Id NSArray)
layerInstructions avVideoCompositionInstruction  =
  sendMsg avVideoCompositionInstruction (mkSelector "layerInstructions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- enablePostProcessing@
enablePostProcessing :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO Bool
enablePostProcessing avVideoCompositionInstruction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avVideoCompositionInstruction (mkSelector "enablePostProcessing") retCULong []

-- | If the video composition result is one of the source frames for the duration of the instruction, this property returns the corresponding track ID. The compositor won't be run for the duration of the instruction and the proper source frame will be used instead. The value of this property is computed from the layer instructions
--
-- ObjC selector: @- passthroughTrackID@
passthroughTrackID :: IsAVVideoCompositionInstruction avVideoCompositionInstruction => avVideoCompositionInstruction -> IO CInt
passthroughTrackID avVideoCompositionInstruction  =
  sendMsg avVideoCompositionInstruction (mkSelector "passthroughTrackID") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionInstructionWithInstruction:@
videoCompositionInstructionWithInstructionSelector :: Selector
videoCompositionInstructionWithInstructionSelector = mkSelector "videoCompositionInstructionWithInstruction:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @layerInstructions@
layerInstructionsSelector :: Selector
layerInstructionsSelector = mkSelector "layerInstructions"

-- | @Selector@ for @enablePostProcessing@
enablePostProcessingSelector :: Selector
enablePostProcessingSelector = mkSelector "enablePostProcessing"

-- | @Selector@ for @passthroughTrackID@
passthroughTrackIDSelector :: Selector
passthroughTrackIDSelector = mkSelector "passthroughTrackID"

