{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableVideoCompositionInstruction@.
module ObjC.AVFoundation.AVMutableVideoCompositionInstruction
  ( AVMutableVideoCompositionInstruction
  , IsAVMutableVideoCompositionInstruction(..)
  , videoCompositionInstruction
  , backgroundColor
  , setBackgroundColor
  , layerInstructions
  , setLayerInstructions
  , enablePostProcessing
  , setEnablePostProcessing
  , requiredSourceSampleDataTrackIDs
  , setRequiredSourceSampleDataTrackIDs
  , backgroundColorSelector
  , enablePostProcessingSelector
  , layerInstructionsSelector
  , requiredSourceSampleDataTrackIDsSelector
  , setBackgroundColorSelector
  , setEnablePostProcessingSelector
  , setLayerInstructionsSelector
  , setRequiredSourceSampleDataTrackIDsSelector
  , videoCompositionInstructionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new instance of AVMutableVideoCompositionInstruction.
--
-- The returned AVMutableVideoCompositionInstruction will have a timeRange of kCMTimeRangeInvalid, a NULL backgroundColor, and a nil array of layerInstructions.
--
-- ObjC selector: @+ videoCompositionInstruction@
videoCompositionInstruction :: IO (Id AVMutableVideoCompositionInstruction)
videoCompositionInstruction  =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionInstruction"
    sendClassMessage cls' videoCompositionInstructionSelector

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Ptr ())
backgroundColor avMutableVideoCompositionInstruction =
  sendMessage avMutableVideoCompositionInstruction backgroundColorSelector

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> Ptr () -> IO ()
setBackgroundColor avMutableVideoCompositionInstruction value =
  sendMessage avMutableVideoCompositionInstruction setBackgroundColorSelector value

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- layerInstructions@
layerInstructions :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Id NSArray)
layerInstructions avMutableVideoCompositionInstruction =
  sendMessage avMutableVideoCompositionInstruction layerInstructionsSelector

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- setLayerInstructions:@
setLayerInstructions :: (IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction, IsNSArray value) => avMutableVideoCompositionInstruction -> value -> IO ()
setLayerInstructions avMutableVideoCompositionInstruction value =
  sendMessage avMutableVideoCompositionInstruction setLayerInstructionsSelector (toNSArray value)

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- enablePostProcessing@
enablePostProcessing :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO Bool
enablePostProcessing avMutableVideoCompositionInstruction =
  sendMessage avMutableVideoCompositionInstruction enablePostProcessingSelector

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- setEnablePostProcessing:@
setEnablePostProcessing :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> Bool -> IO ()
setEnablePostProcessing avMutableVideoCompositionInstruction value =
  sendMessage avMutableVideoCompositionInstruction setEnablePostProcessingSelector value

-- | List of sample data track IDs required to compose frames for this instruction.
--
-- Currently only tracks of type kCMMediaType_Metadata are allowed to be specified.  If this property is unspecified or is an empty array, no sample data is considered to be required for this instruction.  Note that you must also specify all tracks for which sample data is required for ANY instruction in the AVVideoComposition, in AVVideoComposition's property sourceSampleDataTrackIDs.
--
-- ObjC selector: @- requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDs :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Id NSArray)
requiredSourceSampleDataTrackIDs avMutableVideoCompositionInstruction =
  sendMessage avMutableVideoCompositionInstruction requiredSourceSampleDataTrackIDsSelector

-- | List of sample data track IDs required to compose frames for this instruction.
--
-- Currently only tracks of type kCMMediaType_Metadata are allowed to be specified.  If this property is unspecified or is an empty array, no sample data is considered to be required for this instruction.  Note that you must also specify all tracks for which sample data is required for ANY instruction in the AVVideoComposition, in AVVideoComposition's property sourceSampleDataTrackIDs.
--
-- ObjC selector: @- setRequiredSourceSampleDataTrackIDs:@
setRequiredSourceSampleDataTrackIDs :: (IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction, IsNSArray value) => avMutableVideoCompositionInstruction -> value -> IO ()
setRequiredSourceSampleDataTrackIDs avMutableVideoCompositionInstruction value =
  sendMessage avMutableVideoCompositionInstruction setRequiredSourceSampleDataTrackIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionInstruction@
videoCompositionInstructionSelector :: Selector '[] (Id AVMutableVideoCompositionInstruction)
videoCompositionInstructionSelector = mkSelector "videoCompositionInstruction"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Ptr ())
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Ptr ()] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @layerInstructions@
layerInstructionsSelector :: Selector '[] (Id NSArray)
layerInstructionsSelector = mkSelector "layerInstructions"

-- | @Selector@ for @setLayerInstructions:@
setLayerInstructionsSelector :: Selector '[Id NSArray] ()
setLayerInstructionsSelector = mkSelector "setLayerInstructions:"

-- | @Selector@ for @enablePostProcessing@
enablePostProcessingSelector :: Selector '[] Bool
enablePostProcessingSelector = mkSelector "enablePostProcessing"

-- | @Selector@ for @setEnablePostProcessing:@
setEnablePostProcessingSelector :: Selector '[Bool] ()
setEnablePostProcessingSelector = mkSelector "setEnablePostProcessing:"

-- | @Selector@ for @requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDsSelector :: Selector '[] (Id NSArray)
requiredSourceSampleDataTrackIDsSelector = mkSelector "requiredSourceSampleDataTrackIDs"

-- | @Selector@ for @setRequiredSourceSampleDataTrackIDs:@
setRequiredSourceSampleDataTrackIDsSelector :: Selector '[Id NSArray] ()
setRequiredSourceSampleDataTrackIDsSelector = mkSelector "setRequiredSourceSampleDataTrackIDs:"

