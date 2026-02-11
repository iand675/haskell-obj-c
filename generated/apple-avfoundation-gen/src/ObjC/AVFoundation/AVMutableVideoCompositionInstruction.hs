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
  , videoCompositionInstructionSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , layerInstructionsSelector
  , setLayerInstructionsSelector
  , enablePostProcessingSelector
  , setEnablePostProcessingSelector
  , requiredSourceSampleDataTrackIDsSelector
  , setRequiredSourceSampleDataTrackIDsSelector


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

-- | Returns a new instance of AVMutableVideoCompositionInstruction.
--
-- The returned AVMutableVideoCompositionInstruction will have a timeRange of kCMTimeRangeInvalid, a NULL backgroundColor, and a nil array of layerInstructions.
--
-- ObjC selector: @+ videoCompositionInstruction@
videoCompositionInstruction :: IO (Id AVMutableVideoCompositionInstruction)
videoCompositionInstruction  =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionInstruction"
    sendClassMsg cls' (mkSelector "videoCompositionInstruction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Ptr ())
backgroundColor avMutableVideoCompositionInstruction  =
    fmap castPtr $ sendMsg avMutableVideoCompositionInstruction (mkSelector "backgroundColor") (retPtr retVoid) []

-- | Indicates the background color of the composition.
--
-- Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored. - If the background color is not specified the video compositor will use a default backgroundColor of opaque black. - If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> Ptr () -> IO ()
setBackgroundColor avMutableVideoCompositionInstruction  value =
    sendMsg avMutableVideoCompositionInstruction (mkSelector "setBackgroundColor:") retVoid [argPtr value]

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- layerInstructions@
layerInstructions :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Id NSArray)
layerInstructions avMutableVideoCompositionInstruction  =
    sendMsg avMutableVideoCompositionInstruction (mkSelector "layerInstructions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of instances of AVVideoCompositionLayerInstruction that specify how video frames from source tracks should be layered and composed.
--
-- Tracks are layered in the composition according to the top-to-bottom order of the layerInstructions array; the track with trackID of the first instruction in the array will be layered on top, with the track with the trackID of the second instruction immediately underneath, etc. If this key is nil, the output will be a fill of the background color.
--
-- ObjC selector: @- setLayerInstructions:@
setLayerInstructions :: (IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction, IsNSArray value) => avMutableVideoCompositionInstruction -> value -> IO ()
setLayerInstructions avMutableVideoCompositionInstruction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableVideoCompositionInstruction (mkSelector "setLayerInstructions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- enablePostProcessing@
enablePostProcessing :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO Bool
enablePostProcessing avMutableVideoCompositionInstruction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableVideoCompositionInstruction (mkSelector "enablePostProcessing") retCULong []

-- | If NO, indicates that post-processing should be skipped for the duration of this instruction.  YES by default.
--
-- See +[AVVideoCompositionCoreAnimationTool videoCompositionToolWithPostProcessingAsVideoLayer:inLayer:].
--
-- ObjC selector: @- setEnablePostProcessing:@
setEnablePostProcessing :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> Bool -> IO ()
setEnablePostProcessing avMutableVideoCompositionInstruction  value =
    sendMsg avMutableVideoCompositionInstruction (mkSelector "setEnablePostProcessing:") retVoid [argCULong (if value then 1 else 0)]

-- | List of sample data track IDs required to compose frames for this instruction.
--
-- Currently only tracks of type kCMMediaType_Metadata are allowed to be specified.  If this property is unspecified or is an empty array, no sample data is considered to be required for this instruction.  Note that you must also specify all tracks for which sample data is required for ANY instruction in the AVVideoComposition, in AVVideoComposition's property sourceSampleDataTrackIDs.
--
-- ObjC selector: @- requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDs :: IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction => avMutableVideoCompositionInstruction -> IO (Id NSArray)
requiredSourceSampleDataTrackIDs avMutableVideoCompositionInstruction  =
    sendMsg avMutableVideoCompositionInstruction (mkSelector "requiredSourceSampleDataTrackIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of sample data track IDs required to compose frames for this instruction.
--
-- Currently only tracks of type kCMMediaType_Metadata are allowed to be specified.  If this property is unspecified or is an empty array, no sample data is considered to be required for this instruction.  Note that you must also specify all tracks for which sample data is required for ANY instruction in the AVVideoComposition, in AVVideoComposition's property sourceSampleDataTrackIDs.
--
-- ObjC selector: @- setRequiredSourceSampleDataTrackIDs:@
setRequiredSourceSampleDataTrackIDs :: (IsAVMutableVideoCompositionInstruction avMutableVideoCompositionInstruction, IsNSArray value) => avMutableVideoCompositionInstruction -> value -> IO ()
setRequiredSourceSampleDataTrackIDs avMutableVideoCompositionInstruction  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableVideoCompositionInstruction (mkSelector "setRequiredSourceSampleDataTrackIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionInstruction@
videoCompositionInstructionSelector :: Selector
videoCompositionInstructionSelector = mkSelector "videoCompositionInstruction"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @layerInstructions@
layerInstructionsSelector :: Selector
layerInstructionsSelector = mkSelector "layerInstructions"

-- | @Selector@ for @setLayerInstructions:@
setLayerInstructionsSelector :: Selector
setLayerInstructionsSelector = mkSelector "setLayerInstructions:"

-- | @Selector@ for @enablePostProcessing@
enablePostProcessingSelector :: Selector
enablePostProcessingSelector = mkSelector "enablePostProcessing"

-- | @Selector@ for @setEnablePostProcessing:@
setEnablePostProcessingSelector :: Selector
setEnablePostProcessingSelector = mkSelector "setEnablePostProcessing:"

-- | @Selector@ for @requiredSourceSampleDataTrackIDs@
requiredSourceSampleDataTrackIDsSelector :: Selector
requiredSourceSampleDataTrackIDsSelector = mkSelector "requiredSourceSampleDataTrackIDs"

-- | @Selector@ for @setRequiredSourceSampleDataTrackIDs:@
setRequiredSourceSampleDataTrackIDsSelector :: Selector
setRequiredSourceSampleDataTrackIDsSelector = mkSelector "setRequiredSourceSampleDataTrackIDs:"

