{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableVideoCompositionLayerInstruction@.
module ObjC.AVFoundation.AVMutableVideoCompositionLayerInstruction
  ( AVMutableVideoCompositionLayerInstruction
  , IsAVMutableVideoCompositionLayerInstruction(..)
  , videoCompositionLayerInstructionWithAssetTrack
  , videoCompositionLayerInstruction
  , trackID
  , setTrackID
  , setTrackIDSelector
  , trackIDSelector
  , videoCompositionLayerInstructionSelector
  , videoCompositionLayerInstructionWithAssetTrackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new instance of AVMutableVideoCompositionLayerInstruction with no transform or opacity ramps and a trackID set to the specified track's trackID.
--
-- - Parameter track: A reference to an AVAssetTrack.
--
-- ObjC selector: @+ videoCompositionLayerInstructionWithAssetTrack:@
videoCompositionLayerInstructionWithAssetTrack :: IsAVAssetTrack track => track -> IO (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithAssetTrack track =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionLayerInstruction"
    sendClassMessage cls' videoCompositionLayerInstructionWithAssetTrackSelector (toAVAssetTrack track)

-- | Returns a new instance of AVMutableVideoCompositionLayerInstruction with no transform or opacity ramps and a trackID initialized to kCMPersistentTrackID_Invalid.
--
-- ObjC selector: @+ videoCompositionLayerInstruction@
videoCompositionLayerInstruction :: IO (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstruction  =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionLayerInstruction"
    sendClassMessage cls' videoCompositionLayerInstructionSelector

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- trackID@
trackID :: IsAVMutableVideoCompositionLayerInstruction avMutableVideoCompositionLayerInstruction => avMutableVideoCompositionLayerInstruction -> IO CInt
trackID avMutableVideoCompositionLayerInstruction =
  sendMessage avMutableVideoCompositionLayerInstruction trackIDSelector

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- setTrackID:@
setTrackID :: IsAVMutableVideoCompositionLayerInstruction avMutableVideoCompositionLayerInstruction => avMutableVideoCompositionLayerInstruction -> CInt -> IO ()
setTrackID avMutableVideoCompositionLayerInstruction value =
  sendMessage avMutableVideoCompositionLayerInstruction setTrackIDSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionLayerInstructionWithAssetTrack:@
videoCompositionLayerInstructionWithAssetTrackSelector :: Selector '[Id AVAssetTrack] (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithAssetTrackSelector = mkSelector "videoCompositionLayerInstructionWithAssetTrack:"

-- | @Selector@ for @videoCompositionLayerInstruction@
videoCompositionLayerInstructionSelector :: Selector '[] (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstructionSelector = mkSelector "videoCompositionLayerInstruction"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector '[CInt] ()
setTrackIDSelector = mkSelector "setTrackID:"

