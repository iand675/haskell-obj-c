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
  , videoCompositionLayerInstructionWithAssetTrackSelector
  , videoCompositionLayerInstructionSelector
  , trackIDSelector
  , setTrackIDSelector


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

-- | Returns a new instance of AVMutableVideoCompositionLayerInstruction with no transform or opacity ramps and a trackID set to the specified track's trackID.
--
-- - Parameter track: A reference to an AVAssetTrack.
--
-- ObjC selector: @+ videoCompositionLayerInstructionWithAssetTrack:@
videoCompositionLayerInstructionWithAssetTrack :: IsAVAssetTrack track => track -> IO (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithAssetTrack track =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionLayerInstruction"
    withObjCPtr track $ \raw_track ->
      sendClassMsg cls' (mkSelector "videoCompositionLayerInstructionWithAssetTrack:") (retPtr retVoid) [argPtr (castPtr raw_track :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a new instance of AVMutableVideoCompositionLayerInstruction with no transform or opacity ramps and a trackID initialized to kCMPersistentTrackID_Invalid.
--
-- ObjC selector: @+ videoCompositionLayerInstruction@
videoCompositionLayerInstruction :: IO (Id AVMutableVideoCompositionLayerInstruction)
videoCompositionLayerInstruction  =
  do
    cls' <- getRequiredClass "AVMutableVideoCompositionLayerInstruction"
    sendClassMsg cls' (mkSelector "videoCompositionLayerInstruction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- trackID@
trackID :: IsAVMutableVideoCompositionLayerInstruction avMutableVideoCompositionLayerInstruction => avMutableVideoCompositionLayerInstruction -> IO CInt
trackID avMutableVideoCompositionLayerInstruction  =
  sendMsg avMutableVideoCompositionLayerInstruction (mkSelector "trackID") retCInt []

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- setTrackID:@
setTrackID :: IsAVMutableVideoCompositionLayerInstruction avMutableVideoCompositionLayerInstruction => avMutableVideoCompositionLayerInstruction -> CInt -> IO ()
setTrackID avMutableVideoCompositionLayerInstruction  value =
  sendMsg avMutableVideoCompositionLayerInstruction (mkSelector "setTrackID:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionLayerInstructionWithAssetTrack:@
videoCompositionLayerInstructionWithAssetTrackSelector :: Selector
videoCompositionLayerInstructionWithAssetTrackSelector = mkSelector "videoCompositionLayerInstructionWithAssetTrack:"

-- | @Selector@ for @videoCompositionLayerInstruction@
videoCompositionLayerInstructionSelector :: Selector
videoCompositionLayerInstructionSelector = mkSelector "videoCompositionLayerInstruction"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector
setTrackIDSelector = mkSelector "setTrackID:"

