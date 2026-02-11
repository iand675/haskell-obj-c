{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVVideoCompositionLayerInstruction object represents the transform, opacity, and cropping ramps to apply to a given track. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoCompositionLayerInstruction@.
module ObjC.AVFoundation.AVVideoCompositionLayerInstruction
  ( AVVideoCompositionLayerInstruction
  , IsAVVideoCompositionLayerInstruction(..)
  , videoCompositionLayerInstructionWithLayerInstruction
  , trackID
  , videoCompositionLayerInstructionWithLayerInstructionSelector
  , trackIDSelector


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
-- ObjC selector: @+ videoCompositionLayerInstructionWithLayerInstruction:@
videoCompositionLayerInstructionWithLayerInstruction :: IsAVVideoCompositionLayerInstruction instruction => instruction -> IO (Id AVVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithLayerInstruction instruction =
  do
    cls' <- getRequiredClass "AVVideoCompositionLayerInstruction"
    withObjCPtr instruction $ \raw_instruction ->
      sendClassMsg cls' (mkSelector "videoCompositionLayerInstructionWithLayerInstruction:") (retPtr retVoid) [argPtr (castPtr raw_instruction :: Ptr ())] >>= retainedObject . castPtr

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- trackID@
trackID :: IsAVVideoCompositionLayerInstruction avVideoCompositionLayerInstruction => avVideoCompositionLayerInstruction -> IO CInt
trackID avVideoCompositionLayerInstruction  =
  sendMsg avVideoCompositionLayerInstruction (mkSelector "trackID") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionLayerInstructionWithLayerInstruction:@
videoCompositionLayerInstructionWithLayerInstructionSelector :: Selector
videoCompositionLayerInstructionWithLayerInstructionSelector = mkSelector "videoCompositionLayerInstructionWithLayerInstruction:"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

