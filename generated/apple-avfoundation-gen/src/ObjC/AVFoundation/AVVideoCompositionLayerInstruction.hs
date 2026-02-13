{-# LANGUAGE DataKinds #-}
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
  , trackIDSelector
  , videoCompositionLayerInstructionWithLayerInstructionSelector


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
-- ObjC selector: @+ videoCompositionLayerInstructionWithLayerInstruction:@
videoCompositionLayerInstructionWithLayerInstruction :: IsAVVideoCompositionLayerInstruction instruction => instruction -> IO (Id AVVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithLayerInstruction instruction =
  do
    cls' <- getRequiredClass "AVVideoCompositionLayerInstruction"
    sendClassMessage cls' videoCompositionLayerInstructionWithLayerInstructionSelector (toAVVideoCompositionLayerInstruction instruction)

-- | Indicates the trackID of the source track to which the compositor will apply the instruction.
--
-- ObjC selector: @- trackID@
trackID :: IsAVVideoCompositionLayerInstruction avVideoCompositionLayerInstruction => avVideoCompositionLayerInstruction -> IO CInt
trackID avVideoCompositionLayerInstruction =
  sendMessage avVideoCompositionLayerInstruction trackIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoCompositionLayerInstructionWithLayerInstruction:@
videoCompositionLayerInstructionWithLayerInstructionSelector :: Selector '[Id AVVideoCompositionLayerInstruction] (Id AVVideoCompositionLayerInstruction)
videoCompositionLayerInstructionWithLayerInstructionSelector = mkSelector "videoCompositionLayerInstructionWithLayerInstruction:"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

