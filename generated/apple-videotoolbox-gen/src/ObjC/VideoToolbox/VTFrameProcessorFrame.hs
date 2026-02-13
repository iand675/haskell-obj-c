{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Helper class to wrap pixel buffers as video frames.
--
-- You can use the frames as source frames, reference frames, or output frames of a processor. Frame instances retain the backing pixel buffer.
--
-- Generated bindings for @VTFrameProcessorFrame@.
module ObjC.VideoToolbox.VTFrameProcessorFrame
  ( VTFrameProcessorFrame
  , IsVTFrameProcessorFrame(..)
  , init_
  , new
  , buffer
  , bufferSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVTFrameProcessorFrame vtFrameProcessorFrame => vtFrameProcessorFrame -> IO (Id VTFrameProcessorFrame)
init_ vtFrameProcessorFrame =
  sendOwnedMessage vtFrameProcessorFrame initSelector

-- | @+ new@
new :: IO (Id VTFrameProcessorFrame)
new  =
  do
    cls' <- getRequiredClass "VTFrameProcessorFrame"
    sendOwnedClassMessage cls' newSelector

-- | Pixel buffer that you provided when you initialized the object.
--
-- ObjC selector: @- buffer@
buffer :: IsVTFrameProcessorFrame vtFrameProcessorFrame => vtFrameProcessorFrame -> IO (Ptr ())
buffer vtFrameProcessorFrame =
  sendMessage vtFrameProcessorFrame bufferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTFrameProcessorFrame)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTFrameProcessorFrame)
newSelector = mkSelector "new"

-- | @Selector@ for @buffer@
bufferSelector :: Selector '[] (Ptr ())
bufferSelector = mkSelector "buffer"

