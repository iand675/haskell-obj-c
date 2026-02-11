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
  , initSelector
  , newSelector
  , bufferSelector


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

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVTFrameProcessorFrame vtFrameProcessorFrame => vtFrameProcessorFrame -> IO (Id VTFrameProcessorFrame)
init_ vtFrameProcessorFrame  =
  sendMsg vtFrameProcessorFrame (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTFrameProcessorFrame)
new  =
  do
    cls' <- getRequiredClass "VTFrameProcessorFrame"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Pixel buffer that you provided when you initialized the object.
--
-- ObjC selector: @- buffer@
buffer :: IsVTFrameProcessorFrame vtFrameProcessorFrame => vtFrameProcessorFrame -> IO (Ptr ())
buffer vtFrameProcessorFrame  =
  fmap castPtr $ sendMsg vtFrameProcessorFrame (mkSelector "buffer") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @buffer@
bufferSelector :: Selector
bufferSelector = mkSelector "buffer"

