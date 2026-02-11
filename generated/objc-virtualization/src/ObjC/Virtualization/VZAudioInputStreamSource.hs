{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an audio input stream source.
--
-- An audio input stream source defines how audio input data for a guest is produced on the host system.
--
-- VZAudioInputStreamSource should not be instantiated directly. One of its subclasses should be used instead.
--
-- See: VZHostAudioInputStreamSource
--
-- Generated bindings for @VZAudioInputStreamSource@.
module ObjC.Virtualization.VZAudioInputStreamSource
  ( VZAudioInputStreamSource
  , IsVZAudioInputStreamSource(..)
  , new
  , init_
  , newSelector
  , initSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZAudioInputStreamSource)
new  =
  do
    cls' <- getRequiredClass "VZAudioInputStreamSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZAudioInputStreamSource vzAudioInputStreamSource => vzAudioInputStreamSource -> IO (Id VZAudioInputStreamSource)
init_ vzAudioInputStreamSource  =
  sendMsg vzAudioInputStreamSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

