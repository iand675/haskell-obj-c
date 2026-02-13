{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaItemArtwork@.
module ObjC.MediaPlayer.MPMediaItemArtwork
  ( MPMediaItemArtwork
  , IsMPMediaItemArtwork(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMediaItemArtwork)
new  =
  do
    cls' <- getRequiredClass "MPMediaItemArtwork"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMediaItemArtwork mpMediaItemArtwork => mpMediaItemArtwork -> IO (Id MPMediaItemArtwork)
init_ mpMediaItemArtwork =
  sendOwnedMessage mpMediaItemArtwork initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMediaItemArtwork)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMediaItemArtwork)
initSelector = mkSelector "init"

