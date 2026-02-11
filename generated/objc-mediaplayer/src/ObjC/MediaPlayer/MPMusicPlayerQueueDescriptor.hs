{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerQueueDescriptor@.
module ObjC.MediaPlayer.MPMusicPlayerQueueDescriptor
  ( MPMusicPlayerQueueDescriptor
  , IsMPMusicPlayerQueueDescriptor(..)
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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMusicPlayerQueueDescriptor)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerQueueDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPMusicPlayerQueueDescriptor mpMusicPlayerQueueDescriptor => mpMusicPlayerQueueDescriptor -> IO (Id MPMusicPlayerQueueDescriptor)
init_ mpMusicPlayerQueueDescriptor  =
  sendMsg mpMusicPlayerQueueDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

