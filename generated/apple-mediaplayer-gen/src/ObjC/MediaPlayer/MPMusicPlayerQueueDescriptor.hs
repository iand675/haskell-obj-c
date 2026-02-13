{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerQueueDescriptor@.
module ObjC.MediaPlayer.MPMusicPlayerQueueDescriptor
  ( MPMusicPlayerQueueDescriptor
  , IsMPMusicPlayerQueueDescriptor(..)
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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MPMusicPlayerQueueDescriptor)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerQueueDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMusicPlayerQueueDescriptor mpMusicPlayerQueueDescriptor => mpMusicPlayerQueueDescriptor -> IO (Id MPMusicPlayerQueueDescriptor)
init_ mpMusicPlayerQueueDescriptor =
  sendOwnedMessage mpMusicPlayerQueueDescriptor initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMusicPlayerQueueDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMusicPlayerQueueDescriptor)
initSelector = mkSelector "init"

