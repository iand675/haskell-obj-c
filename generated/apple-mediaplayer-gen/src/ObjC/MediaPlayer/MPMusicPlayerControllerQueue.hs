{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerControllerQueue@.
module ObjC.MediaPlayer.MPMusicPlayerControllerQueue
  ( MPMusicPlayerControllerQueue
  , IsMPMusicPlayerControllerQueue(..)
  , new
  , init_
  , items
  , initSelector
  , itemsSelector
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
new :: IO (Id MPMusicPlayerControllerQueue)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerControllerQueue"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPMusicPlayerControllerQueue mpMusicPlayerControllerQueue => mpMusicPlayerControllerQueue -> IO (Id MPMusicPlayerControllerQueue)
init_ mpMusicPlayerControllerQueue =
  sendOwnedMessage mpMusicPlayerControllerQueue initSelector

-- | @- items@
items :: IsMPMusicPlayerControllerQueue mpMusicPlayerControllerQueue => mpMusicPlayerControllerQueue -> IO (Id NSArray)
items mpMusicPlayerControllerQueue =
  sendMessage mpMusicPlayerControllerQueue itemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPMusicPlayerControllerQueue)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPMusicPlayerControllerQueue)
initSelector = mkSelector "init"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

