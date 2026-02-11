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
  , newSelector
  , initSelector
  , itemsSelector


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
new :: IO (Id MPMusicPlayerControllerQueue)
new  =
  do
    cls' <- getRequiredClass "MPMusicPlayerControllerQueue"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPMusicPlayerControllerQueue mpMusicPlayerControllerQueue => mpMusicPlayerControllerQueue -> IO (Id MPMusicPlayerControllerQueue)
init_ mpMusicPlayerControllerQueue  =
  sendMsg mpMusicPlayerControllerQueue (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- items@
items :: IsMPMusicPlayerControllerQueue mpMusicPlayerControllerQueue => mpMusicPlayerControllerQueue -> IO (Id NSArray)
items mpMusicPlayerControllerQueue  =
  sendMsg mpMusicPlayerControllerQueue (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

