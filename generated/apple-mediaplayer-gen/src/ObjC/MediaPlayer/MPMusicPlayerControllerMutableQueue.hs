{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerControllerMutableQueue@.
module ObjC.MediaPlayer.MPMusicPlayerControllerMutableQueue
  ( MPMusicPlayerControllerMutableQueue
  , IsMPMusicPlayerControllerMutableQueue(..)
  , insertQueueDescriptor_afterItem
  , removeItem
  , insertQueueDescriptor_afterItemSelector
  , removeItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- insertQueueDescriptor:afterItem:@
insertQueueDescriptor_afterItem :: (IsMPMusicPlayerControllerMutableQueue mpMusicPlayerControllerMutableQueue, IsMPMusicPlayerQueueDescriptor queueDescriptor, IsMPMediaItem afterItem) => mpMusicPlayerControllerMutableQueue -> queueDescriptor -> afterItem -> IO ()
insertQueueDescriptor_afterItem mpMusicPlayerControllerMutableQueue queueDescriptor afterItem =
  sendMessage mpMusicPlayerControllerMutableQueue insertQueueDescriptor_afterItemSelector (toMPMusicPlayerQueueDescriptor queueDescriptor) (toMPMediaItem afterItem)

-- | @- removeItem:@
removeItem :: (IsMPMusicPlayerControllerMutableQueue mpMusicPlayerControllerMutableQueue, IsMPMediaItem item) => mpMusicPlayerControllerMutableQueue -> item -> IO ()
removeItem mpMusicPlayerControllerMutableQueue item =
  sendMessage mpMusicPlayerControllerMutableQueue removeItemSelector (toMPMediaItem item)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertQueueDescriptor:afterItem:@
insertQueueDescriptor_afterItemSelector :: Selector '[Id MPMusicPlayerQueueDescriptor, Id MPMediaItem] ()
insertQueueDescriptor_afterItemSelector = mkSelector "insertQueueDescriptor:afterItem:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector '[Id MPMediaItem] ()
removeItemSelector = mkSelector "removeItem:"

