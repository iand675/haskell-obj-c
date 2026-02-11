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

-- | @- insertQueueDescriptor:afterItem:@
insertQueueDescriptor_afterItem :: (IsMPMusicPlayerControllerMutableQueue mpMusicPlayerControllerMutableQueue, IsMPMusicPlayerQueueDescriptor queueDescriptor, IsMPMediaItem afterItem) => mpMusicPlayerControllerMutableQueue -> queueDescriptor -> afterItem -> IO ()
insertQueueDescriptor_afterItem mpMusicPlayerControllerMutableQueue  queueDescriptor afterItem =
withObjCPtr queueDescriptor $ \raw_queueDescriptor ->
  withObjCPtr afterItem $ \raw_afterItem ->
      sendMsg mpMusicPlayerControllerMutableQueue (mkSelector "insertQueueDescriptor:afterItem:") retVoid [argPtr (castPtr raw_queueDescriptor :: Ptr ()), argPtr (castPtr raw_afterItem :: Ptr ())]

-- | @- removeItem:@
removeItem :: (IsMPMusicPlayerControllerMutableQueue mpMusicPlayerControllerMutableQueue, IsMPMediaItem item) => mpMusicPlayerControllerMutableQueue -> item -> IO ()
removeItem mpMusicPlayerControllerMutableQueue  item =
withObjCPtr item $ \raw_item ->
    sendMsg mpMusicPlayerControllerMutableQueue (mkSelector "removeItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertQueueDescriptor:afterItem:@
insertQueueDescriptor_afterItemSelector :: Selector
insertQueueDescriptor_afterItemSelector = mkSelector "insertQueueDescriptor:afterItem:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector
removeItemSelector = mkSelector "removeItem:"

