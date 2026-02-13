{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSynchronizedLayer@.
module ObjC.AVFoundation.AVSynchronizedLayer
  ( AVSynchronizedLayer
  , IsAVSynchronizedLayer(..)
  , synchronizedLayerWithPlayerItem
  , playerItem
  , setPlayerItem
  , playerItemSelector
  , setPlayerItemSelector
  , synchronizedLayerWithPlayerItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | synchronizedLayerWithPlayerItem:
--
-- Returns an instance of AVSynchronizedLayer with timing synchronized with the specified AVPlayerItem.
--
-- Returns: An instance of AVSynchronizedLayer.
--
-- ObjC selector: @+ synchronizedLayerWithPlayerItem:@
synchronizedLayerWithPlayerItem :: IsAVPlayerItem playerItem => playerItem -> IO (Id AVSynchronizedLayer)
synchronizedLayerWithPlayerItem playerItem =
  do
    cls' <- getRequiredClass "AVSynchronizedLayer"
    sendClassMessage cls' synchronizedLayerWithPlayerItemSelector (toAVPlayerItem playerItem)

-- | playerItem
--
-- Indicates the instance of AVPlayerItem to which the timing of the AVSynchronizedLayer is synchronized.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- playerItem@
playerItem :: IsAVSynchronizedLayer avSynchronizedLayer => avSynchronizedLayer -> IO (Id AVPlayerItem)
playerItem avSynchronizedLayer =
  sendMessage avSynchronizedLayer playerItemSelector

-- | playerItem
--
-- Indicates the instance of AVPlayerItem to which the timing of the AVSynchronizedLayer is synchronized.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setPlayerItem:@
setPlayerItem :: (IsAVSynchronizedLayer avSynchronizedLayer, IsAVPlayerItem value) => avSynchronizedLayer -> value -> IO ()
setPlayerItem avSynchronizedLayer value =
  sendMessage avSynchronizedLayer setPlayerItemSelector (toAVPlayerItem value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @synchronizedLayerWithPlayerItem:@
synchronizedLayerWithPlayerItemSelector :: Selector '[Id AVPlayerItem] (Id AVSynchronizedLayer)
synchronizedLayerWithPlayerItemSelector = mkSelector "synchronizedLayerWithPlayerItem:"

-- | @Selector@ for @playerItem@
playerItemSelector :: Selector '[] (Id AVPlayerItem)
playerItemSelector = mkSelector "playerItem"

-- | @Selector@ for @setPlayerItem:@
setPlayerItemSelector :: Selector '[Id AVPlayerItem] ()
setPlayerItemSelector = mkSelector "setPlayerItem:"

