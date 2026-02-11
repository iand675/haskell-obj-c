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
  , synchronizedLayerWithPlayerItemSelector
  , playerItemSelector
  , setPlayerItemSelector


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
    withObjCPtr playerItem $ \raw_playerItem ->
      sendClassMsg cls' (mkSelector "synchronizedLayerWithPlayerItem:") (retPtr retVoid) [argPtr (castPtr raw_playerItem :: Ptr ())] >>= retainedObject . castPtr

-- | playerItem
--
-- Indicates the instance of AVPlayerItem to which the timing of the AVSynchronizedLayer is synchronized.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- playerItem@
playerItem :: IsAVSynchronizedLayer avSynchronizedLayer => avSynchronizedLayer -> IO (Id AVPlayerItem)
playerItem avSynchronizedLayer  =
  sendMsg avSynchronizedLayer (mkSelector "playerItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | playerItem
--
-- Indicates the instance of AVPlayerItem to which the timing of the AVSynchronizedLayer is synchronized.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setPlayerItem:@
setPlayerItem :: (IsAVSynchronizedLayer avSynchronizedLayer, IsAVPlayerItem value) => avSynchronizedLayer -> value -> IO ()
setPlayerItem avSynchronizedLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSynchronizedLayer (mkSelector "setPlayerItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @synchronizedLayerWithPlayerItem:@
synchronizedLayerWithPlayerItemSelector :: Selector
synchronizedLayerWithPlayerItemSelector = mkSelector "synchronizedLayerWithPlayerItem:"

-- | @Selector@ for @playerItem@
playerItemSelector :: Selector
playerItemSelector = mkSelector "playerItem"

-- | @Selector@ for @setPlayerItem:@
setPlayerItemSelector :: Selector
setPlayerItemSelector = mkSelector "setPlayerItem:"

