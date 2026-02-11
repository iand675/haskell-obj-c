{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerVideoOutputConfiguration
--
-- An AVPlayerVideoOutputConfiguration carries an identifier for the AVPlayerItem the configuration is associated with as well as presentation settings for that item.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerVideoOutputConfiguration@.
module ObjC.AVFoundation.AVPlayerVideoOutputConfiguration
  ( AVPlayerVideoOutputConfiguration
  , IsAVPlayerVideoOutputConfiguration(..)
  , init_
  , new
  , sourcePlayerItem
  , initSelector
  , newSelector
  , sourcePlayerItemSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerVideoOutputConfiguration avPlayerVideoOutputConfiguration => avPlayerVideoOutputConfiguration -> IO (Id AVPlayerVideoOutputConfiguration)
init_ avPlayerVideoOutputConfiguration  =
  sendMsg avPlayerVideoOutputConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerVideoOutputConfiguration)
new  =
  do
    cls' <- getRequiredClass "AVPlayerVideoOutputConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | sourcePlayerItem
--
-- The AVPlayerItem which is the source of this configuration.
--
-- This AVPlayerItem can be seen as the source of all samples this configuration vended alongside.
--
-- ObjC selector: @- sourcePlayerItem@
sourcePlayerItem :: IsAVPlayerVideoOutputConfiguration avPlayerVideoOutputConfiguration => avPlayerVideoOutputConfiguration -> IO (Id AVPlayerItem)
sourcePlayerItem avPlayerVideoOutputConfiguration  =
  sendMsg avPlayerVideoOutputConfiguration (mkSelector "sourcePlayerItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourcePlayerItem@
sourcePlayerItemSelector :: Selector
sourcePlayerItemSelector = mkSelector "sourcePlayerItem"

