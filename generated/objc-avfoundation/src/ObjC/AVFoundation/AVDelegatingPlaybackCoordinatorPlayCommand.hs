{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playback command requesting playback with specific timing.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorPlayCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorPlayCommand
  ( AVDelegatingPlaybackCoordinatorPlayCommand
  , IsAVDelegatingPlaybackCoordinatorPlayCommand(..)
  , init_
  , new
  , rate
  , initSelector
  , newSelector
  , rateSelector


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

-- | @- init@
init_ :: IsAVDelegatingPlaybackCoordinatorPlayCommand avDelegatingPlaybackCoordinatorPlayCommand => avDelegatingPlaybackCoordinatorPlayCommand -> IO (Id AVDelegatingPlaybackCoordinatorPlayCommand)
init_ avDelegatingPlaybackCoordinatorPlayCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPlayCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPlayCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPlayCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Playback rate. Will always be non-zero.
--
-- ObjC selector: @- rate@
rate :: IsAVDelegatingPlaybackCoordinatorPlayCommand avDelegatingPlaybackCoordinatorPlayCommand => avDelegatingPlaybackCoordinatorPlayCommand -> IO CFloat
rate avDelegatingPlaybackCoordinatorPlayCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPlayCommand (mkSelector "rate") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

