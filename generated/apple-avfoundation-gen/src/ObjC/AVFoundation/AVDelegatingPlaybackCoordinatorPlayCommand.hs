{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVDelegatingPlaybackCoordinatorPlayCommand avDelegatingPlaybackCoordinatorPlayCommand => avDelegatingPlaybackCoordinatorPlayCommand -> IO (Id AVDelegatingPlaybackCoordinatorPlayCommand)
init_ avDelegatingPlaybackCoordinatorPlayCommand =
  sendOwnedMessage avDelegatingPlaybackCoordinatorPlayCommand initSelector

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPlayCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPlayCommand"
    sendOwnedClassMessage cls' newSelector

-- | Playback rate. Will always be non-zero.
--
-- ObjC selector: @- rate@
rate :: IsAVDelegatingPlaybackCoordinatorPlayCommand avDelegatingPlaybackCoordinatorPlayCommand => avDelegatingPlaybackCoordinatorPlayCommand -> IO CFloat
rate avDelegatingPlaybackCoordinatorPlayCommand =
  sendMessage avDelegatingPlaybackCoordinatorPlayCommand rateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPlayCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPlayCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

