{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMediaSelectionGroup provides a collection of mutually exclusive options for the presentation of media within an asset.
--
-- Generated bindings for @AVMediaSelectionGroup@.
module ObjC.MediaPlayer.AVMediaSelectionGroup
  ( AVMediaSelectionGroup
  , IsAVMediaSelectionGroup(..)
  , makeNowPlayingInfoLanguageOptionGroup
  , makeNowPlayingInfoLanguageOptionGroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AVFoundation.Internal.Classes

-- | Will create a language option group from the AVMediaSelectionGroup Any AVMediaSelectionOptions in the AVMediaSelectionGroup not representing Audible or Legible selection options will be ignored.
--
-- ObjC selector: @- makeNowPlayingInfoLanguageOptionGroup@
makeNowPlayingInfoLanguageOptionGroup :: IsAVMediaSelectionGroup avMediaSelectionGroup => avMediaSelectionGroup -> IO (Id MPNowPlayingInfoLanguageOptionGroup)
makeNowPlayingInfoLanguageOptionGroup avMediaSelectionGroup =
  sendMessage avMediaSelectionGroup makeNowPlayingInfoLanguageOptionGroupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeNowPlayingInfoLanguageOptionGroup@
makeNowPlayingInfoLanguageOptionGroupSelector :: Selector '[] (Id MPNowPlayingInfoLanguageOptionGroup)
makeNowPlayingInfoLanguageOptionGroupSelector = mkSelector "makeNowPlayingInfoLanguageOptionGroup"

