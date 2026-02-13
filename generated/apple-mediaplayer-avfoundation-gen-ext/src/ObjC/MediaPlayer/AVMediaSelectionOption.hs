{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMediaSelectionOption represents a specific option for the presentation of media within a group of options.
--
-- Generated bindings for @AVMediaSelectionOption@.
module ObjC.MediaPlayer.AVMediaSelectionOption
  ( AVMediaSelectionOption
  , IsAVMediaSelectionOption(..)
  , makeNowPlayingInfoLanguageOption
  , makeNowPlayingInfoLanguageOptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AVFoundation.Internal.Classes

-- | Will create a language option from the AVMediaSelectionOption Returns nil if the AVMediaSelectionOption does not represent an Audible or Legible selection option.
--
-- ObjC selector: @- makeNowPlayingInfoLanguageOption@
makeNowPlayingInfoLanguageOption :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id MPNowPlayingInfoLanguageOption)
makeNowPlayingInfoLanguageOption avMediaSelectionOption =
  sendMessage avMediaSelectionOption makeNowPlayingInfoLanguageOptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeNowPlayingInfoLanguageOption@
makeNowPlayingInfoLanguageOptionSelector :: Selector '[] (Id MPNowPlayingInfoLanguageOption)
makeNowPlayingInfoLanguageOptionSelector = mkSelector "makeNowPlayingInfoLanguageOption"

