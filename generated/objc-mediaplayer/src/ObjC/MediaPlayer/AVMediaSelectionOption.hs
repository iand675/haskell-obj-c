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
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Will create a language option from the AVMediaSelectionOption Returns nil if the AVMediaSelectionOption does not represent an Audible or Legible selection option.
--
-- ObjC selector: @- makeNowPlayingInfoLanguageOption@
makeNowPlayingInfoLanguageOption :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id MPNowPlayingInfoLanguageOption)
makeNowPlayingInfoLanguageOption avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "makeNowPlayingInfoLanguageOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeNowPlayingInfoLanguageOption@
makeNowPlayingInfoLanguageOptionSelector :: Selector
makeNowPlayingInfoLanguageOptionSelector = mkSelector "makeNowPlayingInfoLanguageOption"

