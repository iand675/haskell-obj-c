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

-- | Will create a language option group from the AVMediaSelectionGroup Any AVMediaSelectionOptions in the AVMediaSelectionGroup not representing Audible or Legible selection options will be ignored.
--
-- ObjC selector: @- makeNowPlayingInfoLanguageOptionGroup@
makeNowPlayingInfoLanguageOptionGroup :: IsAVMediaSelectionGroup avMediaSelectionGroup => avMediaSelectionGroup -> IO (Id MPNowPlayingInfoLanguageOptionGroup)
makeNowPlayingInfoLanguageOptionGroup avMediaSelectionGroup  =
  sendMsg avMediaSelectionGroup (mkSelector "makeNowPlayingInfoLanguageOptionGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeNowPlayingInfoLanguageOptionGroup@
makeNowPlayingInfoLanguageOptionGroupSelector :: Selector
makeNowPlayingInfoLanguageOptionGroupSelector = mkSelector "makeNowPlayingInfoLanguageOptionGroup"

