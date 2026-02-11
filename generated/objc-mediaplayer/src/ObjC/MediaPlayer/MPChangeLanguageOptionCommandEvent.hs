{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangeLanguageOptionCommandEvent@.
module ObjC.MediaPlayer.MPChangeLanguageOptionCommandEvent
  ( MPChangeLanguageOptionCommandEvent
  , IsMPChangeLanguageOptionCommandEvent(..)
  , languageOption
  , setting
  , languageOptionSelector
  , settingSelector

  -- * Enum types
  , MPChangeLanguageOptionSetting(MPChangeLanguageOptionSetting)
  , pattern MPChangeLanguageOptionSettingNone
  , pattern MPChangeLanguageOptionSettingNowPlayingItemOnly
  , pattern MPChangeLanguageOptionSettingPermanent

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
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The requested language option to change. The supplied language option may be the Automatic Legible Language Option which would mean that best legible language option based on user preferences is being requested. See MPNowPlayingInfoLanguageOption isAutomaticLegibleLanguageOption
--
-- ObjC selector: @- languageOption@
languageOption :: IsMPChangeLanguageOptionCommandEvent mpChangeLanguageOptionCommandEvent => mpChangeLanguageOptionCommandEvent -> IO (Id MPNowPlayingInfoLanguageOption)
languageOption mpChangeLanguageOptionCommandEvent  =
  sendMsg mpChangeLanguageOptionCommandEvent (mkSelector "languageOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes the extent of the changed language option
--
-- ObjC selector: @- setting@
setting :: IsMPChangeLanguageOptionCommandEvent mpChangeLanguageOptionCommandEvent => mpChangeLanguageOptionCommandEvent -> IO MPChangeLanguageOptionSetting
setting mpChangeLanguageOptionCommandEvent  =
  fmap (coerce :: CLong -> MPChangeLanguageOptionSetting) $ sendMsg mpChangeLanguageOptionCommandEvent (mkSelector "setting") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageOption@
languageOptionSelector :: Selector
languageOptionSelector = mkSelector "languageOption"

-- | @Selector@ for @setting@
settingSelector :: Selector
settingSelector = mkSelector "setting"

