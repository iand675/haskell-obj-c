{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPNowPlayingInfoLanguageOptionGroup@.
module ObjC.MediaPlayer.MPNowPlayingInfoLanguageOptionGroup
  ( MPNowPlayingInfoLanguageOptionGroup
  , IsMPNowPlayingInfoLanguageOptionGroup(..)
  , initWithLanguageOptions_defaultLanguageOption_allowEmptySelection
  , languageOptions
  , defaultLanguageOption
  , allowEmptySelection
  , allowEmptySelectionSelector
  , defaultLanguageOptionSelector
  , initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector
  , languageOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:@
initWithLanguageOptions_defaultLanguageOption_allowEmptySelection :: (IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup, IsNSArray languageOptions, IsMPNowPlayingInfoLanguageOption defaultLanguageOption) => mpNowPlayingInfoLanguageOptionGroup -> languageOptions -> defaultLanguageOption -> Bool -> IO (Id MPNowPlayingInfoLanguageOptionGroup)
initWithLanguageOptions_defaultLanguageOption_allowEmptySelection mpNowPlayingInfoLanguageOptionGroup languageOptions defaultLanguageOption allowEmptySelection =
  sendOwnedMessage mpNowPlayingInfoLanguageOptionGroup initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector (toNSArray languageOptions) (toMPNowPlayingInfoLanguageOption defaultLanguageOption) allowEmptySelection

-- | The available language options within this group.
--
-- ObjC selector: @- languageOptions@
languageOptions :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO (Id NSArray)
languageOptions mpNowPlayingInfoLanguageOptionGroup =
  sendMessage mpNowPlayingInfoLanguageOptionGroup languageOptionsSelector

-- | The default language option, if any, within this group.
--
-- ObjC selector: @- defaultLanguageOption@
defaultLanguageOption :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO (Id MPNowPlayingInfoLanguageOption)
defaultLanguageOption mpNowPlayingInfoLanguageOptionGroup =
  sendMessage mpNowPlayingInfoLanguageOptionGroup defaultLanguageOptionSelector

-- | Indicates whether a selection in this group is required at all times.
--
-- ObjC selector: @- allowEmptySelection@
allowEmptySelection :: IsMPNowPlayingInfoLanguageOptionGroup mpNowPlayingInfoLanguageOptionGroup => mpNowPlayingInfoLanguageOptionGroup -> IO Bool
allowEmptySelection mpNowPlayingInfoLanguageOptionGroup =
  sendMessage mpNowPlayingInfoLanguageOptionGroup allowEmptySelectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:@
initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector :: Selector '[Id NSArray, Id MPNowPlayingInfoLanguageOption, Bool] (Id MPNowPlayingInfoLanguageOptionGroup)
initWithLanguageOptions_defaultLanguageOption_allowEmptySelectionSelector = mkSelector "initWithLanguageOptions:defaultLanguageOption:allowEmptySelection:"

-- | @Selector@ for @languageOptions@
languageOptionsSelector :: Selector '[] (Id NSArray)
languageOptionsSelector = mkSelector "languageOptions"

-- | @Selector@ for @defaultLanguageOption@
defaultLanguageOptionSelector :: Selector '[] (Id MPNowPlayingInfoLanguageOption)
defaultLanguageOptionSelector = mkSelector "defaultLanguageOption"

-- | @Selector@ for @allowEmptySelection@
allowEmptySelectionSelector :: Selector '[] Bool
allowEmptySelectionSelector = mkSelector "allowEmptySelection"

