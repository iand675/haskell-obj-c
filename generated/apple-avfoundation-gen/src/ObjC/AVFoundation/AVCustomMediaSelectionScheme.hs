{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVCustomMediaSelectionScheme provides a collection of custom settings for controlling the presentation of the media.
--
-- Each selectable setting is associated with a media characteristic that one or more of the AVMediaSelectionOptions in the AVMediaSelectionGroup possesses. By selecting a setting in a user interface based on an AVCustomMediaSelectionScheme, users are essentially indicating a preference for the media characteristic of the selected setting. Selection of a specific AVMediaSelectionOption in the AVMediaSelectionGroup is then derived from the user's indicated preferences. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCustomMediaSelectionScheme@.
module ObjC.AVFoundation.AVCustomMediaSelectionScheme
  ( AVCustomMediaSelectionScheme
  , IsAVCustomMediaSelectionScheme(..)
  , mediaPresentationSettingsForSelector_complementaryToLanguage_settings
  , shouldOfferLanguageSelection
  , availableLanguages
  , selectors
  , availableLanguagesSelector
  , mediaPresentationSettingsForSelector_complementaryToLanguage_settingsSelector
  , selectorsSelector
  , shouldOfferLanguageSelectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides an array of media presentation settings that can be effective at the same time as the specified language and settings for other selectors of the receiver.
--
-- If the content is authored to provide a collection of AVMediaSelectionOptions that include one or more with all of the combinations of media characteristics of the specified AVMediaPresentationSettings together with all of the settings of the specified AVMediaPresentationSelector, this method will return all of the settings for that selector. However, if one or more of the available combinations are not possessed by any of the AVMediaSelectionOptions, it will return fewer.
--
-- - Parameter selector: The AVMediaPresentationSelector for which complementary settings are requested. - Parameter language: A BCP 47 language tag chosen among the availableLanguages of the receiver. If no language setting pertains, can be nil. - Parameter settings: A collection of AVMediaPresentationSettings provided by selectors of the receiver other than the specified selector. Because no two AVMediaPresentationSettings of the same AVMediaPresentationSelector are complementary, an empty array will be returned if you specify more than one setting for any selector.
--
-- ObjC selector: @- mediaPresentationSettingsForSelector:complementaryToLanguage:settings:@
mediaPresentationSettingsForSelector_complementaryToLanguage_settings :: (IsAVCustomMediaSelectionScheme avCustomMediaSelectionScheme, IsAVMediaPresentationSelector selector, IsNSString language, IsNSArray settings) => avCustomMediaSelectionScheme -> selector -> language -> settings -> IO (Id NSArray)
mediaPresentationSettingsForSelector_complementaryToLanguage_settings avCustomMediaSelectionScheme selector language settings =
  sendMessage avCustomMediaSelectionScheme mediaPresentationSettingsForSelector_complementaryToLanguage_settingsSelector (toAVMediaPresentationSelector selector) (toNSString language) (toNSArray settings)

-- | Indicates whether an alternative selection interface should provide a menu of language choices.
--
-- ObjC selector: @- shouldOfferLanguageSelection@
shouldOfferLanguageSelection :: IsAVCustomMediaSelectionScheme avCustomMediaSelectionScheme => avCustomMediaSelectionScheme -> IO Bool
shouldOfferLanguageSelection avCustomMediaSelectionScheme =
  sendMessage avCustomMediaSelectionScheme shouldOfferLanguageSelectionSelector

-- | Provides available language choices.
--
-- Each string in the array is intended to be interpreted as a BCP 47 language tag.
--
-- ObjC selector: @- availableLanguages@
availableLanguages :: IsAVCustomMediaSelectionScheme avCustomMediaSelectionScheme => avCustomMediaSelectionScheme -> IO (Id NSArray)
availableLanguages avCustomMediaSelectionScheme =
  sendMessage avCustomMediaSelectionScheme availableLanguagesSelector

-- | Provides custom settings.
--
-- ObjC selector: @- selectors@
selectors :: IsAVCustomMediaSelectionScheme avCustomMediaSelectionScheme => avCustomMediaSelectionScheme -> IO (Id NSArray)
selectors avCustomMediaSelectionScheme =
  sendMessage avCustomMediaSelectionScheme selectorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaPresentationSettingsForSelector:complementaryToLanguage:settings:@
mediaPresentationSettingsForSelector_complementaryToLanguage_settingsSelector :: Selector '[Id AVMediaPresentationSelector, Id NSString, Id NSArray] (Id NSArray)
mediaPresentationSettingsForSelector_complementaryToLanguage_settingsSelector = mkSelector "mediaPresentationSettingsForSelector:complementaryToLanguage:settings:"

-- | @Selector@ for @shouldOfferLanguageSelection@
shouldOfferLanguageSelectionSelector :: Selector '[] Bool
shouldOfferLanguageSelectionSelector = mkSelector "shouldOfferLanguageSelection"

-- | @Selector@ for @availableLanguages@
availableLanguagesSelector :: Selector '[] (Id NSArray)
availableLanguagesSelector = mkSelector "availableLanguages"

-- | @Selector@ for @selectors@
selectorsSelector :: Selector '[] (Id NSArray)
selectorsSelector = mkSelector "selectors"

