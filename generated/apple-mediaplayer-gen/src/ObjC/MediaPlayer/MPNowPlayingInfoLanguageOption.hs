{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a single language option option.
--
-- Generated bindings for @MPNowPlayingInfoLanguageOption@.
module ObjC.MediaPlayer.MPNowPlayingInfoLanguageOption
  ( MPNowPlayingInfoLanguageOption
  , IsMPNowPlayingInfoLanguageOption(..)
  , initWithType_languageTag_characteristics_displayName_identifier
  , isAutomaticLegibleLanguageOption
  , isAutomaticAudibleLanguageOption
  , languageOptionType
  , languageTag
  , languageOptionCharacteristics
  , displayName
  , identifier
  , displayNameSelector
  , identifierSelector
  , initWithType_languageTag_characteristics_displayName_identifierSelector
  , isAutomaticAudibleLanguageOptionSelector
  , isAutomaticLegibleLanguageOptionSelector
  , languageOptionCharacteristicsSelector
  , languageOptionTypeSelector
  , languageTagSelector

  -- * Enum types
  , MPNowPlayingInfoLanguageOptionType(MPNowPlayingInfoLanguageOptionType)
  , pattern MPNowPlayingInfoLanguageOptionTypeAudible
  , pattern MPNowPlayingInfoLanguageOptionTypeLegible

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithType:languageTag:characteristics:displayName:identifier:@
initWithType_languageTag_characteristics_displayName_identifier :: (IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption, IsNSString languageTag, IsNSArray languageOptionCharacteristics, IsNSString displayName, IsNSString identifier) => mpNowPlayingInfoLanguageOption -> MPNowPlayingInfoLanguageOptionType -> languageTag -> languageOptionCharacteristics -> displayName -> identifier -> IO (Id MPNowPlayingInfoLanguageOption)
initWithType_languageTag_characteristics_displayName_identifier mpNowPlayingInfoLanguageOption languageOptionType languageTag languageOptionCharacteristics displayName identifier =
  sendOwnedMessage mpNowPlayingInfoLanguageOption initWithType_languageTag_characteristics_displayName_identifierSelector languageOptionType (toNSString languageTag) (toNSArray languageOptionCharacteristics) (toNSString displayName) (toNSString identifier)

-- | Represents a special case that is used to represent the best legible language option based on system preferences. See AVPlayerItem-selectMediaOptionAutomaticallyInMediaSelectionGroup
--
-- ObjC selector: @- isAutomaticLegibleLanguageOption@
isAutomaticLegibleLanguageOption :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO Bool
isAutomaticLegibleLanguageOption mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption isAutomaticLegibleLanguageOptionSelector

-- | Represents a special case that is used to represent the best audible language option based on system preferences. See AVPlayerItem-selectMediaOptionAutomaticallyInMediaSelectionGroup
--
-- ObjC selector: @- isAutomaticAudibleLanguageOption@
isAutomaticAudibleLanguageOption :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO Bool
isAutomaticAudibleLanguageOption mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption isAutomaticAudibleLanguageOptionSelector

-- | The type of language option.
--
-- ObjC selector: @- languageOptionType@
languageOptionType :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO MPNowPlayingInfoLanguageOptionType
languageOptionType mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption languageOptionTypeSelector

-- | The IETF BCP 47 language tag. A nil languageTag reprsents that this option should be disabled. A languageTag with the value of MPLangaugeOptionAutoLangaugeTag represents that the best langauge based on the system preferences should be used.
--
-- ObjC selector: @- languageTag@
languageTag :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
languageTag mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption languageTagSelector

-- | Characteristics describing the content of the language options. See the LanguageOptionCharacteristics for the most commonly used values.
--
-- ObjC selector: @- languageOptionCharacteristics@
languageOptionCharacteristics :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSArray)
languageOptionCharacteristics mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption languageOptionCharacteristicsSelector

-- | A user presentable display name for this option.
--
-- ObjC selector: @- displayName@
displayName :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
displayName mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption displayNameSelector

-- | A unique identifier representing this option.
--
-- ObjC selector: @- identifier@
identifier :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
identifier mpNowPlayingInfoLanguageOption =
  sendMessage mpNowPlayingInfoLanguageOption identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:languageTag:characteristics:displayName:identifier:@
initWithType_languageTag_characteristics_displayName_identifierSelector :: Selector '[MPNowPlayingInfoLanguageOptionType, Id NSString, Id NSArray, Id NSString, Id NSString] (Id MPNowPlayingInfoLanguageOption)
initWithType_languageTag_characteristics_displayName_identifierSelector = mkSelector "initWithType:languageTag:characteristics:displayName:identifier:"

-- | @Selector@ for @isAutomaticLegibleLanguageOption@
isAutomaticLegibleLanguageOptionSelector :: Selector '[] Bool
isAutomaticLegibleLanguageOptionSelector = mkSelector "isAutomaticLegibleLanguageOption"

-- | @Selector@ for @isAutomaticAudibleLanguageOption@
isAutomaticAudibleLanguageOptionSelector :: Selector '[] Bool
isAutomaticAudibleLanguageOptionSelector = mkSelector "isAutomaticAudibleLanguageOption"

-- | @Selector@ for @languageOptionType@
languageOptionTypeSelector :: Selector '[] MPNowPlayingInfoLanguageOptionType
languageOptionTypeSelector = mkSelector "languageOptionType"

-- | @Selector@ for @languageTag@
languageTagSelector :: Selector '[] (Id NSString)
languageTagSelector = mkSelector "languageTag"

-- | @Selector@ for @languageOptionCharacteristics@
languageOptionCharacteristicsSelector :: Selector '[] (Id NSArray)
languageOptionCharacteristicsSelector = mkSelector "languageOptionCharacteristics"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

