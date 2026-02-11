{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_languageTag_characteristics_displayName_identifierSelector
  , isAutomaticLegibleLanguageOptionSelector
  , isAutomaticAudibleLanguageOptionSelector
  , languageOptionTypeSelector
  , languageTagSelector
  , languageOptionCharacteristicsSelector
  , displayNameSelector
  , identifierSelector

  -- * Enum types
  , MPNowPlayingInfoLanguageOptionType(MPNowPlayingInfoLanguageOptionType)
  , pattern MPNowPlayingInfoLanguageOptionTypeAudible
  , pattern MPNowPlayingInfoLanguageOptionTypeLegible

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

-- | @- initWithType:languageTag:characteristics:displayName:identifier:@
initWithType_languageTag_characteristics_displayName_identifier :: (IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption, IsNSString languageTag, IsNSArray languageOptionCharacteristics, IsNSString displayName, IsNSString identifier) => mpNowPlayingInfoLanguageOption -> MPNowPlayingInfoLanguageOptionType -> languageTag -> languageOptionCharacteristics -> displayName -> identifier -> IO (Id MPNowPlayingInfoLanguageOption)
initWithType_languageTag_characteristics_displayName_identifier mpNowPlayingInfoLanguageOption  languageOptionType languageTag languageOptionCharacteristics displayName identifier =
withObjCPtr languageTag $ \raw_languageTag ->
  withObjCPtr languageOptionCharacteristics $ \raw_languageOptionCharacteristics ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr identifier $ \raw_identifier ->
          sendMsg mpNowPlayingInfoLanguageOption (mkSelector "initWithType:languageTag:characteristics:displayName:identifier:") (retPtr retVoid) [argCULong (coerce languageOptionType), argPtr (castPtr raw_languageTag :: Ptr ()), argPtr (castPtr raw_languageOptionCharacteristics :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Represents a special case that is used to represent the best legible language option based on system preferences. See AVPlayerItem-selectMediaOptionAutomaticallyInMediaSelectionGroup
--
-- ObjC selector: @- isAutomaticLegibleLanguageOption@
isAutomaticLegibleLanguageOption :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO Bool
isAutomaticLegibleLanguageOption mpNowPlayingInfoLanguageOption  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingInfoLanguageOption (mkSelector "isAutomaticLegibleLanguageOption") retCULong []

-- | Represents a special case that is used to represent the best audible language option based on system preferences. See AVPlayerItem-selectMediaOptionAutomaticallyInMediaSelectionGroup
--
-- ObjC selector: @- isAutomaticAudibleLanguageOption@
isAutomaticAudibleLanguageOption :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO Bool
isAutomaticAudibleLanguageOption mpNowPlayingInfoLanguageOption  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpNowPlayingInfoLanguageOption (mkSelector "isAutomaticAudibleLanguageOption") retCULong []

-- | The type of language option.
--
-- ObjC selector: @- languageOptionType@
languageOptionType :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO MPNowPlayingInfoLanguageOptionType
languageOptionType mpNowPlayingInfoLanguageOption  =
  fmap (coerce :: CULong -> MPNowPlayingInfoLanguageOptionType) $ sendMsg mpNowPlayingInfoLanguageOption (mkSelector "languageOptionType") retCULong []

-- | The IETF BCP 47 language tag. A nil languageTag reprsents that this option should be disabled. A languageTag with the value of MPLangaugeOptionAutoLangaugeTag represents that the best langauge based on the system preferences should be used.
--
-- ObjC selector: @- languageTag@
languageTag :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
languageTag mpNowPlayingInfoLanguageOption  =
  sendMsg mpNowPlayingInfoLanguageOption (mkSelector "languageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Characteristics describing the content of the language options. See the LanguageOptionCharacteristics for the most commonly used values.
--
-- ObjC selector: @- languageOptionCharacteristics@
languageOptionCharacteristics :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSArray)
languageOptionCharacteristics mpNowPlayingInfoLanguageOption  =
  sendMsg mpNowPlayingInfoLanguageOption (mkSelector "languageOptionCharacteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A user presentable display name for this option.
--
-- ObjC selector: @- displayName@
displayName :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
displayName mpNowPlayingInfoLanguageOption  =
  sendMsg mpNowPlayingInfoLanguageOption (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier representing this option.
--
-- ObjC selector: @- identifier@
identifier :: IsMPNowPlayingInfoLanguageOption mpNowPlayingInfoLanguageOption => mpNowPlayingInfoLanguageOption -> IO (Id NSString)
identifier mpNowPlayingInfoLanguageOption  =
  sendMsg mpNowPlayingInfoLanguageOption (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:languageTag:characteristics:displayName:identifier:@
initWithType_languageTag_characteristics_displayName_identifierSelector :: Selector
initWithType_languageTag_characteristics_displayName_identifierSelector = mkSelector "initWithType:languageTag:characteristics:displayName:identifier:"

-- | @Selector@ for @isAutomaticLegibleLanguageOption@
isAutomaticLegibleLanguageOptionSelector :: Selector
isAutomaticLegibleLanguageOptionSelector = mkSelector "isAutomaticLegibleLanguageOption"

-- | @Selector@ for @isAutomaticAudibleLanguageOption@
isAutomaticAudibleLanguageOptionSelector :: Selector
isAutomaticAudibleLanguageOptionSelector = mkSelector "isAutomaticAudibleLanguageOption"

-- | @Selector@ for @languageOptionType@
languageOptionTypeSelector :: Selector
languageOptionTypeSelector = mkSelector "languageOptionType"

-- | @Selector@ for @languageTag@
languageTagSelector :: Selector
languageTagSelector = mkSelector "languageTag"

-- | @Selector@ for @languageOptionCharacteristics@
languageOptionCharacteristicsSelector :: Selector
languageOptionCharacteristicsSelector = mkSelector "languageOptionCharacteristics"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

