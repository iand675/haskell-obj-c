{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXProviderConfiguration@.
module ObjC.CallKit.CXProviderConfiguration
  ( CXProviderConfiguration
  , IsCXProviderConfiguration(..)
  , init_
  , initWithLocalizedName
  , localizedName
  , ringtoneSound
  , setRingtoneSound
  , iconTemplateImageData
  , setIconTemplateImageData
  , maximumCallGroups
  , setMaximumCallGroups
  , maximumCallsPerCallGroup
  , setMaximumCallsPerCallGroup
  , includesCallsInRecents
  , setIncludesCallsInRecents
  , supportsVideo
  , setSupportsVideo
  , supportsAudioTranslation
  , setSupportsAudioTranslation
  , supportedHandleTypes
  , setSupportedHandleTypes
  , iconTemplateImageDataSelector
  , includesCallsInRecentsSelector
  , initSelector
  , initWithLocalizedNameSelector
  , localizedNameSelector
  , maximumCallGroupsSelector
  , maximumCallsPerCallGroupSelector
  , ringtoneSoundSelector
  , setIconTemplateImageDataSelector
  , setIncludesCallsInRecentsSelector
  , setMaximumCallGroupsSelector
  , setMaximumCallsPerCallGroupSelector
  , setRingtoneSoundSelector
  , setSupportedHandleTypesSelector
  , setSupportsAudioTranslationSelector
  , setSupportsVideoSelector
  , supportedHandleTypesSelector
  , supportsAudioTranslationSelector
  , supportsVideoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id CXProviderConfiguration)
init_ cxProviderConfiguration =
  sendOwnedMessage cxProviderConfiguration initSelector

-- | @- initWithLocalizedName:@
initWithLocalizedName :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSString localizedName) => cxProviderConfiguration -> localizedName -> IO (Id CXProviderConfiguration)
initWithLocalizedName cxProviderConfiguration localizedName =
  sendOwnedMessage cxProviderConfiguration initWithLocalizedNameSelector (toNSString localizedName)

-- | Localized name of the provider
--
-- ObjC selector: @- localizedName@
localizedName :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSString)
localizedName cxProviderConfiguration =
  sendMessage cxProviderConfiguration localizedNameSelector

-- | Name of resource in app's bundle to play as ringtone for incoming call
--
-- ObjC selector: @- ringtoneSound@
ringtoneSound :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSString)
ringtoneSound cxProviderConfiguration =
  sendMessage cxProviderConfiguration ringtoneSoundSelector

-- | Name of resource in app's bundle to play as ringtone for incoming call
--
-- ObjC selector: @- setRingtoneSound:@
setRingtoneSound :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSString value) => cxProviderConfiguration -> value -> IO ()
setRingtoneSound cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setRingtoneSoundSelector (toNSString value)

-- | @- iconTemplateImageData@
iconTemplateImageData :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSData)
iconTemplateImageData cxProviderConfiguration =
  sendMessage cxProviderConfiguration iconTemplateImageDataSelector

-- | @- setIconTemplateImageData:@
setIconTemplateImageData :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSData value) => cxProviderConfiguration -> value -> IO ()
setIconTemplateImageData cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setIconTemplateImageDataSelector (toNSData value)

-- | @- maximumCallGroups@
maximumCallGroups :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO CULong
maximumCallGroups cxProviderConfiguration =
  sendMessage cxProviderConfiguration maximumCallGroupsSelector

-- | @- setMaximumCallGroups:@
setMaximumCallGroups :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> CULong -> IO ()
setMaximumCallGroups cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setMaximumCallGroupsSelector value

-- | @- maximumCallsPerCallGroup@
maximumCallsPerCallGroup :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO CULong
maximumCallsPerCallGroup cxProviderConfiguration =
  sendMessage cxProviderConfiguration maximumCallsPerCallGroupSelector

-- | @- setMaximumCallsPerCallGroup:@
setMaximumCallsPerCallGroup :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> CULong -> IO ()
setMaximumCallsPerCallGroup cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setMaximumCallsPerCallGroupSelector value

-- | Whether this provider's calls should be included in the system's Recents list at the end of each call. Default: YES
--
-- ObjC selector: @- includesCallsInRecents@
includesCallsInRecents :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
includesCallsInRecents cxProviderConfiguration =
  sendMessage cxProviderConfiguration includesCallsInRecentsSelector

-- | Whether this provider's calls should be included in the system's Recents list at the end of each call. Default: YES
--
-- ObjC selector: @- setIncludesCallsInRecents:@
setIncludesCallsInRecents :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setIncludesCallsInRecents cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setIncludesCallsInRecentsSelector value

-- | @- supportsVideo@
supportsVideo :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
supportsVideo cxProviderConfiguration =
  sendMessage cxProviderConfiguration supportsVideoSelector

-- | @- setSupportsVideo:@
setSupportsVideo :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setSupportsVideo cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setSupportsVideoSelector value

-- | @- supportsAudioTranslation@
supportsAudioTranslation :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
supportsAudioTranslation cxProviderConfiguration =
  sendMessage cxProviderConfiguration supportsAudioTranslationSelector

-- | @- setSupportsAudioTranslation:@
setSupportsAudioTranslation :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setSupportsAudioTranslation cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setSupportsAudioTranslationSelector value

-- | @- supportedHandleTypes@
supportedHandleTypes :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSSet)
supportedHandleTypes cxProviderConfiguration =
  sendMessage cxProviderConfiguration supportedHandleTypesSelector

-- | @- setSupportedHandleTypes:@
setSupportedHandleTypes :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSSet value) => cxProviderConfiguration -> value -> IO ()
setSupportedHandleTypes cxProviderConfiguration value =
  sendMessage cxProviderConfiguration setSupportedHandleTypesSelector (toNSSet value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXProviderConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLocalizedName:@
initWithLocalizedNameSelector :: Selector '[Id NSString] (Id CXProviderConfiguration)
initWithLocalizedNameSelector = mkSelector "initWithLocalizedName:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @ringtoneSound@
ringtoneSoundSelector :: Selector '[] (Id NSString)
ringtoneSoundSelector = mkSelector "ringtoneSound"

-- | @Selector@ for @setRingtoneSound:@
setRingtoneSoundSelector :: Selector '[Id NSString] ()
setRingtoneSoundSelector = mkSelector "setRingtoneSound:"

-- | @Selector@ for @iconTemplateImageData@
iconTemplateImageDataSelector :: Selector '[] (Id NSData)
iconTemplateImageDataSelector = mkSelector "iconTemplateImageData"

-- | @Selector@ for @setIconTemplateImageData:@
setIconTemplateImageDataSelector :: Selector '[Id NSData] ()
setIconTemplateImageDataSelector = mkSelector "setIconTemplateImageData:"

-- | @Selector@ for @maximumCallGroups@
maximumCallGroupsSelector :: Selector '[] CULong
maximumCallGroupsSelector = mkSelector "maximumCallGroups"

-- | @Selector@ for @setMaximumCallGroups:@
setMaximumCallGroupsSelector :: Selector '[CULong] ()
setMaximumCallGroupsSelector = mkSelector "setMaximumCallGroups:"

-- | @Selector@ for @maximumCallsPerCallGroup@
maximumCallsPerCallGroupSelector :: Selector '[] CULong
maximumCallsPerCallGroupSelector = mkSelector "maximumCallsPerCallGroup"

-- | @Selector@ for @setMaximumCallsPerCallGroup:@
setMaximumCallsPerCallGroupSelector :: Selector '[CULong] ()
setMaximumCallsPerCallGroupSelector = mkSelector "setMaximumCallsPerCallGroup:"

-- | @Selector@ for @includesCallsInRecents@
includesCallsInRecentsSelector :: Selector '[] Bool
includesCallsInRecentsSelector = mkSelector "includesCallsInRecents"

-- | @Selector@ for @setIncludesCallsInRecents:@
setIncludesCallsInRecentsSelector :: Selector '[Bool] ()
setIncludesCallsInRecentsSelector = mkSelector "setIncludesCallsInRecents:"

-- | @Selector@ for @supportsVideo@
supportsVideoSelector :: Selector '[] Bool
supportsVideoSelector = mkSelector "supportsVideo"

-- | @Selector@ for @setSupportsVideo:@
setSupportsVideoSelector :: Selector '[Bool] ()
setSupportsVideoSelector = mkSelector "setSupportsVideo:"

-- | @Selector@ for @supportsAudioTranslation@
supportsAudioTranslationSelector :: Selector '[] Bool
supportsAudioTranslationSelector = mkSelector "supportsAudioTranslation"

-- | @Selector@ for @setSupportsAudioTranslation:@
setSupportsAudioTranslationSelector :: Selector '[Bool] ()
setSupportsAudioTranslationSelector = mkSelector "setSupportsAudioTranslation:"

-- | @Selector@ for @supportedHandleTypes@
supportedHandleTypesSelector :: Selector '[] (Id NSSet)
supportedHandleTypesSelector = mkSelector "supportedHandleTypes"

-- | @Selector@ for @setSupportedHandleTypes:@
setSupportedHandleTypesSelector :: Selector '[Id NSSet] ()
setSupportedHandleTypesSelector = mkSelector "setSupportedHandleTypes:"

