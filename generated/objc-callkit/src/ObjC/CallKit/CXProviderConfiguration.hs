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
  , initSelector
  , initWithLocalizedNameSelector
  , localizedNameSelector
  , ringtoneSoundSelector
  , setRingtoneSoundSelector
  , iconTemplateImageDataSelector
  , setIconTemplateImageDataSelector
  , maximumCallGroupsSelector
  , setMaximumCallGroupsSelector
  , maximumCallsPerCallGroupSelector
  , setMaximumCallsPerCallGroupSelector
  , includesCallsInRecentsSelector
  , setIncludesCallsInRecentsSelector
  , supportsVideoSelector
  , setSupportsVideoSelector
  , supportsAudioTranslationSelector
  , setSupportsAudioTranslationSelector
  , supportedHandleTypesSelector
  , setSupportedHandleTypesSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id CXProviderConfiguration)
init_ cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithLocalizedName:@
initWithLocalizedName :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSString localizedName) => cxProviderConfiguration -> localizedName -> IO (Id CXProviderConfiguration)
initWithLocalizedName cxProviderConfiguration  localizedName =
withObjCPtr localizedName $ \raw_localizedName ->
    sendMsg cxProviderConfiguration (mkSelector "initWithLocalizedName:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ())] >>= ownedObject . castPtr

-- | Localized name of the provider
--
-- ObjC selector: @- localizedName@
localizedName :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSString)
localizedName cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of resource in app's bundle to play as ringtone for incoming call
--
-- ObjC selector: @- ringtoneSound@
ringtoneSound :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSString)
ringtoneSound cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "ringtoneSound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of resource in app's bundle to play as ringtone for incoming call
--
-- ObjC selector: @- setRingtoneSound:@
setRingtoneSound :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSString value) => cxProviderConfiguration -> value -> IO ()
setRingtoneSound cxProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxProviderConfiguration (mkSelector "setRingtoneSound:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iconTemplateImageData@
iconTemplateImageData :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSData)
iconTemplateImageData cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "iconTemplateImageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIconTemplateImageData:@
setIconTemplateImageData :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSData value) => cxProviderConfiguration -> value -> IO ()
setIconTemplateImageData cxProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxProviderConfiguration (mkSelector "setIconTemplateImageData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumCallGroups@
maximumCallGroups :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO CULong
maximumCallGroups cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "maximumCallGroups") retCULong []

-- | @- setMaximumCallGroups:@
setMaximumCallGroups :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> CULong -> IO ()
setMaximumCallGroups cxProviderConfiguration  value =
  sendMsg cxProviderConfiguration (mkSelector "setMaximumCallGroups:") retVoid [argCULong (fromIntegral value)]

-- | @- maximumCallsPerCallGroup@
maximumCallsPerCallGroup :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO CULong
maximumCallsPerCallGroup cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "maximumCallsPerCallGroup") retCULong []

-- | @- setMaximumCallsPerCallGroup:@
setMaximumCallsPerCallGroup :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> CULong -> IO ()
setMaximumCallsPerCallGroup cxProviderConfiguration  value =
  sendMsg cxProviderConfiguration (mkSelector "setMaximumCallsPerCallGroup:") retVoid [argCULong (fromIntegral value)]

-- | Whether this provider's calls should be included in the system's Recents list at the end of each call. Default: YES
--
-- ObjC selector: @- includesCallsInRecents@
includesCallsInRecents :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
includesCallsInRecents cxProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxProviderConfiguration (mkSelector "includesCallsInRecents") retCULong []

-- | Whether this provider's calls should be included in the system's Recents list at the end of each call. Default: YES
--
-- ObjC selector: @- setIncludesCallsInRecents:@
setIncludesCallsInRecents :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setIncludesCallsInRecents cxProviderConfiguration  value =
  sendMsg cxProviderConfiguration (mkSelector "setIncludesCallsInRecents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- supportsVideo@
supportsVideo :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
supportsVideo cxProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxProviderConfiguration (mkSelector "supportsVideo") retCULong []

-- | @- setSupportsVideo:@
setSupportsVideo :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setSupportsVideo cxProviderConfiguration  value =
  sendMsg cxProviderConfiguration (mkSelector "setSupportsVideo:") retVoid [argCULong (if value then 1 else 0)]

-- | @- supportsAudioTranslation@
supportsAudioTranslation :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO Bool
supportsAudioTranslation cxProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxProviderConfiguration (mkSelector "supportsAudioTranslation") retCULong []

-- | @- setSupportsAudioTranslation:@
setSupportsAudioTranslation :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> Bool -> IO ()
setSupportsAudioTranslation cxProviderConfiguration  value =
  sendMsg cxProviderConfiguration (mkSelector "setSupportsAudioTranslation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- supportedHandleTypes@
supportedHandleTypes :: IsCXProviderConfiguration cxProviderConfiguration => cxProviderConfiguration -> IO (Id NSSet)
supportedHandleTypes cxProviderConfiguration  =
  sendMsg cxProviderConfiguration (mkSelector "supportedHandleTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedHandleTypes:@
setSupportedHandleTypes :: (IsCXProviderConfiguration cxProviderConfiguration, IsNSSet value) => cxProviderConfiguration -> value -> IO ()
setSupportedHandleTypes cxProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxProviderConfiguration (mkSelector "setSupportedHandleTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLocalizedName:@
initWithLocalizedNameSelector :: Selector
initWithLocalizedNameSelector = mkSelector "initWithLocalizedName:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @ringtoneSound@
ringtoneSoundSelector :: Selector
ringtoneSoundSelector = mkSelector "ringtoneSound"

-- | @Selector@ for @setRingtoneSound:@
setRingtoneSoundSelector :: Selector
setRingtoneSoundSelector = mkSelector "setRingtoneSound:"

-- | @Selector@ for @iconTemplateImageData@
iconTemplateImageDataSelector :: Selector
iconTemplateImageDataSelector = mkSelector "iconTemplateImageData"

-- | @Selector@ for @setIconTemplateImageData:@
setIconTemplateImageDataSelector :: Selector
setIconTemplateImageDataSelector = mkSelector "setIconTemplateImageData:"

-- | @Selector@ for @maximumCallGroups@
maximumCallGroupsSelector :: Selector
maximumCallGroupsSelector = mkSelector "maximumCallGroups"

-- | @Selector@ for @setMaximumCallGroups:@
setMaximumCallGroupsSelector :: Selector
setMaximumCallGroupsSelector = mkSelector "setMaximumCallGroups:"

-- | @Selector@ for @maximumCallsPerCallGroup@
maximumCallsPerCallGroupSelector :: Selector
maximumCallsPerCallGroupSelector = mkSelector "maximumCallsPerCallGroup"

-- | @Selector@ for @setMaximumCallsPerCallGroup:@
setMaximumCallsPerCallGroupSelector :: Selector
setMaximumCallsPerCallGroupSelector = mkSelector "setMaximumCallsPerCallGroup:"

-- | @Selector@ for @includesCallsInRecents@
includesCallsInRecentsSelector :: Selector
includesCallsInRecentsSelector = mkSelector "includesCallsInRecents"

-- | @Selector@ for @setIncludesCallsInRecents:@
setIncludesCallsInRecentsSelector :: Selector
setIncludesCallsInRecentsSelector = mkSelector "setIncludesCallsInRecents:"

-- | @Selector@ for @supportsVideo@
supportsVideoSelector :: Selector
supportsVideoSelector = mkSelector "supportsVideo"

-- | @Selector@ for @setSupportsVideo:@
setSupportsVideoSelector :: Selector
setSupportsVideoSelector = mkSelector "setSupportsVideo:"

-- | @Selector@ for @supportsAudioTranslation@
supportsAudioTranslationSelector :: Selector
supportsAudioTranslationSelector = mkSelector "supportsAudioTranslation"

-- | @Selector@ for @setSupportsAudioTranslation:@
setSupportsAudioTranslationSelector :: Selector
setSupportsAudioTranslationSelector = mkSelector "setSupportsAudioTranslation:"

-- | @Selector@ for @supportedHandleTypes@
supportedHandleTypesSelector :: Selector
supportedHandleTypesSelector = mkSelector "supportedHandleTypes"

-- | @Selector@ for @setSupportedHandleTypes:@
setSupportedHandleTypesSelector :: Selector
setSupportedHandleTypesSelector = mkSelector "setSupportedHandleTypes:"

