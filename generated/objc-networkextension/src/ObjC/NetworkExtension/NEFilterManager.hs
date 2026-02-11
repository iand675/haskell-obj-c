{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterManager
--
-- The NEFilterManager class declares the programmatic interface for an object that manages content filtering configurations.
--
-- NEFilterManager declares methods and properties for configuring and controlling a filter.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEFilterManager@.
module ObjC.NetworkExtension.NEFilterManager
  ( NEFilterManager
  , IsNEFilterManager(..)
  , sharedManager
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , localizedDescription
  , setLocalizedDescription
  , providerConfiguration
  , setProviderConfiguration
  , enabled
  , setEnabled
  , grade
  , setGrade
  , disableEncryptedDNSSettings
  , setDisableEncryptedDNSSettings
  , sharedManagerSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , providerConfigurationSelector
  , setProviderConfigurationSelector
  , enabledSelector
  , setEnabledSelector
  , gradeSelector
  , setGradeSelector
  , disableEncryptedDNSSettingsSelector
  , setDisableEncryptedDNSSettingsSelector

  -- * Enum types
  , NEFilterManagerGrade(NEFilterManagerGrade)
  , pattern NEFilterManagerGradeFirewall
  , pattern NEFilterManagerGradeInspector

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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | sharedManager
--
-- Returns: The singleton NEFilterManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEFilterManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEFilterManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current filter configuration from the caller's filter preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neFilterManager  completionHandler =
  sendMsg neFilterManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the filter configuration from the caller's filter preferences. If the filter is enabled, the filter becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neFilterManager  completionHandler =
  sendMsg neFilterManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the filter configuration in the caller's filter preferences. If the filter is enabled, it will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neFilterManager  completionHandler =
  sendMsg neFilterManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | localizedDescription
--
-- A string containing a description of the filter.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEFilterManager neFilterManager => neFilterManager -> IO (Id NSString)
localizedDescription neFilterManager  =
  sendMsg neFilterManager (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedDescription
--
-- A string containing a description of the filter.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEFilterManager neFilterManager, IsNSString value) => neFilterManager -> value -> IO ()
setLocalizedDescription neFilterManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterManager (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | providerConfiguration
--
-- An NEFilterProviderConfiguration object containing the provider-specific portion of the filter configuration.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNEFilterManager neFilterManager => neFilterManager -> IO (Id NEFilterProviderConfiguration)
providerConfiguration neFilterManager  =
  sendMsg neFilterManager (mkSelector "providerConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerConfiguration
--
-- An NEFilterProviderConfiguration object containing the provider-specific portion of the filter configuration.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNEFilterManager neFilterManager, IsNEFilterProviderConfiguration value) => neFilterManager -> value -> IO ()
setProviderConfiguration neFilterManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterManager (mkSelector "setProviderConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | enabled
--
-- Toggles the enabled status of the filter. On iOS, setting this property will disable filter configurations of other apps, and this property will be set to NO when other filter configurations are enabled.     On macOS, up to 4 filter configurations of the same grade can be enabled simultaneously.
--
-- ObjC selector: @- enabled@
enabled :: IsNEFilterManager neFilterManager => neFilterManager -> IO Bool
enabled neFilterManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterManager (mkSelector "enabled") retCULong []

-- | enabled
--
-- Toggles the enabled status of the filter. On iOS, setting this property will disable filter configurations of other apps, and this property will be set to NO when other filter configurations are enabled.     On macOS, up to 4 filter configurations of the same grade can be enabled simultaneously.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEFilterManager neFilterManager => neFilterManager -> Bool -> IO ()
setEnabled neFilterManager  value =
  sendMsg neFilterManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | grade
--
-- The grade of the filter. The default grade is NEFilterManagerGradeFirewall.
--
-- ObjC selector: @- grade@
grade :: IsNEFilterManager neFilterManager => neFilterManager -> IO NEFilterManagerGrade
grade neFilterManager  =
  fmap (coerce :: CLong -> NEFilterManagerGrade) $ sendMsg neFilterManager (mkSelector "grade") retCLong []

-- | grade
--
-- The grade of the filter. The default grade is NEFilterManagerGradeFirewall.
--
-- ObjC selector: @- setGrade:@
setGrade :: IsNEFilterManager neFilterManager => neFilterManager -> NEFilterManagerGrade -> IO ()
setGrade neFilterManager  value =
  sendMsg neFilterManager (mkSelector "setGrade:") retVoid [argCLong (coerce value)]

-- | disableEncryptedDNSSettings
--
-- Causes the content filter to disable any other installed encrypted DNS settings, including iCloud Private Relay system-wide DNS encryption. This should only be used if the content filter expects to intercept cleartext UDP DNS packets.
--
-- ObjC selector: @- disableEncryptedDNSSettings@
disableEncryptedDNSSettings :: IsNEFilterManager neFilterManager => neFilterManager -> IO Bool
disableEncryptedDNSSettings neFilterManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterManager (mkSelector "disableEncryptedDNSSettings") retCULong []

-- | disableEncryptedDNSSettings
--
-- Causes the content filter to disable any other installed encrypted DNS settings, including iCloud Private Relay system-wide DNS encryption. This should only be used if the content filter expects to intercept cleartext UDP DNS packets.
--
-- ObjC selector: @- setDisableEncryptedDNSSettings:@
setDisableEncryptedDNSSettings :: IsNEFilterManager neFilterManager => neFilterManager -> Bool -> IO ()
setDisableEncryptedDNSSettings neFilterManager  value =
  sendMsg neFilterManager (mkSelector "setDisableEncryptedDNSSettings:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @providerConfiguration@
providerConfigurationSelector :: Selector
providerConfigurationSelector = mkSelector "providerConfiguration"

-- | @Selector@ for @setProviderConfiguration:@
setProviderConfigurationSelector :: Selector
setProviderConfigurationSelector = mkSelector "setProviderConfiguration:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @grade@
gradeSelector :: Selector
gradeSelector = mkSelector "grade"

-- | @Selector@ for @setGrade:@
setGradeSelector :: Selector
setGradeSelector = mkSelector "setGrade:"

-- | @Selector@ for @disableEncryptedDNSSettings@
disableEncryptedDNSSettingsSelector :: Selector
disableEncryptedDNSSettingsSelector = mkSelector "disableEncryptedDNSSettings"

-- | @Selector@ for @setDisableEncryptedDNSSettings:@
setDisableEncryptedDNSSettingsSelector :: Selector
setDisableEncryptedDNSSettingsSelector = mkSelector "setDisableEncryptedDNSSettings:"

