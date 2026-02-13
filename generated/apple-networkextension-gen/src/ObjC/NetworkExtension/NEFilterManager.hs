{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , disableEncryptedDNSSettingsSelector
  , enabledSelector
  , gradeSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , providerConfigurationSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setDisableEncryptedDNSSettingsSelector
  , setEnabledSelector
  , setGradeSelector
  , setLocalizedDescriptionSelector
  , setProviderConfigurationSelector
  , sharedManagerSelector

  -- * Enum types
  , NEFilterManagerGrade(NEFilterManagerGrade)
  , pattern NEFilterManagerGradeFirewall
  , pattern NEFilterManagerGradeInspector

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedManagerSelector

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current filter configuration from the caller's filter preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neFilterManager completionHandler =
  sendMessage neFilterManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the filter configuration from the caller's filter preferences. If the filter is enabled, the filter becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neFilterManager completionHandler =
  sendMessage neFilterManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the filter configuration in the caller's filter preferences. If the filter is enabled, it will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEFilterManager neFilterManager => neFilterManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neFilterManager completionHandler =
  sendMessage neFilterManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | localizedDescription
--
-- A string containing a description of the filter.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEFilterManager neFilterManager => neFilterManager -> IO (Id NSString)
localizedDescription neFilterManager =
  sendMessage neFilterManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the filter.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEFilterManager neFilterManager, IsNSString value) => neFilterManager -> value -> IO ()
setLocalizedDescription neFilterManager value =
  sendMessage neFilterManager setLocalizedDescriptionSelector (toNSString value)

-- | providerConfiguration
--
-- An NEFilterProviderConfiguration object containing the provider-specific portion of the filter configuration.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNEFilterManager neFilterManager => neFilterManager -> IO (Id NEFilterProviderConfiguration)
providerConfiguration neFilterManager =
  sendMessage neFilterManager providerConfigurationSelector

-- | providerConfiguration
--
-- An NEFilterProviderConfiguration object containing the provider-specific portion of the filter configuration.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNEFilterManager neFilterManager, IsNEFilterProviderConfiguration value) => neFilterManager -> value -> IO ()
setProviderConfiguration neFilterManager value =
  sendMessage neFilterManager setProviderConfigurationSelector (toNEFilterProviderConfiguration value)

-- | enabled
--
-- Toggles the enabled status of the filter. On iOS, setting this property will disable filter configurations of other apps, and this property will be set to NO when other filter configurations are enabled.     On macOS, up to 4 filter configurations of the same grade can be enabled simultaneously.
--
-- ObjC selector: @- enabled@
enabled :: IsNEFilterManager neFilterManager => neFilterManager -> IO Bool
enabled neFilterManager =
  sendMessage neFilterManager enabledSelector

-- | enabled
--
-- Toggles the enabled status of the filter. On iOS, setting this property will disable filter configurations of other apps, and this property will be set to NO when other filter configurations are enabled.     On macOS, up to 4 filter configurations of the same grade can be enabled simultaneously.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEFilterManager neFilterManager => neFilterManager -> Bool -> IO ()
setEnabled neFilterManager value =
  sendMessage neFilterManager setEnabledSelector value

-- | grade
--
-- The grade of the filter. The default grade is NEFilterManagerGradeFirewall.
--
-- ObjC selector: @- grade@
grade :: IsNEFilterManager neFilterManager => neFilterManager -> IO NEFilterManagerGrade
grade neFilterManager =
  sendMessage neFilterManager gradeSelector

-- | grade
--
-- The grade of the filter. The default grade is NEFilterManagerGradeFirewall.
--
-- ObjC selector: @- setGrade:@
setGrade :: IsNEFilterManager neFilterManager => neFilterManager -> NEFilterManagerGrade -> IO ()
setGrade neFilterManager value =
  sendMessage neFilterManager setGradeSelector value

-- | disableEncryptedDNSSettings
--
-- Causes the content filter to disable any other installed encrypted DNS settings, including iCloud Private Relay system-wide DNS encryption. This should only be used if the content filter expects to intercept cleartext UDP DNS packets.
--
-- ObjC selector: @- disableEncryptedDNSSettings@
disableEncryptedDNSSettings :: IsNEFilterManager neFilterManager => neFilterManager -> IO Bool
disableEncryptedDNSSettings neFilterManager =
  sendMessage neFilterManager disableEncryptedDNSSettingsSelector

-- | disableEncryptedDNSSettings
--
-- Causes the content filter to disable any other installed encrypted DNS settings, including iCloud Private Relay system-wide DNS encryption. This should only be used if the content filter expects to intercept cleartext UDP DNS packets.
--
-- ObjC selector: @- setDisableEncryptedDNSSettings:@
setDisableEncryptedDNSSettings :: IsNEFilterManager neFilterManager => neFilterManager -> Bool -> IO ()
setDisableEncryptedDNSSettings neFilterManager value =
  sendMessage neFilterManager setDisableEncryptedDNSSettingsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id NEFilterManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @providerConfiguration@
providerConfigurationSelector :: Selector '[] (Id NEFilterProviderConfiguration)
providerConfigurationSelector = mkSelector "providerConfiguration"

-- | @Selector@ for @setProviderConfiguration:@
setProviderConfigurationSelector :: Selector '[Id NEFilterProviderConfiguration] ()
setProviderConfigurationSelector = mkSelector "setProviderConfiguration:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @grade@
gradeSelector :: Selector '[] NEFilterManagerGrade
gradeSelector = mkSelector "grade"

-- | @Selector@ for @setGrade:@
setGradeSelector :: Selector '[NEFilterManagerGrade] ()
setGradeSelector = mkSelector "setGrade:"

-- | @Selector@ for @disableEncryptedDNSSettings@
disableEncryptedDNSSettingsSelector :: Selector '[] Bool
disableEncryptedDNSSettingsSelector = mkSelector "disableEncryptedDNSSettings"

-- | @Selector@ for @setDisableEncryptedDNSSettings:@
setDisableEncryptedDNSSettingsSelector :: Selector '[Bool] ()
setDisableEncryptedDNSSettingsSelector = mkSelector "setDisableEncryptedDNSSettings:"

