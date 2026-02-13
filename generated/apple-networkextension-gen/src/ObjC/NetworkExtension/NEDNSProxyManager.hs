{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEDNSProxyManager
--
-- The NEDNSProxyManager class declares the programmatic interface for an object that manages DNS proxy configurations.
--
-- NEDNSProxyManager declares methods and properties for configuring and controlling a DNS proxy.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEDNSProxyManager@.
module ObjC.NetworkExtension.NEDNSProxyManager
  ( NEDNSProxyManager
  , IsNEDNSProxyManager(..)
  , sharedManager
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , localizedDescription
  , setLocalizedDescription
  , providerProtocol
  , setProviderProtocol
  , enabled
  , setEnabled
  , enabledSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , providerProtocolSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setEnabledSelector
  , setLocalizedDescriptionSelector
  , setProviderProtocolSelector
  , sharedManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedManager
--
-- Returns: The singleton NEDNSProxyManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEDNSProxyManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEDNSProxyManager"
    sendClassMessage cls' sharedManagerSelector

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current DNS proxy configuration from the caller's DNS proxy preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nednsProxyManager completionHandler =
  sendMessage nednsProxyManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the DNS proxy configuration from the caller's DNS proxy preferences. If the DNS proxy is enabled, the DNS proxy becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nednsProxyManager completionHandler =
  sendMessage nednsProxyManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the DNS proxy configuration in the caller's DNS proxy preferences. If the DNS proxy is enabled, it will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nednsProxyManager completionHandler =
  sendMessage nednsProxyManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | localizedDescription
--
-- A string containing a description of the DNS proxy.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO (Id NSString)
localizedDescription nednsProxyManager =
  sendMessage nednsProxyManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the DNS proxy.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEDNSProxyManager nednsProxyManager, IsNSString value) => nednsProxyManager -> value -> IO ()
setLocalizedDescription nednsProxyManager value =
  sendMessage nednsProxyManager setLocalizedDescriptionSelector (toNSString value)

-- | providerProtocol
--
-- An NEDNSProxyProviderProtocol object containing the provider-specific portion of the DNS proxy configuration.
--
-- ObjC selector: @- providerProtocol@
providerProtocol :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO (Id NEDNSProxyProviderProtocol)
providerProtocol nednsProxyManager =
  sendMessage nednsProxyManager providerProtocolSelector

-- | providerProtocol
--
-- An NEDNSProxyProviderProtocol object containing the provider-specific portion of the DNS proxy configuration.
--
-- ObjC selector: @- setProviderProtocol:@
setProviderProtocol :: (IsNEDNSProxyManager nednsProxyManager, IsNEDNSProxyProviderProtocol value) => nednsProxyManager -> value -> IO ()
setProviderProtocol nednsProxyManager value =
  sendMessage nednsProxyManager setProviderProtocolSelector (toNEDNSProxyProviderProtocol value)

-- | enabled
--
-- Toggles the enabled status of the DNS proxy. Setting this property will disable DNS proxy configurations of other apps. This property will be set to NO when other DNS proxy configurations are enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO Bool
enabled nednsProxyManager =
  sendMessage nednsProxyManager enabledSelector

-- | enabled
--
-- Toggles the enabled status of the DNS proxy. Setting this property will disable DNS proxy configurations of other apps. This property will be set to NO when other DNS proxy configurations are enabled.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Bool -> IO ()
setEnabled nednsProxyManager value =
  sendMessage nednsProxyManager setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id NEDNSProxyManager)
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

-- | @Selector@ for @providerProtocol@
providerProtocolSelector :: Selector '[] (Id NEDNSProxyProviderProtocol)
providerProtocolSelector = mkSelector "providerProtocol"

-- | @Selector@ for @setProviderProtocol:@
setProviderProtocolSelector :: Selector '[Id NEDNSProxyProviderProtocol] ()
setProviderProtocolSelector = mkSelector "setProviderProtocol:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

