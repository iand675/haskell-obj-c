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
  , sharedManagerSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , providerProtocolSelector
  , setProviderProtocolSelector
  , enabledSelector
  , setEnabledSelector


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
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current DNS proxy configuration from the caller's DNS proxy preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nednsProxyManager  completionHandler =
  sendMsg nednsProxyManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the DNS proxy configuration from the caller's DNS proxy preferences. If the DNS proxy is enabled, the DNS proxy becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nednsProxyManager  completionHandler =
  sendMsg nednsProxyManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the DNS proxy configuration in the caller's DNS proxy preferences. If the DNS proxy is enabled, it will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nednsProxyManager  completionHandler =
  sendMsg nednsProxyManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | localizedDescription
--
-- A string containing a description of the DNS proxy.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO (Id NSString)
localizedDescription nednsProxyManager  =
  sendMsg nednsProxyManager (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedDescription
--
-- A string containing a description of the DNS proxy.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEDNSProxyManager nednsProxyManager, IsNSString value) => nednsProxyManager -> value -> IO ()
setLocalizedDescription nednsProxyManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsProxyManager (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | providerProtocol
--
-- An NEDNSProxyProviderProtocol object containing the provider-specific portion of the DNS proxy configuration.
--
-- ObjC selector: @- providerProtocol@
providerProtocol :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO (Id NEDNSProxyProviderProtocol)
providerProtocol nednsProxyManager  =
  sendMsg nednsProxyManager (mkSelector "providerProtocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerProtocol
--
-- An NEDNSProxyProviderProtocol object containing the provider-specific portion of the DNS proxy configuration.
--
-- ObjC selector: @- setProviderProtocol:@
setProviderProtocol :: (IsNEDNSProxyManager nednsProxyManager, IsNEDNSProxyProviderProtocol value) => nednsProxyManager -> value -> IO ()
setProviderProtocol nednsProxyManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsProxyManager (mkSelector "setProviderProtocol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | enabled
--
-- Toggles the enabled status of the DNS proxy. Setting this property will disable DNS proxy configurations of other apps. This property will be set to NO when other DNS proxy configurations are enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> IO Bool
enabled nednsProxyManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nednsProxyManager (mkSelector "enabled") retCULong []

-- | enabled
--
-- Toggles the enabled status of the DNS proxy. Setting this property will disable DNS proxy configurations of other apps. This property will be set to NO when other DNS proxy configurations are enabled.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEDNSProxyManager nednsProxyManager => nednsProxyManager -> Bool -> IO ()
setEnabled nednsProxyManager  value =
  sendMsg nednsProxyManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

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

-- | @Selector@ for @providerProtocol@
providerProtocolSelector :: Selector
providerProtocolSelector = mkSelector "providerProtocol"

-- | @Selector@ for @setProviderProtocol:@
setProviderProtocolSelector :: Selector
setProviderProtocolSelector = mkSelector "setProviderProtocol:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

