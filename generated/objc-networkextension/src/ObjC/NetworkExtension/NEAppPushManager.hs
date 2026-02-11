{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppPushManager
--
-- The NEAppPushManager class declares a programmatic interface to configure NEAppPushProvider.
--
-- NEAppPushManager declares methods and properties for configuring and managing life cycle of app push provider.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppPushManager@.
module ObjC.NetworkExtension.NEAppPushManager
  ( NEAppPushManager
  , IsNEAppPushManager(..)
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , matchEthernet
  , setMatchEthernet
  , enabled
  , setEnabled
  , active
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , matchEthernetSelector
  , setMatchEthernetSelector
  , enabledSelector
  , setEnabledSelector
  , activeSelector


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

-- | loadFromPreferencesWithCompletionHandler:
--
-- This method loads the saved configuration from the persistent store.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError object passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neAppPushManager  completionHandler =
  sendMsg neAppPushManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This method removes the configuration from the persistent store.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError object passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neAppPushManager  completionHandler =
  sendMsg neAppPushManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This method saves the configuration in the persistent store.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError object passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neAppPushManager  completionHandler =
  sendMsg neAppPushManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | matchEthernet
--
-- If set to YES NEAppPushProvider is started when iOS device is connected to an Ethernet network and the ethernet network is the primary route  on the device. NEAppPushProvider must determine viability of its functionality on the network. If the network does not support its operation it must call  [NEAppPushProvider unmatchEthernet:] method to stop itself.
--
-- ObjC selector: @- matchEthernet@
matchEthernet :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
matchEthernet neAppPushManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppPushManager (mkSelector "matchEthernet") retCULong []

-- | matchEthernet
--
-- If set to YES NEAppPushProvider is started when iOS device is connected to an Ethernet network and the ethernet network is the primary route  on the device. NEAppPushProvider must determine viability of its functionality on the network. If the network does not support its operation it must call  [NEAppPushProvider unmatchEthernet:] method to stop itself.
--
-- ObjC selector: @- setMatchEthernet:@
setMatchEthernet :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Bool -> IO ()
setMatchEthernet neAppPushManager  value =
  sendMsg neAppPushManager (mkSelector "setMatchEthernet:") retVoid [argCULong (if value then 1 else 0)]

-- | enabled
--
-- Toggles the enabled status of the configuration. This property will be set to NO when the same app saves another configuration that overlaps with this configuration.
--
-- ObjC selector: @- enabled@
enabled :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
enabled neAppPushManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppPushManager (mkSelector "enabled") retCULong []

-- | enabled
--
-- Toggles the enabled status of the configuration. This property will be set to NO when the same app saves another configuration that overlaps with this configuration.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Bool -> IO ()
setEnabled neAppPushManager  value =
  sendMsg neAppPushManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | active
--
-- If set to YES, it indicates the associated configuration is in use. Use KVO to watch for changes.
--
-- ObjC selector: @- active@
active :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
active neAppPushManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppPushManager (mkSelector "active") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @matchEthernet@
matchEthernetSelector :: Selector
matchEthernetSelector = mkSelector "matchEthernet"

-- | @Selector@ for @setMatchEthernet:@
setMatchEthernetSelector :: Selector
setMatchEthernetSelector = mkSelector "setMatchEthernet:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

