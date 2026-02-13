{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Holds configuration of one class of tokens
--
-- Generated bindings for @TKTokenDriverConfiguration@.
module ObjC.CryptoTokenKit.TKTokenDriverConfiguration
  ( TKTokenDriverConfiguration
  , IsTKTokenDriverConfiguration(..)
  , addTokenConfigurationForTokenInstanceID
  , removeTokenConfigurationForTokenInstanceID
  , init_
  , new
  , driverConfigurations
  , classID
  , tokenConfigurations
  , addTokenConfigurationForTokenInstanceIDSelector
  , classIDSelector
  , driverConfigurationsSelector
  , initSelector
  , newSelector
  , removeTokenConfigurationForTokenInstanceIDSelector
  , tokenConfigurationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates new configuration object for token with specified instanceID and adds it into tokenConfigurations dictionary. If configuration with specified instanceID already exists, it is replaced with new empty configuration.
--
-- ObjC selector: @- addTokenConfigurationForTokenInstanceID:@
addTokenConfigurationForTokenInstanceID :: (IsTKTokenDriverConfiguration tkTokenDriverConfiguration, IsNSString instanceID) => tkTokenDriverConfiguration -> instanceID -> IO (Id TKTokenConfiguration)
addTokenConfigurationForTokenInstanceID tkTokenDriverConfiguration instanceID =
  sendMessage tkTokenDriverConfiguration addTokenConfigurationForTokenInstanceIDSelector (toNSString instanceID)

-- | Removes configuration with specified tokenID. Does nothing if no such token configuration exists.
--
-- ObjC selector: @- removeTokenConfigurationForTokenInstanceID:@
removeTokenConfigurationForTokenInstanceID :: (IsTKTokenDriverConfiguration tkTokenDriverConfiguration, IsNSString instanceID) => tkTokenDriverConfiguration -> instanceID -> IO ()
removeTokenConfigurationForTokenInstanceID tkTokenDriverConfiguration instanceID =
  sendMessage tkTokenDriverConfiguration removeTokenConfigurationForTokenInstanceIDSelector (toNSString instanceID)

-- | @- init@
init_ :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id TKTokenDriverConfiguration)
init_ tkTokenDriverConfiguration =
  sendOwnedMessage tkTokenDriverConfiguration initSelector

-- | @+ new@
new :: IO (Id TKTokenDriverConfiguration)
new  =
  do
    cls' <- getRequiredClass "TKTokenDriverConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Contains dictionary of token class configurations keyed by TKTokenDriverClassID of token driver.
--
-- Hosting application of token extension will contain the list of configurations for hosted token extensions. All other callers will get an empty array. This means that only token's hosting application can actually modify token's configuration. Typically, hosting application will contain only one token extension, therefore this dictionary will have one element.
--
-- ObjC selector: @+ driverConfigurations@
driverConfigurations :: IO (Id NSDictionary)
driverConfigurations  =
  do
    cls' <- getRequiredClass "TKTokenDriverConfiguration"
    sendClassMessage cls' driverConfigurationsSelector

-- | ClassID of the token configuration. ClassID is taken from @com.apple.ctk.class-id@ token extension attribute.
--
-- ObjC selector: @- classID@
classID :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id NSString)
classID tkTokenDriverConfiguration =
  sendMessage tkTokenDriverConfiguration classIDSelector

-- | Dictionary of all currently configured tokens for this token class, keyed by instanceID.
--
-- ObjC selector: @- tokenConfigurations@
tokenConfigurations :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id NSDictionary)
tokenConfigurations tkTokenDriverConfiguration =
  sendMessage tkTokenDriverConfiguration tokenConfigurationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTokenConfigurationForTokenInstanceID:@
addTokenConfigurationForTokenInstanceIDSelector :: Selector '[Id NSString] (Id TKTokenConfiguration)
addTokenConfigurationForTokenInstanceIDSelector = mkSelector "addTokenConfigurationForTokenInstanceID:"

-- | @Selector@ for @removeTokenConfigurationForTokenInstanceID:@
removeTokenConfigurationForTokenInstanceIDSelector :: Selector '[Id NSString] ()
removeTokenConfigurationForTokenInstanceIDSelector = mkSelector "removeTokenConfigurationForTokenInstanceID:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenDriverConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id TKTokenDriverConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @driverConfigurations@
driverConfigurationsSelector :: Selector '[] (Id NSDictionary)
driverConfigurationsSelector = mkSelector "driverConfigurations"

-- | @Selector@ for @classID@
classIDSelector :: Selector '[] (Id NSString)
classIDSelector = mkSelector "classID"

-- | @Selector@ for @tokenConfigurations@
tokenConfigurationsSelector :: Selector '[] (Id NSDictionary)
tokenConfigurationsSelector = mkSelector "tokenConfigurations"

