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
  , removeTokenConfigurationForTokenInstanceIDSelector
  , initSelector
  , newSelector
  , driverConfigurationsSelector
  , classIDSelector
  , tokenConfigurationsSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates new configuration object for token with specified instanceID and adds it into tokenConfigurations dictionary. If configuration with specified instanceID already exists, it is replaced with new empty configuration.
--
-- ObjC selector: @- addTokenConfigurationForTokenInstanceID:@
addTokenConfigurationForTokenInstanceID :: (IsTKTokenDriverConfiguration tkTokenDriverConfiguration, IsNSString instanceID) => tkTokenDriverConfiguration -> instanceID -> IO (Id TKTokenConfiguration)
addTokenConfigurationForTokenInstanceID tkTokenDriverConfiguration  instanceID =
withObjCPtr instanceID $ \raw_instanceID ->
    sendMsg tkTokenDriverConfiguration (mkSelector "addTokenConfigurationForTokenInstanceID:") (retPtr retVoid) [argPtr (castPtr raw_instanceID :: Ptr ())] >>= retainedObject . castPtr

-- | Removes configuration with specified tokenID. Does nothing if no such token configuration exists.
--
-- ObjC selector: @- removeTokenConfigurationForTokenInstanceID:@
removeTokenConfigurationForTokenInstanceID :: (IsTKTokenDriverConfiguration tkTokenDriverConfiguration, IsNSString instanceID) => tkTokenDriverConfiguration -> instanceID -> IO ()
removeTokenConfigurationForTokenInstanceID tkTokenDriverConfiguration  instanceID =
withObjCPtr instanceID $ \raw_instanceID ->
    sendMsg tkTokenDriverConfiguration (mkSelector "removeTokenConfigurationForTokenInstanceID:") retVoid [argPtr (castPtr raw_instanceID :: Ptr ())]

-- | @- init@
init_ :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id TKTokenDriverConfiguration)
init_ tkTokenDriverConfiguration  =
  sendMsg tkTokenDriverConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id TKTokenDriverConfiguration)
new  =
  do
    cls' <- getRequiredClass "TKTokenDriverConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Contains dictionary of token class configurations keyed by TKTokenDriverClassID of token driver.
--
-- Hosting application of token extension will contain the list of configurations for hosted token extensions. All other callers will get an empty array. This means that only token's hosting application can actually modify token's configuration. Typically, hosting application will contain only one token extension, therefore this dictionary will have one element.
--
-- ObjC selector: @+ driverConfigurations@
driverConfigurations :: IO (Id NSDictionary)
driverConfigurations  =
  do
    cls' <- getRequiredClass "TKTokenDriverConfiguration"
    sendClassMsg cls' (mkSelector "driverConfigurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ClassID of the token configuration. ClassID is taken from @com.apple.ctk.class-id@ token extension attribute.
--
-- ObjC selector: @- classID@
classID :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id NSString)
classID tkTokenDriverConfiguration  =
  sendMsg tkTokenDriverConfiguration (mkSelector "classID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Dictionary of all currently configured tokens for this token class, keyed by instanceID.
--
-- ObjC selector: @- tokenConfigurations@
tokenConfigurations :: IsTKTokenDriverConfiguration tkTokenDriverConfiguration => tkTokenDriverConfiguration -> IO (Id NSDictionary)
tokenConfigurations tkTokenDriverConfiguration  =
  sendMsg tkTokenDriverConfiguration (mkSelector "tokenConfigurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addTokenConfigurationForTokenInstanceID:@
addTokenConfigurationForTokenInstanceIDSelector :: Selector
addTokenConfigurationForTokenInstanceIDSelector = mkSelector "addTokenConfigurationForTokenInstanceID:"

-- | @Selector@ for @removeTokenConfigurationForTokenInstanceID:@
removeTokenConfigurationForTokenInstanceIDSelector :: Selector
removeTokenConfigurationForTokenInstanceIDSelector = mkSelector "removeTokenConfigurationForTokenInstanceID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @driverConfigurations@
driverConfigurationsSelector :: Selector
driverConfigurationsSelector = mkSelector "driverConfigurations"

-- | @Selector@ for @classID@
classIDSelector :: Selector
classIDSelector = mkSelector "classID"

-- | @Selector@ for @tokenConfigurations@
tokenConfigurationsSelector :: Selector
tokenConfigurationsSelector = mkSelector "tokenConfigurations"

