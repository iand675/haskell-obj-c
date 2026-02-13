{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Holds configuration of one token identified by unique token's instanceID
--
-- Generated bindings for @TKTokenConfiguration@.
module ObjC.CryptoTokenKit.TKTokenConfiguration
  ( TKTokenConfiguration
  , IsTKTokenConfiguration(..)
  , keyForObjectID_error
  , certificateForObjectID_error
  , init_
  , new
  , instanceID
  , configurationData
  , setConfigurationData
  , keychainItems
  , setKeychainItems
  , certificateForObjectID_errorSelector
  , configurationDataSelector
  , initSelector
  , instanceIDSelector
  , keyForObjectID_errorSelector
  , keychainItemsSelector
  , newSelector
  , setConfigurationDataSelector
  , setKeychainItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns keychain item key with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such key exists.
--
-- ObjC selector: @- keyForObjectID:error:@
keyForObjectID_error :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSError error_) => tkTokenConfiguration -> RawId -> error_ -> IO (Id TKTokenKeychainKey)
keyForObjectID_error tkTokenConfiguration objectID error_ =
  sendMessage tkTokenConfiguration keyForObjectID_errorSelector objectID (toNSError error_)

-- | Returns certificate with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such certificate exists.
--
-- ObjC selector: @- certificateForObjectID:error:@
certificateForObjectID_error :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSError error_) => tkTokenConfiguration -> RawId -> error_ -> IO (Id TKTokenKeychainCertificate)
certificateForObjectID_error tkTokenConfiguration objectID error_ =
  sendMessage tkTokenConfiguration certificateForObjectID_errorSelector objectID (toNSError error_)

-- | @- init@
init_ :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id TKTokenConfiguration)
init_ tkTokenConfiguration =
  sendOwnedMessage tkTokenConfiguration initSelector

-- | @+ new@
new :: IO (Id TKTokenConfiguration)
new  =
  do
    cls' <- getRequiredClass "TKTokenConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Unique, persistent identifier of this token, always created by specific token implementation. Typically implemented by some kind of serial number of the target hardware, for example SmartCard serial number.
--
-- ObjC selector: @- instanceID@
instanceID :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSString)
instanceID tkTokenConfiguration =
  sendMessage tkTokenConfiguration instanceIDSelector

-- | Additional configuration available for token instance.
--
-- Token implementation and its hosting application can use this data for specifying any additional configuration for the token. System does not interpret this data in any way. For example, network-based HSM can store here (using Codable or other serialization mechanisms) target network address, access credentials and the list of identities accessible in the HSM.
--
-- ObjC selector: @- configurationData@
configurationData :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSData)
configurationData tkTokenConfiguration =
  sendMessage tkTokenConfiguration configurationDataSelector

-- | Additional configuration available for token instance.
--
-- Token implementation and its hosting application can use this data for specifying any additional configuration for the token. System does not interpret this data in any way. For example, network-based HSM can store here (using Codable or other serialization mechanisms) target network address, access credentials and the list of identities accessible in the HSM.
--
-- ObjC selector: @- setConfigurationData:@
setConfigurationData :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSData value) => tkTokenConfiguration -> value -> IO ()
setConfigurationData tkTokenConfiguration value =
  sendMessage tkTokenConfiguration setConfigurationDataSelector (toNSData value)

-- | All keychain items of this token.
--
-- ObjC selector: @- keychainItems@
keychainItems :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSArray)
keychainItems tkTokenConfiguration =
  sendMessage tkTokenConfiguration keychainItemsSelector

-- | All keychain items of this token.
--
-- ObjC selector: @- setKeychainItems:@
setKeychainItems :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSArray value) => tkTokenConfiguration -> value -> IO ()
setKeychainItems tkTokenConfiguration value =
  sendMessage tkTokenConfiguration setKeychainItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyForObjectID:error:@
keyForObjectID_errorSelector :: Selector '[RawId, Id NSError] (Id TKTokenKeychainKey)
keyForObjectID_errorSelector = mkSelector "keyForObjectID:error:"

-- | @Selector@ for @certificateForObjectID:error:@
certificateForObjectID_errorSelector :: Selector '[RawId, Id NSError] (Id TKTokenKeychainCertificate)
certificateForObjectID_errorSelector = mkSelector "certificateForObjectID:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id TKTokenConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @instanceID@
instanceIDSelector :: Selector '[] (Id NSString)
instanceIDSelector = mkSelector "instanceID"

-- | @Selector@ for @configurationData@
configurationDataSelector :: Selector '[] (Id NSData)
configurationDataSelector = mkSelector "configurationData"

-- | @Selector@ for @setConfigurationData:@
setConfigurationDataSelector :: Selector '[Id NSData] ()
setConfigurationDataSelector = mkSelector "setConfigurationData:"

-- | @Selector@ for @keychainItems@
keychainItemsSelector :: Selector '[] (Id NSArray)
keychainItemsSelector = mkSelector "keychainItems"

-- | @Selector@ for @setKeychainItems:@
setKeychainItemsSelector :: Selector '[Id NSArray] ()
setKeychainItemsSelector = mkSelector "setKeychainItems:"

