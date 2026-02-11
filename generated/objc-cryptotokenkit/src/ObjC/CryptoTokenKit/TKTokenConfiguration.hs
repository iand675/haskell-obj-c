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
  , keyForObjectID_errorSelector
  , certificateForObjectID_errorSelector
  , initSelector
  , newSelector
  , instanceIDSelector
  , configurationDataSelector
  , setConfigurationDataSelector
  , keychainItemsSelector
  , setKeychainItemsSelector


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

-- | Returns keychain item key with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such key exists.
--
-- ObjC selector: @- keyForObjectID:error:@
keyForObjectID_error :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSError error_) => tkTokenConfiguration -> RawId -> error_ -> IO (Id TKTokenKeychainKey)
keyForObjectID_error tkTokenConfiguration  objectID error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg tkTokenConfiguration (mkSelector "keyForObjectID:error:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Returns certificate with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such certificate exists.
--
-- ObjC selector: @- certificateForObjectID:error:@
certificateForObjectID_error :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSError error_) => tkTokenConfiguration -> RawId -> error_ -> IO (Id TKTokenKeychainCertificate)
certificateForObjectID_error tkTokenConfiguration  objectID error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg tkTokenConfiguration (mkSelector "certificateForObjectID:error:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id TKTokenConfiguration)
init_ tkTokenConfiguration  =
  sendMsg tkTokenConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id TKTokenConfiguration)
new  =
  do
    cls' <- getRequiredClass "TKTokenConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Unique, persistent identifier of this token, always created by specific token implementation. Typically implemented by some kind of serial number of the target hardware, for example SmartCard serial number.
--
-- ObjC selector: @- instanceID@
instanceID :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSString)
instanceID tkTokenConfiguration  =
  sendMsg tkTokenConfiguration (mkSelector "instanceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Additional configuration available for token instance.
--
-- Token implementation and its hosting application can use this data for specifying any additional configuration for the token. System does not interpret this data in any way. For example, network-based HSM can store here (using Codable or other serialization mechanisms) target network address, access credentials and the list of identities accessible in the HSM.
--
-- ObjC selector: @- configurationData@
configurationData :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSData)
configurationData tkTokenConfiguration  =
  sendMsg tkTokenConfiguration (mkSelector "configurationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Additional configuration available for token instance.
--
-- Token implementation and its hosting application can use this data for specifying any additional configuration for the token. System does not interpret this data in any way. For example, network-based HSM can store here (using Codable or other serialization mechanisms) target network address, access credentials and the list of identities accessible in the HSM.
--
-- ObjC selector: @- setConfigurationData:@
setConfigurationData :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSData value) => tkTokenConfiguration -> value -> IO ()
setConfigurationData tkTokenConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenConfiguration (mkSelector "setConfigurationData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | All keychain items of this token.
--
-- ObjC selector: @- keychainItems@
keychainItems :: IsTKTokenConfiguration tkTokenConfiguration => tkTokenConfiguration -> IO (Id NSArray)
keychainItems tkTokenConfiguration  =
  sendMsg tkTokenConfiguration (mkSelector "keychainItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All keychain items of this token.
--
-- ObjC selector: @- setKeychainItems:@
setKeychainItems :: (IsTKTokenConfiguration tkTokenConfiguration, IsNSArray value) => tkTokenConfiguration -> value -> IO ()
setKeychainItems tkTokenConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenConfiguration (mkSelector "setKeychainItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyForObjectID:error:@
keyForObjectID_errorSelector :: Selector
keyForObjectID_errorSelector = mkSelector "keyForObjectID:error:"

-- | @Selector@ for @certificateForObjectID:error:@
certificateForObjectID_errorSelector :: Selector
certificateForObjectID_errorSelector = mkSelector "certificateForObjectID:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @instanceID@
instanceIDSelector :: Selector
instanceIDSelector = mkSelector "instanceID"

-- | @Selector@ for @configurationData@
configurationDataSelector :: Selector
configurationDataSelector = mkSelector "configurationData"

-- | @Selector@ for @setConfigurationData:@
setConfigurationDataSelector :: Selector
setConfigurationDataSelector = mkSelector "setConfigurationData:"

-- | @Selector@ for @keychainItems@
keychainItemsSelector :: Selector
keychainItemsSelector = mkSelector "keychainItems"

-- | @Selector@ for @setKeychainItems:@
setKeychainItemsSelector :: Selector
setKeychainItemsSelector = mkSelector "setKeychainItems:"

