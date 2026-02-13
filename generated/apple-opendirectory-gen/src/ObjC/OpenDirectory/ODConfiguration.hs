{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ODConfiguration@.
module ObjC.OpenDirectory.ODConfiguration
  ( ODConfiguration
  , IsODConfiguration(..)
  , configuration
  , suggestedTrustAccount
  , suggestedTrustPassword
  , saveUsingAuthorization_error
  , addTrustType_trustAccount_trustPassword_username_password_joinExisting_error
  , removeTrustUsingUsername_password_deleteTrustAccount_error
  , nodeName
  , setNodeName
  , comment
  , setComment
  , defaultMappings
  , setDefaultMappings
  , templateName
  , setTemplateName
  , virtualSubnodes
  , setVirtualSubnodes
  , hideRegistration
  , setHideRegistration
  , preferredDestinationHostName
  , setPreferredDestinationHostName
  , preferredDestinationHostPort
  , setPreferredDestinationHostPort
  , trustAccount
  , trustMetaAccount
  , trustKerberosPrincipal
  , trustType
  , trustUsesMutualAuthentication
  , trustUsesKerberosKeytab
  , trustUsesSystemKeychain
  , packetSigning
  , setPacketSigning
  , packetEncryption
  , setPacketEncryption
  , manInTheMiddleProtection
  , setManInTheMiddleProtection
  , queryTimeoutInSeconds
  , setQueryTimeoutInSeconds
  , connectionSetupTimeoutInSeconds
  , setConnectionSetupTimeoutInSeconds
  , connectionIdleTimeoutInSeconds
  , setConnectionIdleTimeoutInSeconds
  , defaultModuleEntries
  , setDefaultModuleEntries
  , authenticationModuleEntries
  , setAuthenticationModuleEntries
  , discoveryModuleEntries
  , setDiscoveryModuleEntries
  , generalModuleEntries
  , setGeneralModuleEntries
  , addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector
  , authenticationModuleEntriesSelector
  , commentSelector
  , configurationSelector
  , connectionIdleTimeoutInSecondsSelector
  , connectionSetupTimeoutInSecondsSelector
  , defaultMappingsSelector
  , defaultModuleEntriesSelector
  , discoveryModuleEntriesSelector
  , generalModuleEntriesSelector
  , hideRegistrationSelector
  , manInTheMiddleProtectionSelector
  , nodeNameSelector
  , packetEncryptionSelector
  , packetSigningSelector
  , preferredDestinationHostNameSelector
  , preferredDestinationHostPortSelector
  , queryTimeoutInSecondsSelector
  , removeTrustUsingUsername_password_deleteTrustAccount_errorSelector
  , saveUsingAuthorization_errorSelector
  , setAuthenticationModuleEntriesSelector
  , setCommentSelector
  , setConnectionIdleTimeoutInSecondsSelector
  , setConnectionSetupTimeoutInSecondsSelector
  , setDefaultMappingsSelector
  , setDefaultModuleEntriesSelector
  , setDiscoveryModuleEntriesSelector
  , setGeneralModuleEntriesSelector
  , setHideRegistrationSelector
  , setManInTheMiddleProtectionSelector
  , setNodeNameSelector
  , setPacketEncryptionSelector
  , setPacketSigningSelector
  , setPreferredDestinationHostNameSelector
  , setPreferredDestinationHostPortSelector
  , setQueryTimeoutInSecondsSelector
  , setTemplateNameSelector
  , setVirtualSubnodesSelector
  , suggestedTrustAccountSelector
  , suggestedTrustPasswordSelector
  , templateNameSelector
  , trustAccountSelector
  , trustKerberosPrincipalSelector
  , trustMetaAccountSelector
  , trustTypeSelector
  , trustUsesKerberosKeytabSelector
  , trustUsesMutualAuthenticationSelector
  , trustUsesSystemKeychainSelector
  , virtualSubnodesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- | configuration
--
-- Returns an initialized and autoreleased ODConfiguration object.
--
-- Returns an initialized and autoreleased ODConfiguration object.
--
-- ObjC selector: @+ configuration@
configuration :: IO (Id ODConfiguration)
configuration  =
  do
    cls' <- getRequiredClass "ODConfiguration"
    sendClassMessage cls' configurationSelector

-- | suggestedTrustAccount:
--
-- Returns a suggested name to use for the trust account.
--
-- Returns a suggested name to use for a trust account.  This name will be derived from the hostname (if provided), otherwise it will be derived from the local hostname removing special characters that may not be allowed by many systems.
--
-- ObjC selector: @+ suggestedTrustAccount:@
suggestedTrustAccount :: IsNSString hostname => hostname -> IO (Id NSString)
suggestedTrustAccount hostname =
  do
    cls' <- getRequiredClass "ODConfiguration"
    sendClassMessage cls' suggestedTrustAccountSelector (toNSString hostname)

-- | suggestedTrustPassword:
--
-- Returns a suggested password to be used for trust account with the requested length.
--
-- Returns a suggested password to be used for trust account with the requested length.
--
-- ObjC selector: @+ suggestedTrustPassword:@
suggestedTrustPassword :: CULong -> IO (Id NSString)
suggestedTrustPassword length_ =
  do
    cls' <- getRequiredClass "ODConfiguration"
    sendClassMessage cls' suggestedTrustPasswordSelector length_

-- | saveUsingAuthorization:error:
--
-- Saves the configuration using the provided authorization.
--
-- Saves the configuration using the provided authorization.
--
-- ObjC selector: @- saveUsingAuthorization:error:@
saveUsingAuthorization_error :: (IsODConfiguration odConfiguration, IsSFAuthorization authorization, IsNSError error_) => odConfiguration -> authorization -> error_ -> IO Bool
saveUsingAuthorization_error odConfiguration authorization error_ =
  sendMessage odConfiguration saveUsingAuthorization_errorSelector (toSFAuthorization authorization) (toNSError error_)

-- | addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:
--
-- Adds a trust account with the provided name and password using the credentials provided by the user.
--
-- Adds a trust account with the provided name and password using the credentials provided by the user.  User can request that the trust be forcibly created (replacing existing trust if found in directory).  A trust should be established only after enough configuration is available and the configuration been saved.  If the trust is required, then the configuration can be deleted if necessary upon failure.
--
-- ObjC selector: @- addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:@
addTrustType_trustAccount_trustPassword_username_password_joinExisting_error :: (IsODConfiguration odConfiguration, IsNSString trustType, IsNSString account, IsNSString accountPassword, IsNSString username, IsNSString password, IsNSError error_) => odConfiguration -> trustType -> account -> accountPassword -> username -> password -> Bool -> error_ -> IO Bool
addTrustType_trustAccount_trustPassword_username_password_joinExisting_error odConfiguration trustType account accountPassword username password join error_ =
  sendMessage odConfiguration addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector (toNSString trustType) (toNSString account) (toNSString accountPassword) (toNSString username) (toNSString password) join (toNSError error_)

-- | removeTrustUsingUsername:password:deleteTrustAccount:error:
--
-- Removes trust using the provided username and password.
--
-- Removes trust using the provided username and password.  The trust account will be removed from the directory only if requested.
--
-- ObjC selector: @- removeTrustUsingUsername:password:deleteTrustAccount:error:@
removeTrustUsingUsername_password_deleteTrustAccount_error :: (IsODConfiguration odConfiguration, IsNSString username, IsNSString password, IsNSError error_) => odConfiguration -> username -> password -> Bool -> error_ -> IO Bool
removeTrustUsingUsername_password_deleteTrustAccount_error odConfiguration username password deleteAccount error_ =
  sendMessage odConfiguration removeTrustUsingUsername_password_deleteTrustAccount_errorSelector (toNSString username) (toNSString password) deleteAccount (toNSError error_)

-- | @- nodeName@
nodeName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
nodeName odConfiguration =
  sendMessage odConfiguration nodeNameSelector

-- | @- setNodeName:@
setNodeName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setNodeName odConfiguration value =
  sendMessage odConfiguration setNodeNameSelector (toNSString value)

-- | @- comment@
comment :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
comment odConfiguration =
  sendMessage odConfiguration commentSelector

-- | @- setComment:@
setComment :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setComment odConfiguration value =
  sendMessage odConfiguration setCommentSelector (toNSString value)

-- | @- defaultMappings@
defaultMappings :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id ODMappings)
defaultMappings odConfiguration =
  sendMessage odConfiguration defaultMappingsSelector

-- | @- setDefaultMappings:@
setDefaultMappings :: (IsODConfiguration odConfiguration, IsODMappings value) => odConfiguration -> value -> IO ()
setDefaultMappings odConfiguration value =
  sendMessage odConfiguration setDefaultMappingsSelector (toODMappings value)

-- | @- templateName@
templateName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
templateName odConfiguration =
  sendMessage odConfiguration templateNameSelector

-- | @- setTemplateName:@
setTemplateName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setTemplateName odConfiguration value =
  sendMessage odConfiguration setTemplateNameSelector (toNSString value)

-- | @- virtualSubnodes@
virtualSubnodes :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
virtualSubnodes odConfiguration =
  sendMessage odConfiguration virtualSubnodesSelector

-- | @- setVirtualSubnodes:@
setVirtualSubnodes :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setVirtualSubnodes odConfiguration value =
  sendMessage odConfiguration setVirtualSubnodesSelector (toNSArray value)

-- | @- hideRegistration@
hideRegistration :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
hideRegistration odConfiguration =
  sendMessage odConfiguration hideRegistrationSelector

-- | @- setHideRegistration:@
setHideRegistration :: IsODConfiguration odConfiguration => odConfiguration -> Bool -> IO ()
setHideRegistration odConfiguration value =
  sendMessage odConfiguration setHideRegistrationSelector value

-- | @- preferredDestinationHostName@
preferredDestinationHostName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
preferredDestinationHostName odConfiguration =
  sendMessage odConfiguration preferredDestinationHostNameSelector

-- | @- setPreferredDestinationHostName:@
setPreferredDestinationHostName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setPreferredDestinationHostName odConfiguration value =
  sendMessage odConfiguration setPreferredDestinationHostNameSelector (toNSString value)

-- | @- preferredDestinationHostPort@
preferredDestinationHostPort :: IsODConfiguration odConfiguration => odConfiguration -> IO CUShort
preferredDestinationHostPort odConfiguration =
  sendMessage odConfiguration preferredDestinationHostPortSelector

-- | @- setPreferredDestinationHostPort:@
setPreferredDestinationHostPort :: IsODConfiguration odConfiguration => odConfiguration -> CUShort -> IO ()
setPreferredDestinationHostPort odConfiguration value =
  sendMessage odConfiguration setPreferredDestinationHostPortSelector value

-- | @- trustAccount@
trustAccount :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustAccount odConfiguration =
  sendMessage odConfiguration trustAccountSelector

-- | @- trustMetaAccount@
trustMetaAccount :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustMetaAccount odConfiguration =
  sendMessage odConfiguration trustMetaAccountSelector

-- | @- trustKerberosPrincipal@
trustKerberosPrincipal :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustKerberosPrincipal odConfiguration =
  sendMessage odConfiguration trustKerberosPrincipalSelector

-- | @- trustType@
trustType :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustType odConfiguration =
  sendMessage odConfiguration trustTypeSelector

-- | @- trustUsesMutualAuthentication@
trustUsesMutualAuthentication :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesMutualAuthentication odConfiguration =
  sendMessage odConfiguration trustUsesMutualAuthenticationSelector

-- | @- trustUsesKerberosKeytab@
trustUsesKerberosKeytab :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesKerberosKeytab odConfiguration =
  sendMessage odConfiguration trustUsesKerberosKeytabSelector

-- | @- trustUsesSystemKeychain@
trustUsesSystemKeychain :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesSystemKeychain odConfiguration =
  sendMessage odConfiguration trustUsesSystemKeychainSelector

-- | @- packetSigning@
packetSigning :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
packetSigning odConfiguration =
  sendMessage odConfiguration packetSigningSelector

-- | @- setPacketSigning:@
setPacketSigning :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setPacketSigning odConfiguration value =
  sendMessage odConfiguration setPacketSigningSelector value

-- | @- packetEncryption@
packetEncryption :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
packetEncryption odConfiguration =
  sendMessage odConfiguration packetEncryptionSelector

-- | @- setPacketEncryption:@
setPacketEncryption :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setPacketEncryption odConfiguration value =
  sendMessage odConfiguration setPacketEncryptionSelector value

-- | @- manInTheMiddleProtection@
manInTheMiddleProtection :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
manInTheMiddleProtection odConfiguration =
  sendMessage odConfiguration manInTheMiddleProtectionSelector

-- | @- setManInTheMiddleProtection:@
setManInTheMiddleProtection :: IsODConfiguration odConfiguration => odConfiguration -> Bool -> IO ()
setManInTheMiddleProtection odConfiguration value =
  sendMessage odConfiguration setManInTheMiddleProtectionSelector value

-- | @- queryTimeoutInSeconds@
queryTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
queryTimeoutInSeconds odConfiguration =
  sendMessage odConfiguration queryTimeoutInSecondsSelector

-- | @- setQueryTimeoutInSeconds:@
setQueryTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setQueryTimeoutInSeconds odConfiguration value =
  sendMessage odConfiguration setQueryTimeoutInSecondsSelector value

-- | @- connectionSetupTimeoutInSeconds@
connectionSetupTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
connectionSetupTimeoutInSeconds odConfiguration =
  sendMessage odConfiguration connectionSetupTimeoutInSecondsSelector

-- | @- setConnectionSetupTimeoutInSeconds:@
setConnectionSetupTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setConnectionSetupTimeoutInSeconds odConfiguration value =
  sendMessage odConfiguration setConnectionSetupTimeoutInSecondsSelector value

-- | @- connectionIdleTimeoutInSeconds@
connectionIdleTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
connectionIdleTimeoutInSeconds odConfiguration =
  sendMessage odConfiguration connectionIdleTimeoutInSecondsSelector

-- | @- setConnectionIdleTimeoutInSeconds:@
setConnectionIdleTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setConnectionIdleTimeoutInSeconds odConfiguration value =
  sendMessage odConfiguration setConnectionIdleTimeoutInSecondsSelector value

-- | @- defaultModuleEntries@
defaultModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
defaultModuleEntries odConfiguration =
  sendMessage odConfiguration defaultModuleEntriesSelector

-- | @- setDefaultModuleEntries:@
setDefaultModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setDefaultModuleEntries odConfiguration value =
  sendMessage odConfiguration setDefaultModuleEntriesSelector (toNSArray value)

-- | @- authenticationModuleEntries@
authenticationModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
authenticationModuleEntries odConfiguration =
  sendMessage odConfiguration authenticationModuleEntriesSelector

-- | @- setAuthenticationModuleEntries:@
setAuthenticationModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setAuthenticationModuleEntries odConfiguration value =
  sendMessage odConfiguration setAuthenticationModuleEntriesSelector (toNSArray value)

-- | @- discoveryModuleEntries@
discoveryModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
discoveryModuleEntries odConfiguration =
  sendMessage odConfiguration discoveryModuleEntriesSelector

-- | @- setDiscoveryModuleEntries:@
setDiscoveryModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setDiscoveryModuleEntries odConfiguration value =
  sendMessage odConfiguration setDiscoveryModuleEntriesSelector (toNSArray value)

-- | @- generalModuleEntries@
generalModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
generalModuleEntries odConfiguration =
  sendMessage odConfiguration generalModuleEntriesSelector

-- | @- setGeneralModuleEntries:@
setGeneralModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setGeneralModuleEntries odConfiguration value =
  sendMessage odConfiguration setGeneralModuleEntriesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id ODConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @suggestedTrustAccount:@
suggestedTrustAccountSelector :: Selector '[Id NSString] (Id NSString)
suggestedTrustAccountSelector = mkSelector "suggestedTrustAccount:"

-- | @Selector@ for @suggestedTrustPassword:@
suggestedTrustPasswordSelector :: Selector '[CULong] (Id NSString)
suggestedTrustPasswordSelector = mkSelector "suggestedTrustPassword:"

-- | @Selector@ for @saveUsingAuthorization:error:@
saveUsingAuthorization_errorSelector :: Selector '[Id SFAuthorization, Id NSError] Bool
saveUsingAuthorization_errorSelector = mkSelector "saveUsingAuthorization:error:"

-- | @Selector@ for @addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:@
addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Bool, Id NSError] Bool
addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector = mkSelector "addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:"

-- | @Selector@ for @removeTrustUsingUsername:password:deleteTrustAccount:error:@
removeTrustUsingUsername_password_deleteTrustAccount_errorSelector :: Selector '[Id NSString, Id NSString, Bool, Id NSError] Bool
removeTrustUsingUsername_password_deleteTrustAccount_errorSelector = mkSelector "removeTrustUsingUsername:password:deleteTrustAccount:error:"

-- | @Selector@ for @nodeName@
nodeNameSelector :: Selector '[] (Id NSString)
nodeNameSelector = mkSelector "nodeName"

-- | @Selector@ for @setNodeName:@
setNodeNameSelector :: Selector '[Id NSString] ()
setNodeNameSelector = mkSelector "setNodeName:"

-- | @Selector@ for @comment@
commentSelector :: Selector '[] (Id NSString)
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector '[Id NSString] ()
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @defaultMappings@
defaultMappingsSelector :: Selector '[] (Id ODMappings)
defaultMappingsSelector = mkSelector "defaultMappings"

-- | @Selector@ for @setDefaultMappings:@
setDefaultMappingsSelector :: Selector '[Id ODMappings] ()
setDefaultMappingsSelector = mkSelector "setDefaultMappings:"

-- | @Selector@ for @templateName@
templateNameSelector :: Selector '[] (Id NSString)
templateNameSelector = mkSelector "templateName"

-- | @Selector@ for @setTemplateName:@
setTemplateNameSelector :: Selector '[Id NSString] ()
setTemplateNameSelector = mkSelector "setTemplateName:"

-- | @Selector@ for @virtualSubnodes@
virtualSubnodesSelector :: Selector '[] (Id NSArray)
virtualSubnodesSelector = mkSelector "virtualSubnodes"

-- | @Selector@ for @setVirtualSubnodes:@
setVirtualSubnodesSelector :: Selector '[Id NSArray] ()
setVirtualSubnodesSelector = mkSelector "setVirtualSubnodes:"

-- | @Selector@ for @hideRegistration@
hideRegistrationSelector :: Selector '[] Bool
hideRegistrationSelector = mkSelector "hideRegistration"

-- | @Selector@ for @setHideRegistration:@
setHideRegistrationSelector :: Selector '[Bool] ()
setHideRegistrationSelector = mkSelector "setHideRegistration:"

-- | @Selector@ for @preferredDestinationHostName@
preferredDestinationHostNameSelector :: Selector '[] (Id NSString)
preferredDestinationHostNameSelector = mkSelector "preferredDestinationHostName"

-- | @Selector@ for @setPreferredDestinationHostName:@
setPreferredDestinationHostNameSelector :: Selector '[Id NSString] ()
setPreferredDestinationHostNameSelector = mkSelector "setPreferredDestinationHostName:"

-- | @Selector@ for @preferredDestinationHostPort@
preferredDestinationHostPortSelector :: Selector '[] CUShort
preferredDestinationHostPortSelector = mkSelector "preferredDestinationHostPort"

-- | @Selector@ for @setPreferredDestinationHostPort:@
setPreferredDestinationHostPortSelector :: Selector '[CUShort] ()
setPreferredDestinationHostPortSelector = mkSelector "setPreferredDestinationHostPort:"

-- | @Selector@ for @trustAccount@
trustAccountSelector :: Selector '[] (Id NSString)
trustAccountSelector = mkSelector "trustAccount"

-- | @Selector@ for @trustMetaAccount@
trustMetaAccountSelector :: Selector '[] (Id NSString)
trustMetaAccountSelector = mkSelector "trustMetaAccount"

-- | @Selector@ for @trustKerberosPrincipal@
trustKerberosPrincipalSelector :: Selector '[] (Id NSString)
trustKerberosPrincipalSelector = mkSelector "trustKerberosPrincipal"

-- | @Selector@ for @trustType@
trustTypeSelector :: Selector '[] (Id NSString)
trustTypeSelector = mkSelector "trustType"

-- | @Selector@ for @trustUsesMutualAuthentication@
trustUsesMutualAuthenticationSelector :: Selector '[] Bool
trustUsesMutualAuthenticationSelector = mkSelector "trustUsesMutualAuthentication"

-- | @Selector@ for @trustUsesKerberosKeytab@
trustUsesKerberosKeytabSelector :: Selector '[] Bool
trustUsesKerberosKeytabSelector = mkSelector "trustUsesKerberosKeytab"

-- | @Selector@ for @trustUsesSystemKeychain@
trustUsesSystemKeychainSelector :: Selector '[] Bool
trustUsesSystemKeychainSelector = mkSelector "trustUsesSystemKeychain"

-- | @Selector@ for @packetSigning@
packetSigningSelector :: Selector '[] CLong
packetSigningSelector = mkSelector "packetSigning"

-- | @Selector@ for @setPacketSigning:@
setPacketSigningSelector :: Selector '[CLong] ()
setPacketSigningSelector = mkSelector "setPacketSigning:"

-- | @Selector@ for @packetEncryption@
packetEncryptionSelector :: Selector '[] CLong
packetEncryptionSelector = mkSelector "packetEncryption"

-- | @Selector@ for @setPacketEncryption:@
setPacketEncryptionSelector :: Selector '[CLong] ()
setPacketEncryptionSelector = mkSelector "setPacketEncryption:"

-- | @Selector@ for @manInTheMiddleProtection@
manInTheMiddleProtectionSelector :: Selector '[] Bool
manInTheMiddleProtectionSelector = mkSelector "manInTheMiddleProtection"

-- | @Selector@ for @setManInTheMiddleProtection:@
setManInTheMiddleProtectionSelector :: Selector '[Bool] ()
setManInTheMiddleProtectionSelector = mkSelector "setManInTheMiddleProtection:"

-- | @Selector@ for @queryTimeoutInSeconds@
queryTimeoutInSecondsSelector :: Selector '[] CLong
queryTimeoutInSecondsSelector = mkSelector "queryTimeoutInSeconds"

-- | @Selector@ for @setQueryTimeoutInSeconds:@
setQueryTimeoutInSecondsSelector :: Selector '[CLong] ()
setQueryTimeoutInSecondsSelector = mkSelector "setQueryTimeoutInSeconds:"

-- | @Selector@ for @connectionSetupTimeoutInSeconds@
connectionSetupTimeoutInSecondsSelector :: Selector '[] CLong
connectionSetupTimeoutInSecondsSelector = mkSelector "connectionSetupTimeoutInSeconds"

-- | @Selector@ for @setConnectionSetupTimeoutInSeconds:@
setConnectionSetupTimeoutInSecondsSelector :: Selector '[CLong] ()
setConnectionSetupTimeoutInSecondsSelector = mkSelector "setConnectionSetupTimeoutInSeconds:"

-- | @Selector@ for @connectionIdleTimeoutInSeconds@
connectionIdleTimeoutInSecondsSelector :: Selector '[] CLong
connectionIdleTimeoutInSecondsSelector = mkSelector "connectionIdleTimeoutInSeconds"

-- | @Selector@ for @setConnectionIdleTimeoutInSeconds:@
setConnectionIdleTimeoutInSecondsSelector :: Selector '[CLong] ()
setConnectionIdleTimeoutInSecondsSelector = mkSelector "setConnectionIdleTimeoutInSeconds:"

-- | @Selector@ for @defaultModuleEntries@
defaultModuleEntriesSelector :: Selector '[] (Id NSArray)
defaultModuleEntriesSelector = mkSelector "defaultModuleEntries"

-- | @Selector@ for @setDefaultModuleEntries:@
setDefaultModuleEntriesSelector :: Selector '[Id NSArray] ()
setDefaultModuleEntriesSelector = mkSelector "setDefaultModuleEntries:"

-- | @Selector@ for @authenticationModuleEntries@
authenticationModuleEntriesSelector :: Selector '[] (Id NSArray)
authenticationModuleEntriesSelector = mkSelector "authenticationModuleEntries"

-- | @Selector@ for @setAuthenticationModuleEntries:@
setAuthenticationModuleEntriesSelector :: Selector '[Id NSArray] ()
setAuthenticationModuleEntriesSelector = mkSelector "setAuthenticationModuleEntries:"

-- | @Selector@ for @discoveryModuleEntries@
discoveryModuleEntriesSelector :: Selector '[] (Id NSArray)
discoveryModuleEntriesSelector = mkSelector "discoveryModuleEntries"

-- | @Selector@ for @setDiscoveryModuleEntries:@
setDiscoveryModuleEntriesSelector :: Selector '[Id NSArray] ()
setDiscoveryModuleEntriesSelector = mkSelector "setDiscoveryModuleEntries:"

-- | @Selector@ for @generalModuleEntries@
generalModuleEntriesSelector :: Selector '[] (Id NSArray)
generalModuleEntriesSelector = mkSelector "generalModuleEntries"

-- | @Selector@ for @setGeneralModuleEntries:@
setGeneralModuleEntriesSelector :: Selector '[Id NSArray] ()
setGeneralModuleEntriesSelector = mkSelector "setGeneralModuleEntries:"

