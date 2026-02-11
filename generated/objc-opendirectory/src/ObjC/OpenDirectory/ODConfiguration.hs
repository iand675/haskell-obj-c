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
  , configurationSelector
  , suggestedTrustAccountSelector
  , suggestedTrustPasswordSelector
  , saveUsingAuthorization_errorSelector
  , addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector
  , removeTrustUsingUsername_password_deleteTrustAccount_errorSelector
  , nodeNameSelector
  , setNodeNameSelector
  , commentSelector
  , setCommentSelector
  , defaultMappingsSelector
  , setDefaultMappingsSelector
  , templateNameSelector
  , setTemplateNameSelector
  , virtualSubnodesSelector
  , setVirtualSubnodesSelector
  , hideRegistrationSelector
  , setHideRegistrationSelector
  , preferredDestinationHostNameSelector
  , setPreferredDestinationHostNameSelector
  , preferredDestinationHostPortSelector
  , setPreferredDestinationHostPortSelector
  , trustAccountSelector
  , trustMetaAccountSelector
  , trustKerberosPrincipalSelector
  , trustTypeSelector
  , trustUsesMutualAuthenticationSelector
  , trustUsesKerberosKeytabSelector
  , trustUsesSystemKeychainSelector
  , packetSigningSelector
  , setPacketSigningSelector
  , packetEncryptionSelector
  , setPacketEncryptionSelector
  , manInTheMiddleProtectionSelector
  , setManInTheMiddleProtectionSelector
  , queryTimeoutInSecondsSelector
  , setQueryTimeoutInSecondsSelector
  , connectionSetupTimeoutInSecondsSelector
  , setConnectionSetupTimeoutInSecondsSelector
  , connectionIdleTimeoutInSecondsSelector
  , setConnectionIdleTimeoutInSecondsSelector
  , defaultModuleEntriesSelector
  , setDefaultModuleEntriesSelector
  , authenticationModuleEntriesSelector
  , setAuthenticationModuleEntriesSelector
  , discoveryModuleEntriesSelector
  , setDiscoveryModuleEntriesSelector
  , generalModuleEntriesSelector
  , setGeneralModuleEntriesSelector


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
    sendClassMsg cls' (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr hostname $ \raw_hostname ->
      sendClassMsg cls' (mkSelector "suggestedTrustAccount:") (retPtr retVoid) [argPtr (castPtr raw_hostname :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "suggestedTrustPassword:") (retPtr retVoid) [argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | saveUsingAuthorization:error:
--
-- Saves the configuration using the provided authorization.
--
-- Saves the configuration using the provided authorization.
--
-- ObjC selector: @- saveUsingAuthorization:error:@
saveUsingAuthorization_error :: (IsODConfiguration odConfiguration, IsSFAuthorization authorization, IsNSError error_) => odConfiguration -> authorization -> error_ -> IO Bool
saveUsingAuthorization_error odConfiguration  authorization error_ =
withObjCPtr authorization $ \raw_authorization ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "saveUsingAuthorization:error:") retCULong [argPtr (castPtr raw_authorization :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:
--
-- Adds a trust account with the provided name and password using the credentials provided by the user.
--
-- Adds a trust account with the provided name and password using the credentials provided by the user.  User can request that the trust be forcibly created (replacing existing trust if found in directory).  A trust should be established only after enough configuration is available and the configuration been saved.  If the trust is required, then the configuration can be deleted if necessary upon failure.
--
-- ObjC selector: @- addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:@
addTrustType_trustAccount_trustPassword_username_password_joinExisting_error :: (IsODConfiguration odConfiguration, IsNSString trustType, IsNSString account, IsNSString accountPassword, IsNSString username, IsNSString password, IsNSError error_) => odConfiguration -> trustType -> account -> accountPassword -> username -> password -> Bool -> error_ -> IO Bool
addTrustType_trustAccount_trustPassword_username_password_joinExisting_error odConfiguration  trustType account accountPassword username password join error_ =
withObjCPtr trustType $ \raw_trustType ->
  withObjCPtr account $ \raw_account ->
    withObjCPtr accountPassword $ \raw_accountPassword ->
      withObjCPtr username $ \raw_username ->
        withObjCPtr password $ \raw_password ->
          withObjCPtr error_ $ \raw_error_ ->
              fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:") retCULong [argPtr (castPtr raw_trustType :: Ptr ()), argPtr (castPtr raw_account :: Ptr ()), argPtr (castPtr raw_accountPassword :: Ptr ()), argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_password :: Ptr ()), argCULong (if join then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeTrustUsingUsername:password:deleteTrustAccount:error:
--
-- Removes trust using the provided username and password.
--
-- Removes trust using the provided username and password.  The trust account will be removed from the directory only if requested.
--
-- ObjC selector: @- removeTrustUsingUsername:password:deleteTrustAccount:error:@
removeTrustUsingUsername_password_deleteTrustAccount_error :: (IsODConfiguration odConfiguration, IsNSString username, IsNSString password, IsNSError error_) => odConfiguration -> username -> password -> Bool -> error_ -> IO Bool
removeTrustUsingUsername_password_deleteTrustAccount_error odConfiguration  username password deleteAccount error_ =
withObjCPtr username $ \raw_username ->
  withObjCPtr password $ \raw_password ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "removeTrustUsingUsername:password:deleteTrustAccount:error:") retCULong [argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_password :: Ptr ()), argCULong (if deleteAccount then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- nodeName@
nodeName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
nodeName odConfiguration  =
  sendMsg odConfiguration (mkSelector "nodeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeName:@
setNodeName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setNodeName odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setNodeName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- comment@
comment :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
comment odConfiguration  =
  sendMsg odConfiguration (mkSelector "comment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setComment:@
setComment :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setComment odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setComment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultMappings@
defaultMappings :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id ODMappings)
defaultMappings odConfiguration  =
  sendMsg odConfiguration (mkSelector "defaultMappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultMappings:@
setDefaultMappings :: (IsODConfiguration odConfiguration, IsODMappings value) => odConfiguration -> value -> IO ()
setDefaultMappings odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setDefaultMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- templateName@
templateName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
templateName odConfiguration  =
  sendMsg odConfiguration (mkSelector "templateName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTemplateName:@
setTemplateName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setTemplateName odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setTemplateName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- virtualSubnodes@
virtualSubnodes :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
virtualSubnodes odConfiguration  =
  sendMsg odConfiguration (mkSelector "virtualSubnodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVirtualSubnodes:@
setVirtualSubnodes :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setVirtualSubnodes odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setVirtualSubnodes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hideRegistration@
hideRegistration :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
hideRegistration odConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "hideRegistration") retCULong []

-- | @- setHideRegistration:@
setHideRegistration :: IsODConfiguration odConfiguration => odConfiguration -> Bool -> IO ()
setHideRegistration odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setHideRegistration:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredDestinationHostName@
preferredDestinationHostName :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
preferredDestinationHostName odConfiguration  =
  sendMsg odConfiguration (mkSelector "preferredDestinationHostName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreferredDestinationHostName:@
setPreferredDestinationHostName :: (IsODConfiguration odConfiguration, IsNSString value) => odConfiguration -> value -> IO ()
setPreferredDestinationHostName odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setPreferredDestinationHostName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- preferredDestinationHostPort@
preferredDestinationHostPort :: IsODConfiguration odConfiguration => odConfiguration -> IO CUShort
preferredDestinationHostPort odConfiguration  =
  fmap fromIntegral $ sendMsg odConfiguration (mkSelector "preferredDestinationHostPort") retCUInt []

-- | @- setPreferredDestinationHostPort:@
setPreferredDestinationHostPort :: IsODConfiguration odConfiguration => odConfiguration -> CUShort -> IO ()
setPreferredDestinationHostPort odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setPreferredDestinationHostPort:") retVoid [argCUInt (fromIntegral value)]

-- | @- trustAccount@
trustAccount :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustAccount odConfiguration  =
  sendMsg odConfiguration (mkSelector "trustAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trustMetaAccount@
trustMetaAccount :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustMetaAccount odConfiguration  =
  sendMsg odConfiguration (mkSelector "trustMetaAccount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trustKerberosPrincipal@
trustKerberosPrincipal :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustKerberosPrincipal odConfiguration  =
  sendMsg odConfiguration (mkSelector "trustKerberosPrincipal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trustType@
trustType :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSString)
trustType odConfiguration  =
  sendMsg odConfiguration (mkSelector "trustType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trustUsesMutualAuthentication@
trustUsesMutualAuthentication :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesMutualAuthentication odConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "trustUsesMutualAuthentication") retCULong []

-- | @- trustUsesKerberosKeytab@
trustUsesKerberosKeytab :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesKerberosKeytab odConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "trustUsesKerberosKeytab") retCULong []

-- | @- trustUsesSystemKeychain@
trustUsesSystemKeychain :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
trustUsesSystemKeychain odConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "trustUsesSystemKeychain") retCULong []

-- | @- packetSigning@
packetSigning :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
packetSigning odConfiguration  =
  sendMsg odConfiguration (mkSelector "packetSigning") retCLong []

-- | @- setPacketSigning:@
setPacketSigning :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setPacketSigning odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setPacketSigning:") retVoid [argCLong (fromIntegral value)]

-- | @- packetEncryption@
packetEncryption :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
packetEncryption odConfiguration  =
  sendMsg odConfiguration (mkSelector "packetEncryption") retCLong []

-- | @- setPacketEncryption:@
setPacketEncryption :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setPacketEncryption odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setPacketEncryption:") retVoid [argCLong (fromIntegral value)]

-- | @- manInTheMiddleProtection@
manInTheMiddleProtection :: IsODConfiguration odConfiguration => odConfiguration -> IO Bool
manInTheMiddleProtection odConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg odConfiguration (mkSelector "manInTheMiddleProtection") retCULong []

-- | @- setManInTheMiddleProtection:@
setManInTheMiddleProtection :: IsODConfiguration odConfiguration => odConfiguration -> Bool -> IO ()
setManInTheMiddleProtection odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setManInTheMiddleProtection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- queryTimeoutInSeconds@
queryTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
queryTimeoutInSeconds odConfiguration  =
  sendMsg odConfiguration (mkSelector "queryTimeoutInSeconds") retCLong []

-- | @- setQueryTimeoutInSeconds:@
setQueryTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setQueryTimeoutInSeconds odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setQueryTimeoutInSeconds:") retVoid [argCLong (fromIntegral value)]

-- | @- connectionSetupTimeoutInSeconds@
connectionSetupTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
connectionSetupTimeoutInSeconds odConfiguration  =
  sendMsg odConfiguration (mkSelector "connectionSetupTimeoutInSeconds") retCLong []

-- | @- setConnectionSetupTimeoutInSeconds:@
setConnectionSetupTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setConnectionSetupTimeoutInSeconds odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setConnectionSetupTimeoutInSeconds:") retVoid [argCLong (fromIntegral value)]

-- | @- connectionIdleTimeoutInSeconds@
connectionIdleTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> IO CLong
connectionIdleTimeoutInSeconds odConfiguration  =
  sendMsg odConfiguration (mkSelector "connectionIdleTimeoutInSeconds") retCLong []

-- | @- setConnectionIdleTimeoutInSeconds:@
setConnectionIdleTimeoutInSeconds :: IsODConfiguration odConfiguration => odConfiguration -> CLong -> IO ()
setConnectionIdleTimeoutInSeconds odConfiguration  value =
  sendMsg odConfiguration (mkSelector "setConnectionIdleTimeoutInSeconds:") retVoid [argCLong (fromIntegral value)]

-- | @- defaultModuleEntries@
defaultModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
defaultModuleEntries odConfiguration  =
  sendMsg odConfiguration (mkSelector "defaultModuleEntries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultModuleEntries:@
setDefaultModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setDefaultModuleEntries odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setDefaultModuleEntries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authenticationModuleEntries@
authenticationModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
authenticationModuleEntries odConfiguration  =
  sendMsg odConfiguration (mkSelector "authenticationModuleEntries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthenticationModuleEntries:@
setAuthenticationModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setAuthenticationModuleEntries odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setAuthenticationModuleEntries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- discoveryModuleEntries@
discoveryModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
discoveryModuleEntries odConfiguration  =
  sendMsg odConfiguration (mkSelector "discoveryModuleEntries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDiscoveryModuleEntries:@
setDiscoveryModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setDiscoveryModuleEntries odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setDiscoveryModuleEntries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- generalModuleEntries@
generalModuleEntries :: IsODConfiguration odConfiguration => odConfiguration -> IO (Id NSArray)
generalModuleEntries odConfiguration  =
  sendMsg odConfiguration (mkSelector "generalModuleEntries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGeneralModuleEntries:@
setGeneralModuleEntries :: (IsODConfiguration odConfiguration, IsNSArray value) => odConfiguration -> value -> IO ()
setGeneralModuleEntries odConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg odConfiguration (mkSelector "setGeneralModuleEntries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @suggestedTrustAccount:@
suggestedTrustAccountSelector :: Selector
suggestedTrustAccountSelector = mkSelector "suggestedTrustAccount:"

-- | @Selector@ for @suggestedTrustPassword:@
suggestedTrustPasswordSelector :: Selector
suggestedTrustPasswordSelector = mkSelector "suggestedTrustPassword:"

-- | @Selector@ for @saveUsingAuthorization:error:@
saveUsingAuthorization_errorSelector :: Selector
saveUsingAuthorization_errorSelector = mkSelector "saveUsingAuthorization:error:"

-- | @Selector@ for @addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:@
addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector :: Selector
addTrustType_trustAccount_trustPassword_username_password_joinExisting_errorSelector = mkSelector "addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:"

-- | @Selector@ for @removeTrustUsingUsername:password:deleteTrustAccount:error:@
removeTrustUsingUsername_password_deleteTrustAccount_errorSelector :: Selector
removeTrustUsingUsername_password_deleteTrustAccount_errorSelector = mkSelector "removeTrustUsingUsername:password:deleteTrustAccount:error:"

-- | @Selector@ for @nodeName@
nodeNameSelector :: Selector
nodeNameSelector = mkSelector "nodeName"

-- | @Selector@ for @setNodeName:@
setNodeNameSelector :: Selector
setNodeNameSelector = mkSelector "setNodeName:"

-- | @Selector@ for @comment@
commentSelector :: Selector
commentSelector = mkSelector "comment"

-- | @Selector@ for @setComment:@
setCommentSelector :: Selector
setCommentSelector = mkSelector "setComment:"

-- | @Selector@ for @defaultMappings@
defaultMappingsSelector :: Selector
defaultMappingsSelector = mkSelector "defaultMappings"

-- | @Selector@ for @setDefaultMappings:@
setDefaultMappingsSelector :: Selector
setDefaultMappingsSelector = mkSelector "setDefaultMappings:"

-- | @Selector@ for @templateName@
templateNameSelector :: Selector
templateNameSelector = mkSelector "templateName"

-- | @Selector@ for @setTemplateName:@
setTemplateNameSelector :: Selector
setTemplateNameSelector = mkSelector "setTemplateName:"

-- | @Selector@ for @virtualSubnodes@
virtualSubnodesSelector :: Selector
virtualSubnodesSelector = mkSelector "virtualSubnodes"

-- | @Selector@ for @setVirtualSubnodes:@
setVirtualSubnodesSelector :: Selector
setVirtualSubnodesSelector = mkSelector "setVirtualSubnodes:"

-- | @Selector@ for @hideRegistration@
hideRegistrationSelector :: Selector
hideRegistrationSelector = mkSelector "hideRegistration"

-- | @Selector@ for @setHideRegistration:@
setHideRegistrationSelector :: Selector
setHideRegistrationSelector = mkSelector "setHideRegistration:"

-- | @Selector@ for @preferredDestinationHostName@
preferredDestinationHostNameSelector :: Selector
preferredDestinationHostNameSelector = mkSelector "preferredDestinationHostName"

-- | @Selector@ for @setPreferredDestinationHostName:@
setPreferredDestinationHostNameSelector :: Selector
setPreferredDestinationHostNameSelector = mkSelector "setPreferredDestinationHostName:"

-- | @Selector@ for @preferredDestinationHostPort@
preferredDestinationHostPortSelector :: Selector
preferredDestinationHostPortSelector = mkSelector "preferredDestinationHostPort"

-- | @Selector@ for @setPreferredDestinationHostPort:@
setPreferredDestinationHostPortSelector :: Selector
setPreferredDestinationHostPortSelector = mkSelector "setPreferredDestinationHostPort:"

-- | @Selector@ for @trustAccount@
trustAccountSelector :: Selector
trustAccountSelector = mkSelector "trustAccount"

-- | @Selector@ for @trustMetaAccount@
trustMetaAccountSelector :: Selector
trustMetaAccountSelector = mkSelector "trustMetaAccount"

-- | @Selector@ for @trustKerberosPrincipal@
trustKerberosPrincipalSelector :: Selector
trustKerberosPrincipalSelector = mkSelector "trustKerberosPrincipal"

-- | @Selector@ for @trustType@
trustTypeSelector :: Selector
trustTypeSelector = mkSelector "trustType"

-- | @Selector@ for @trustUsesMutualAuthentication@
trustUsesMutualAuthenticationSelector :: Selector
trustUsesMutualAuthenticationSelector = mkSelector "trustUsesMutualAuthentication"

-- | @Selector@ for @trustUsesKerberosKeytab@
trustUsesKerberosKeytabSelector :: Selector
trustUsesKerberosKeytabSelector = mkSelector "trustUsesKerberosKeytab"

-- | @Selector@ for @trustUsesSystemKeychain@
trustUsesSystemKeychainSelector :: Selector
trustUsesSystemKeychainSelector = mkSelector "trustUsesSystemKeychain"

-- | @Selector@ for @packetSigning@
packetSigningSelector :: Selector
packetSigningSelector = mkSelector "packetSigning"

-- | @Selector@ for @setPacketSigning:@
setPacketSigningSelector :: Selector
setPacketSigningSelector = mkSelector "setPacketSigning:"

-- | @Selector@ for @packetEncryption@
packetEncryptionSelector :: Selector
packetEncryptionSelector = mkSelector "packetEncryption"

-- | @Selector@ for @setPacketEncryption:@
setPacketEncryptionSelector :: Selector
setPacketEncryptionSelector = mkSelector "setPacketEncryption:"

-- | @Selector@ for @manInTheMiddleProtection@
manInTheMiddleProtectionSelector :: Selector
manInTheMiddleProtectionSelector = mkSelector "manInTheMiddleProtection"

-- | @Selector@ for @setManInTheMiddleProtection:@
setManInTheMiddleProtectionSelector :: Selector
setManInTheMiddleProtectionSelector = mkSelector "setManInTheMiddleProtection:"

-- | @Selector@ for @queryTimeoutInSeconds@
queryTimeoutInSecondsSelector :: Selector
queryTimeoutInSecondsSelector = mkSelector "queryTimeoutInSeconds"

-- | @Selector@ for @setQueryTimeoutInSeconds:@
setQueryTimeoutInSecondsSelector :: Selector
setQueryTimeoutInSecondsSelector = mkSelector "setQueryTimeoutInSeconds:"

-- | @Selector@ for @connectionSetupTimeoutInSeconds@
connectionSetupTimeoutInSecondsSelector :: Selector
connectionSetupTimeoutInSecondsSelector = mkSelector "connectionSetupTimeoutInSeconds"

-- | @Selector@ for @setConnectionSetupTimeoutInSeconds:@
setConnectionSetupTimeoutInSecondsSelector :: Selector
setConnectionSetupTimeoutInSecondsSelector = mkSelector "setConnectionSetupTimeoutInSeconds:"

-- | @Selector@ for @connectionIdleTimeoutInSeconds@
connectionIdleTimeoutInSecondsSelector :: Selector
connectionIdleTimeoutInSecondsSelector = mkSelector "connectionIdleTimeoutInSeconds"

-- | @Selector@ for @setConnectionIdleTimeoutInSeconds:@
setConnectionIdleTimeoutInSecondsSelector :: Selector
setConnectionIdleTimeoutInSecondsSelector = mkSelector "setConnectionIdleTimeoutInSeconds:"

-- | @Selector@ for @defaultModuleEntries@
defaultModuleEntriesSelector :: Selector
defaultModuleEntriesSelector = mkSelector "defaultModuleEntries"

-- | @Selector@ for @setDefaultModuleEntries:@
setDefaultModuleEntriesSelector :: Selector
setDefaultModuleEntriesSelector = mkSelector "setDefaultModuleEntries:"

-- | @Selector@ for @authenticationModuleEntries@
authenticationModuleEntriesSelector :: Selector
authenticationModuleEntriesSelector = mkSelector "authenticationModuleEntries"

-- | @Selector@ for @setAuthenticationModuleEntries:@
setAuthenticationModuleEntriesSelector :: Selector
setAuthenticationModuleEntriesSelector = mkSelector "setAuthenticationModuleEntries:"

-- | @Selector@ for @discoveryModuleEntries@
discoveryModuleEntriesSelector :: Selector
discoveryModuleEntriesSelector = mkSelector "discoveryModuleEntries"

-- | @Selector@ for @setDiscoveryModuleEntries:@
setDiscoveryModuleEntriesSelector :: Selector
setDiscoveryModuleEntriesSelector = mkSelector "setDiscoveryModuleEntries:"

-- | @Selector@ for @generalModuleEntries@
generalModuleEntriesSelector :: Selector
generalModuleEntriesSelector = mkSelector "generalModuleEntries"

-- | @Selector@ for @setGeneralModuleEntries:@
setGeneralModuleEntriesSelector :: Selector
setGeneralModuleEntriesSelector = mkSelector "setGeneralModuleEntries:"

