{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionKerberosMapping@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionKerberosMapping
  ( ASAuthorizationProviderExtensionKerberosMapping
  , IsASAuthorizationProviderExtensionKerberosMapping(..)
  , ticketKeyPath
  , setTicketKeyPath
  , messageBufferKeyName
  , setMessageBufferKeyName
  , realmKeyName
  , setRealmKeyName
  , serviceNameKeyName
  , setServiceNameKeyName
  , clientNameKeyName
  , setClientNameKeyName
  , encryptionKeyTypeKeyName
  , setEncryptionKeyTypeKeyName
  , sessionKeyKeyName
  , setSessionKeyKeyName
  , clientNameKeyNameSelector
  , encryptionKeyTypeKeyNameSelector
  , messageBufferKeyNameSelector
  , realmKeyNameSelector
  , serviceNameKeyNameSelector
  , sessionKeyKeyNameSelector
  , setClientNameKeyNameSelector
  , setEncryptionKeyTypeKeyNameSelector
  , setMessageBufferKeyNameSelector
  , setRealmKeyNameSelector
  , setServiceNameKeyNameSelector
  , setSessionKeyKeyNameSelector
  , setTicketKeyPathSelector
  , ticketKeyPathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The keypath in the response JSON that uses this set of mappings.
--
-- If the response tokens from login contain this keypath, then the mapping in this class will be used to create a Kerberos ticket. The expected response is a JSON dictionary with the supplied key names.
--
-- ObjC selector: @- ticketKeyPath@
ticketKeyPath :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
ticketKeyPath asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping ticketKeyPathSelector

-- | The keypath in the response JSON that uses this set of mappings.
--
-- If the response tokens from login contain this keypath, then the mapping in this class will be used to create a Kerberos ticket. The expected response is a JSON dictionary with the supplied key names.
--
-- ObjC selector: @- setTicketKeyPath:@
setTicketKeyPath :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setTicketKeyPath asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setTicketKeyPathSelector (toNSString value)

-- | The key name that contains the base64 encoded kerberos AS-REP string.
--
-- ObjC selector: @- messageBufferKeyName@
messageBufferKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
messageBufferKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping messageBufferKeyNameSelector

-- | The key name that contains the base64 encoded kerberos AS-REP string.
--
-- ObjC selector: @- setMessageBufferKeyName:@
setMessageBufferKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setMessageBufferKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setMessageBufferKeyNameSelector (toNSString value)

-- | The key name that contains the Kerberos Realm string.
--
-- ObjC selector: @- realmKeyName@
realmKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
realmKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping realmKeyNameSelector

-- | The key name that contains the Kerberos Realm string.
--
-- ObjC selector: @- setRealmKeyName:@
setRealmKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setRealmKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setRealmKeyNameSelector (toNSString value)

-- | The key name that contains the Kerberos service name string.
--
-- ObjC selector: @- serviceNameKeyName@
serviceNameKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
serviceNameKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping serviceNameKeyNameSelector

-- | The key name that contains the Kerberos service name string.
--
-- ObjC selector: @- setServiceNameKeyName:@
setServiceNameKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setServiceNameKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setServiceNameKeyNameSelector (toNSString value)

-- | The key name that contains the Kerberos client name string.
--
-- ObjC selector: @- clientNameKeyName@
clientNameKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
clientNameKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping clientNameKeyNameSelector

-- | The key name that contains the Kerberos client name string.
--
-- ObjC selector: @- setClientNameKeyName:@
setClientNameKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setClientNameKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setClientNameKeyNameSelector (toNSString value)

-- | The key name that contains the Kerberos session key type number.
--
-- The value for this key should be the correct encryption type per RFC3962, section 7 for the session key.
--
-- ObjC selector: @- encryptionKeyTypeKeyName@
encryptionKeyTypeKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
encryptionKeyTypeKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping encryptionKeyTypeKeyNameSelector

-- | The key name that contains the Kerberos session key type number.
--
-- The value for this key should be the correct encryption type per RFC3962, section 7 for the session key.
--
-- ObjC selector: @- setEncryptionKeyTypeKeyName:@
setEncryptionKeyTypeKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setEncryptionKeyTypeKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setEncryptionKeyTypeKeyNameSelector (toNSString value)

-- | The key name that contains the Kerberos session key.
--
-- ObjC selector: @- sessionKeyKeyName@
sessionKeyKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
sessionKeyKeyName asAuthorizationProviderExtensionKerberosMapping =
  sendMessage asAuthorizationProviderExtensionKerberosMapping sessionKeyKeyNameSelector

-- | The key name that contains the Kerberos session key.
--
-- ObjC selector: @- setSessionKeyKeyName:@
setSessionKeyKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setSessionKeyKeyName asAuthorizationProviderExtensionKerberosMapping value =
  sendMessage asAuthorizationProviderExtensionKerberosMapping setSessionKeyKeyNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ticketKeyPath@
ticketKeyPathSelector :: Selector '[] (Id NSString)
ticketKeyPathSelector = mkSelector "ticketKeyPath"

-- | @Selector@ for @setTicketKeyPath:@
setTicketKeyPathSelector :: Selector '[Id NSString] ()
setTicketKeyPathSelector = mkSelector "setTicketKeyPath:"

-- | @Selector@ for @messageBufferKeyName@
messageBufferKeyNameSelector :: Selector '[] (Id NSString)
messageBufferKeyNameSelector = mkSelector "messageBufferKeyName"

-- | @Selector@ for @setMessageBufferKeyName:@
setMessageBufferKeyNameSelector :: Selector '[Id NSString] ()
setMessageBufferKeyNameSelector = mkSelector "setMessageBufferKeyName:"

-- | @Selector@ for @realmKeyName@
realmKeyNameSelector :: Selector '[] (Id NSString)
realmKeyNameSelector = mkSelector "realmKeyName"

-- | @Selector@ for @setRealmKeyName:@
setRealmKeyNameSelector :: Selector '[Id NSString] ()
setRealmKeyNameSelector = mkSelector "setRealmKeyName:"

-- | @Selector@ for @serviceNameKeyName@
serviceNameKeyNameSelector :: Selector '[] (Id NSString)
serviceNameKeyNameSelector = mkSelector "serviceNameKeyName"

-- | @Selector@ for @setServiceNameKeyName:@
setServiceNameKeyNameSelector :: Selector '[Id NSString] ()
setServiceNameKeyNameSelector = mkSelector "setServiceNameKeyName:"

-- | @Selector@ for @clientNameKeyName@
clientNameKeyNameSelector :: Selector '[] (Id NSString)
clientNameKeyNameSelector = mkSelector "clientNameKeyName"

-- | @Selector@ for @setClientNameKeyName:@
setClientNameKeyNameSelector :: Selector '[Id NSString] ()
setClientNameKeyNameSelector = mkSelector "setClientNameKeyName:"

-- | @Selector@ for @encryptionKeyTypeKeyName@
encryptionKeyTypeKeyNameSelector :: Selector '[] (Id NSString)
encryptionKeyTypeKeyNameSelector = mkSelector "encryptionKeyTypeKeyName"

-- | @Selector@ for @setEncryptionKeyTypeKeyName:@
setEncryptionKeyTypeKeyNameSelector :: Selector '[Id NSString] ()
setEncryptionKeyTypeKeyNameSelector = mkSelector "setEncryptionKeyTypeKeyName:"

-- | @Selector@ for @sessionKeyKeyName@
sessionKeyKeyNameSelector :: Selector '[] (Id NSString)
sessionKeyKeyNameSelector = mkSelector "sessionKeyKeyName"

-- | @Selector@ for @setSessionKeyKeyName:@
setSessionKeyKeyNameSelector :: Selector '[Id NSString] ()
setSessionKeyKeyNameSelector = mkSelector "setSessionKeyKeyName:"

