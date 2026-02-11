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
  , ticketKeyPathSelector
  , setTicketKeyPathSelector
  , messageBufferKeyNameSelector
  , setMessageBufferKeyNameSelector
  , realmKeyNameSelector
  , setRealmKeyNameSelector
  , serviceNameKeyNameSelector
  , setServiceNameKeyNameSelector
  , clientNameKeyNameSelector
  , setClientNameKeyNameSelector
  , encryptionKeyTypeKeyNameSelector
  , setEncryptionKeyTypeKeyNameSelector
  , sessionKeyKeyNameSelector
  , setSessionKeyKeyNameSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The keypath in the response JSON that uses this set of mappings.
--
-- If the response tokens from login contain this keypath, then the mapping in this class will be used to create a Kerberos ticket. The expected response is a JSON dictionary with the supplied key names.
--
-- ObjC selector: @- ticketKeyPath@
ticketKeyPath :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
ticketKeyPath asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "ticketKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The keypath in the response JSON that uses this set of mappings.
--
-- If the response tokens from login contain this keypath, then the mapping in this class will be used to create a Kerberos ticket. The expected response is a JSON dictionary with the supplied key names.
--
-- ObjC selector: @- setTicketKeyPath:@
setTicketKeyPath :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setTicketKeyPath asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setTicketKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the base64 encoded kerberos AS-REP string.
--
-- ObjC selector: @- messageBufferKeyName@
messageBufferKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
messageBufferKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "messageBufferKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the base64 encoded kerberos AS-REP string.
--
-- ObjC selector: @- setMessageBufferKeyName:@
setMessageBufferKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setMessageBufferKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setMessageBufferKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the Kerberos Realm string.
--
-- ObjC selector: @- realmKeyName@
realmKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
realmKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "realmKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the Kerberos Realm string.
--
-- ObjC selector: @- setRealmKeyName:@
setRealmKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setRealmKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setRealmKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the Kerberos service name string.
--
-- ObjC selector: @- serviceNameKeyName@
serviceNameKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
serviceNameKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "serviceNameKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the Kerberos service name string.
--
-- ObjC selector: @- setServiceNameKeyName:@
setServiceNameKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setServiceNameKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setServiceNameKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the Kerberos client name string.
--
-- ObjC selector: @- clientNameKeyName@
clientNameKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
clientNameKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "clientNameKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the Kerberos client name string.
--
-- ObjC selector: @- setClientNameKeyName:@
setClientNameKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setClientNameKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setClientNameKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the Kerberos session key type number.
--
-- The value for this key should be the correct encryption type per RFC3962, section 7 for the session key.
--
-- ObjC selector: @- encryptionKeyTypeKeyName@
encryptionKeyTypeKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
encryptionKeyTypeKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "encryptionKeyTypeKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the Kerberos session key type number.
--
-- The value for this key should be the correct encryption type per RFC3962, section 7 for the session key.
--
-- ObjC selector: @- setEncryptionKeyTypeKeyName:@
setEncryptionKeyTypeKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setEncryptionKeyTypeKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setEncryptionKeyTypeKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The key name that contains the Kerberos session key.
--
-- ObjC selector: @- sessionKeyKeyName@
sessionKeyKeyName :: IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping => asAuthorizationProviderExtensionKerberosMapping -> IO (Id NSString)
sessionKeyKeyName asAuthorizationProviderExtensionKerberosMapping  =
  sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "sessionKeyKeyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The key name that contains the Kerberos session key.
--
-- ObjC selector: @- setSessionKeyKeyName:@
setSessionKeyKeyName :: (IsASAuthorizationProviderExtensionKerberosMapping asAuthorizationProviderExtensionKerberosMapping, IsNSString value) => asAuthorizationProviderExtensionKerberosMapping -> value -> IO ()
setSessionKeyKeyName asAuthorizationProviderExtensionKerberosMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionKerberosMapping (mkSelector "setSessionKeyKeyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ticketKeyPath@
ticketKeyPathSelector :: Selector
ticketKeyPathSelector = mkSelector "ticketKeyPath"

-- | @Selector@ for @setTicketKeyPath:@
setTicketKeyPathSelector :: Selector
setTicketKeyPathSelector = mkSelector "setTicketKeyPath:"

-- | @Selector@ for @messageBufferKeyName@
messageBufferKeyNameSelector :: Selector
messageBufferKeyNameSelector = mkSelector "messageBufferKeyName"

-- | @Selector@ for @setMessageBufferKeyName:@
setMessageBufferKeyNameSelector :: Selector
setMessageBufferKeyNameSelector = mkSelector "setMessageBufferKeyName:"

-- | @Selector@ for @realmKeyName@
realmKeyNameSelector :: Selector
realmKeyNameSelector = mkSelector "realmKeyName"

-- | @Selector@ for @setRealmKeyName:@
setRealmKeyNameSelector :: Selector
setRealmKeyNameSelector = mkSelector "setRealmKeyName:"

-- | @Selector@ for @serviceNameKeyName@
serviceNameKeyNameSelector :: Selector
serviceNameKeyNameSelector = mkSelector "serviceNameKeyName"

-- | @Selector@ for @setServiceNameKeyName:@
setServiceNameKeyNameSelector :: Selector
setServiceNameKeyNameSelector = mkSelector "setServiceNameKeyName:"

-- | @Selector@ for @clientNameKeyName@
clientNameKeyNameSelector :: Selector
clientNameKeyNameSelector = mkSelector "clientNameKeyName"

-- | @Selector@ for @setClientNameKeyName:@
setClientNameKeyNameSelector :: Selector
setClientNameKeyNameSelector = mkSelector "setClientNameKeyName:"

-- | @Selector@ for @encryptionKeyTypeKeyName@
encryptionKeyTypeKeyNameSelector :: Selector
encryptionKeyTypeKeyNameSelector = mkSelector "encryptionKeyTypeKeyName"

-- | @Selector@ for @setEncryptionKeyTypeKeyName:@
setEncryptionKeyTypeKeyNameSelector :: Selector
setEncryptionKeyTypeKeyNameSelector = mkSelector "setEncryptionKeyTypeKeyName:"

-- | @Selector@ for @sessionKeyKeyName@
sessionKeyKeyNameSelector :: Selector
sessionKeyKeyNameSelector = mkSelector "sessionKeyKeyName"

-- | @Selector@ for @setSessionKeyKeyName:@
setSessionKeyKeyNameSelector :: Selector
setSessionKeyKeyNameSelector = mkSelector "setSessionKeyKeyName:"

