{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NEDNSOverTLSSettings@.
module ObjC.NetworkExtension.NEDNSOverTLSSettings
  ( NEDNSOverTLSSettings
  , IsNEDNSOverTLSSettings(..)
  , serverName
  , setServerName
  , identityReference
  , setIdentityReference
  , serverNameSelector
  , setServerNameSelector
  , identityReferenceSelector
  , setIdentityReferenceSelector


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

-- | serverName
--
-- The name of the server to use for TLS certificate validation.
--
-- ObjC selector: @- serverName@
serverName :: IsNEDNSOverTLSSettings nednsOverTLSSettings => nednsOverTLSSettings -> IO (Id NSString)
serverName nednsOverTLSSettings  =
    sendMsg nednsOverTLSSettings (mkSelector "serverName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverName
--
-- The name of the server to use for TLS certificate validation.
--
-- ObjC selector: @- setServerName:@
setServerName :: (IsNEDNSOverTLSSettings nednsOverTLSSettings, IsNSString value) => nednsOverTLSSettings -> value -> IO ()
setServerName nednsOverTLSSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsOverTLSSettings (mkSelector "setServerName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEDNSOverTLSSettings nednsOverTLSSettings => nednsOverTLSSettings -> IO (Id NSData)
identityReference nednsOverTLSSettings  =
    sendMsg nednsOverTLSSettings (mkSelector "identityReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEDNSOverTLSSettings nednsOverTLSSettings, IsNSData value) => nednsOverTLSSettings -> value -> IO ()
setIdentityReference nednsOverTLSSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsOverTLSSettings (mkSelector "setIdentityReference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serverName@
serverNameSelector :: Selector
serverNameSelector = mkSelector "serverName"

-- | @Selector@ for @setServerName:@
setServerNameSelector :: Selector
setServerNameSelector = mkSelector "setServerName:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

