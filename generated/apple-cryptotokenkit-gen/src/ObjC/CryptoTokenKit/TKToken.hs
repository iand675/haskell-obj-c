{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing single token.  When implementing SmartCard based token, it is recommended to inherit the implementation from TKSmartCardToken.  Token object serves as synchronization point, all operations invoked upon token and all its sessions are serialized.
--
-- Generated bindings for @TKToken@.
module ObjC.CryptoTokenKit.TKToken
  ( TKToken
  , IsTKToken(..)
  , initWithTokenDriver_instanceID
  , init_
  , tokenDriver
  , delegate
  , setDelegate
  , configuration
  , keychainContents
  , configurationSelector
  , delegateSelector
  , initSelector
  , initWithTokenDriver_instanceIDSelector
  , keychainContentsSelector
  , setDelegateSelector
  , tokenDriverSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes token instance
--
-- @tokenDriver@ — Creating token driver.
--
-- @instanceID@ — Unique, persistent identifier of this token.
--
-- ObjC selector: @- initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceID :: (IsTKToken tkToken, IsTKTokenDriver tokenDriver, IsNSString instanceID) => tkToken -> tokenDriver -> instanceID -> IO (Id TKToken)
initWithTokenDriver_instanceID tkToken tokenDriver instanceID =
  sendOwnedMessage tkToken initWithTokenDriver_instanceIDSelector (toTKTokenDriver tokenDriver) (toNSString instanceID)

-- | @- init@
init_ :: IsTKToken tkToken => tkToken -> IO (Id TKToken)
init_ tkToken =
  sendOwnedMessage tkToken initSelector

-- | @- tokenDriver@
tokenDriver :: IsTKToken tkToken => tkToken -> IO (Id TKTokenDriver)
tokenDriver tkToken =
  sendMessage tkToken tokenDriverSelector

-- | @- delegate@
delegate :: IsTKToken tkToken => tkToken -> IO RawId
delegate tkToken =
  sendMessage tkToken delegateSelector

-- | @- setDelegate:@
setDelegate :: IsTKToken tkToken => tkToken -> RawId -> IO ()
setDelegate tkToken value =
  sendMessage tkToken setDelegateSelector value

-- | Token configuration associated with this token instance.
--
-- ObjC selector: @- configuration@
configuration :: IsTKToken tkToken => tkToken -> IO (Id TKTokenConfiguration)
configuration tkToken =
  sendMessage tkToken configurationSelector

-- | Keychain contents (certificate and key items) representing this token.
--
-- ObjC selector: @- keychainContents@
keychainContents :: IsTKToken tkToken => tkToken -> IO (Id TKTokenKeychainContents)
keychainContents tkToken =
  sendMessage tkToken keychainContentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceIDSelector :: Selector '[Id TKTokenDriver, Id NSString] (Id TKToken)
initWithTokenDriver_instanceIDSelector = mkSelector "initWithTokenDriver:instanceID:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKToken)
initSelector = mkSelector "init"

-- | @Selector@ for @tokenDriver@
tokenDriverSelector :: Selector '[] (Id TKTokenDriver)
tokenDriverSelector = mkSelector "tokenDriver"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id TKTokenConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @keychainContents@
keychainContentsSelector :: Selector '[] (Id TKTokenKeychainContents)
keychainContentsSelector = mkSelector "keychainContents"

