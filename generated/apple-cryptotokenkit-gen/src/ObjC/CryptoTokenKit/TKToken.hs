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
  , initWithTokenDriver_instanceIDSelector
  , initSelector
  , tokenDriverSelector
  , delegateSelector
  , setDelegateSelector
  , configurationSelector
  , keychainContentsSelector


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

-- | Initializes token instance
--
-- @tokenDriver@ — Creating token driver.
--
-- @instanceID@ — Unique, persistent identifier of this token.
--
-- ObjC selector: @- initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceID :: (IsTKToken tkToken, IsTKTokenDriver tokenDriver, IsNSString instanceID) => tkToken -> tokenDriver -> instanceID -> IO (Id TKToken)
initWithTokenDriver_instanceID tkToken  tokenDriver instanceID =
  withObjCPtr tokenDriver $ \raw_tokenDriver ->
    withObjCPtr instanceID $ \raw_instanceID ->
        sendMsg tkToken (mkSelector "initWithTokenDriver:instanceID:") (retPtr retVoid) [argPtr (castPtr raw_tokenDriver :: Ptr ()), argPtr (castPtr raw_instanceID :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsTKToken tkToken => tkToken -> IO (Id TKToken)
init_ tkToken  =
    sendMsg tkToken (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- tokenDriver@
tokenDriver :: IsTKToken tkToken => tkToken -> IO (Id TKTokenDriver)
tokenDriver tkToken  =
    sendMsg tkToken (mkSelector "tokenDriver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsTKToken tkToken => tkToken -> IO RawId
delegate tkToken  =
    fmap (RawId . castPtr) $ sendMsg tkToken (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsTKToken tkToken => tkToken -> RawId -> IO ()
setDelegate tkToken  value =
    sendMsg tkToken (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Token configuration associated with this token instance.
--
-- ObjC selector: @- configuration@
configuration :: IsTKToken tkToken => tkToken -> IO (Id TKTokenConfiguration)
configuration tkToken  =
    sendMsg tkToken (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Keychain contents (certificate and key items) representing this token.
--
-- ObjC selector: @- keychainContents@
keychainContents :: IsTKToken tkToken => tkToken -> IO (Id TKTokenKeychainContents)
keychainContents tkToken  =
    sendMsg tkToken (mkSelector "keychainContents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTokenDriver:instanceID:@
initWithTokenDriver_instanceIDSelector :: Selector
initWithTokenDriver_instanceIDSelector = mkSelector "initWithTokenDriver:instanceID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @tokenDriver@
tokenDriverSelector :: Selector
tokenDriverSelector = mkSelector "tokenDriver"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @keychainContents@
keychainContentsSelector :: Selector
keychainContentsSelector = mkSelector "keychainContents"

