{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenSession represents token session which shares authentication status.
--
-- Token implementation must inherit its own session implementation from TKTokenSession (or its subclass TKSmartCardTokenSession in case of SmartCard tokens).
--
-- TKTokenSession should keep an authentication state of the token.  Authentication status (e.g. entered PIN to unlock SmartCard) should not be shared across borders of single TKTokenSession instance.
--
-- TKTokenSession is always instantiated by TKToken when framework detects access to the token from new authentication session.
--
-- Generated bindings for @TKTokenSession@.
module ObjC.CryptoTokenKit.TKTokenSession
  ( TKTokenSession
  , IsTKTokenSession(..)
  , initWithToken
  , init_
  , token
  , delegate
  , setDelegate
  , initWithTokenSelector
  , initSelector
  , tokenSelector
  , delegateSelector
  , setDelegateSelector


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

-- | @token@ — Token instance to which is this session instance bound.
--
-- ObjC selector: @- initWithToken:@
initWithToken :: (IsTKTokenSession tkTokenSession, IsTKToken token) => tkTokenSession -> token -> IO (Id TKTokenSession)
initWithToken tkTokenSession  token =
  withObjCPtr token $ \raw_token ->
      sendMsg tkTokenSession (mkSelector "initWithToken:") (retPtr retVoid) [argPtr (castPtr raw_token :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO (Id TKTokenSession)
init_ tkTokenSession  =
    sendMsg tkTokenSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- token@
token :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO (Id TKToken)
token tkTokenSession  =
    sendMsg tkTokenSession (mkSelector "token") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO RawId
delegate tkTokenSession  =
    fmap (RawId . castPtr) $ sendMsg tkTokenSession (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsTKTokenSession tkTokenSession => tkTokenSession -> RawId -> IO ()
setDelegate tkTokenSession  value =
    sendMsg tkTokenSession (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithToken:@
initWithTokenSelector :: Selector
initWithTokenSelector = mkSelector "initWithToken:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @token@
tokenSelector :: Selector
tokenSelector = mkSelector "token"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

