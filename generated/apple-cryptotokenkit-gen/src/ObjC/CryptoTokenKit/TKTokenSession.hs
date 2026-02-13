{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , initSelector
  , initWithTokenSelector
  , setDelegateSelector
  , tokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @token@ — Token instance to which is this session instance bound.
--
-- ObjC selector: @- initWithToken:@
initWithToken :: (IsTKTokenSession tkTokenSession, IsTKToken token) => tkTokenSession -> token -> IO (Id TKTokenSession)
initWithToken tkTokenSession token =
  sendOwnedMessage tkTokenSession initWithTokenSelector (toTKToken token)

-- | @- init@
init_ :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO (Id TKTokenSession)
init_ tkTokenSession =
  sendOwnedMessage tkTokenSession initSelector

-- | @- token@
token :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO (Id TKToken)
token tkTokenSession =
  sendMessage tkTokenSession tokenSelector

-- | @- delegate@
delegate :: IsTKTokenSession tkTokenSession => tkTokenSession -> IO RawId
delegate tkTokenSession =
  sendMessage tkTokenSession delegateSelector

-- | @- setDelegate:@
setDelegate :: IsTKTokenSession tkTokenSession => tkTokenSession -> RawId -> IO ()
setDelegate tkTokenSession value =
  sendMessage tkTokenSession setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithToken:@
initWithTokenSelector :: Selector '[Id TKToken] (Id TKTokenSession)
initWithTokenSelector = mkSelector "initWithToken:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenSession)
initSelector = mkSelector "init"

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id TKToken)
tokenSelector = mkSelector "token"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

