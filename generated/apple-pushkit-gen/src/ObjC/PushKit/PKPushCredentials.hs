{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that encapsulates the device token you use to deliver push notifications to your app.
--
-- When registering your app's push types, PushKit creates a ``PushKit/PKPushCredentials`` object for each type your app supports and delivers it to your delegate's ``PushKit/PKPushRegistryDelegate/pushRegistry:didUpdatePushCredentials:forType:`` method. Don't create ``PushKit/PKPushCredentials`` objects yourself.
--
-- Generated bindings for @PKPushCredentials@.
module ObjC.PushKit.PKPushCredentials
  ( PKPushCredentials
  , IsPKPushCredentials(..)
  , type_
  , token
  , tokenSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PushKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The push type constant associated with the token.
--
-- For possible values, see ``PushKit/PKPushType``.
--
-- ObjC selector: @- type@
type_ :: IsPKPushCredentials pkPushCredentials => pkPushCredentials -> IO (Id NSString)
type_ pkPushCredentials =
  sendMessage pkPushCredentials typeSelector

-- | A unique device token to use when sending push notifications to the current device.
--
-- Forward this token to the server you use to generate push notifications. When preparing to deliver a push notification to the current device, include the token in the HTTP request you send to Apple Push Notification service (APNs).
--
-- ObjC selector: @- token@
token :: IsPKPushCredentials pkPushCredentials => pkPushCredentials -> IO (Id NSData)
token pkPushCredentials =
  sendMessage pkPushCredentials tokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id NSData)
tokenSelector = mkSelector "token"

