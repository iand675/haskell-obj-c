{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Compact authentication view providing authentication similar to @LAContext@ evaluatePolicy API.
--
-- This view is non-textual, it displays only a compact icon hinting users to use Touch ID or Watch to authenticate. The reason for the authentication must be apparent from the surrounding UI to avoid confusion and security risks.
--
-- Generated bindings for @LAAuthenticationView@.
module ObjC.LocalAuthenticationEmbeddedUI.LAAuthenticationView
  ( LAAuthenticationView
  , IsLAAuthenticationView(..)
  , initWithFrame
  , initWithCoder
  , initWithContext
  , initWithContext_controlSize
  , context
  , controlSize
  , contextSelector
  , controlSizeSelector
  , initWithCoderSelector
  , initWithContextSelector
  , initWithContext_controlSizeSelector
  , initWithFrameSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthenticationEmbeddedUI.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.LocalAuthentication.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> NSRect -> IO (Id LAAuthenticationView)
initWithFrame laAuthenticationView frameRect =
  sendOwnedMessage laAuthenticationView initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsLAAuthenticationView laAuthenticationView, IsNSCoder coder) => laAuthenticationView -> coder -> IO (Id LAAuthenticationView)
initWithCoder laAuthenticationView coder =
  sendOwnedMessage laAuthenticationView initWithCoderSelector (toNSCoder coder)

-- | Creates a new view and pairs it with the specified authentication context.
--
-- The authentication is controlled using the provided authentication context. When @evaluatePolicy@ or @evaluateAccessControl@ is called on this context, the UI will be presented using this view rather than using the standard authentication alert. Since the view is designed for authentication with Touch ID or Watch the only supported policies for calling @evaluatePolicy@ on the context are - @LAPolicyDeviceOwnerAuthenticationWithBiometrics@ - @LAPolicyDeviceOwnerAuthenticationWithCompanion@ - @LAPolicyDeviceOwnerAuthenticationWitchBiometricsOrCompanion@ - @LAPolicyDeviceOwnerAuthentication@ (This one is supported just for convenience. If neither biometric nor watch authentication is available, the evaluation of the policy fails)
--
-- @context@ — @LAContext@ instance to control the authentication.
--
-- ObjC selector: @- initWithContext:@
initWithContext :: (IsLAAuthenticationView laAuthenticationView, IsLAContext context) => laAuthenticationView -> context -> IO (Id LAAuthenticationView)
initWithContext laAuthenticationView context =
  sendOwnedMessage laAuthenticationView initWithContextSelector (toLAContext context)

-- | Creates a new view and pairs it with the specified authentication context.
--
-- The authentication is controlled using the provided authentication context. When @evaluatePolicy@ or @evaluateAccessControl@ is called on this context, the UI will be presented using this view rather than using the standard authentication alert. Since the view is designed for authentication with Touch ID or Watch the only supported policies for calling @evaluatePolicy@ on the context are - @LAPolicyDeviceOwnerAuthenticationWithBiometrics@ - @LAPolicyDeviceOwnerAuthenticationWithCompanion@ - @LAPolicyDeviceOwnerAuthenticationWitchBiometricsOrCompanion@ - @LAPolicyDeviceOwnerAuthentication@ (This one is supported just for convenience. If neither biometric nor watch authentication is available, the evaluation of the policy fails)
--
-- @context@ — @LAContext@ instance to control the authentication.
--
-- @controlSize@ — Preferred size of @LAAuthenticationView@ provided using @NSControlSize@
--
-- ObjC selector: @- initWithContext:controlSize:@
initWithContext_controlSize :: (IsLAAuthenticationView laAuthenticationView, IsLAContext context) => laAuthenticationView -> context -> NSControlSize -> IO (Id LAAuthenticationView)
initWithContext_controlSize laAuthenticationView context controlSize =
  sendOwnedMessage laAuthenticationView initWithContext_controlSizeSelector (toLAContext context) controlSize

-- | @LAContext@ instance passed to the initializer.
--
-- ObjC selector: @- context@
context :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> IO (Id LAContext)
context laAuthenticationView =
  sendMessage laAuthenticationView contextSelector

-- | @NSControlSize@ instance passed to the initializer.
--
-- ObjC selector: @- controlSize@
controlSize :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> IO NSControlSize
controlSize laAuthenticationView =
  sendMessage laAuthenticationView controlSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id LAAuthenticationView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id LAAuthenticationView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContext:@
initWithContextSelector :: Selector '[Id LAContext] (Id LAAuthenticationView)
initWithContextSelector = mkSelector "initWithContext:"

-- | @Selector@ for @initWithContext:controlSize:@
initWithContext_controlSizeSelector :: Selector '[Id LAContext, NSControlSize] (Id LAAuthenticationView)
initWithContext_controlSizeSelector = mkSelector "initWithContext:controlSize:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id LAContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector '[] NSControlSize
controlSizeSelector = mkSelector "controlSize"

