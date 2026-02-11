{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameSelector
  , initWithCoderSelector
  , initWithContextSelector
  , initWithContext_controlSizeSelector
  , contextSelector
  , controlSizeSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge

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

import ObjC.LocalAuthenticationEmbeddedUI.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.LocalAuthentication.Internal.Classes

-- | @- initWithFrame:@
initWithFrame :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> NSRect -> IO (Id LAAuthenticationView)
initWithFrame laAuthenticationView  frameRect =
    sendMsg laAuthenticationView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsLAAuthenticationView laAuthenticationView, IsNSCoder coder) => laAuthenticationView -> coder -> IO (Id LAAuthenticationView)
initWithCoder laAuthenticationView  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg laAuthenticationView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a new view and pairs it with the specified authentication context.
--
-- The authentication is controlled using the provided authentication context. When @evaluatePolicy@ or @evaluateAccessControl@ is called on this context, the UI will be presented using this view rather than using the standard authentication alert. Since the view is designed for authentication with Touch ID or Watch the only supported policies for calling @evaluatePolicy@ on the context are - @LAPolicyDeviceOwnerAuthenticationWithBiometrics@ - @LAPolicyDeviceOwnerAuthenticationWithCompanion@ - @LAPolicyDeviceOwnerAuthenticationWitchBiometricsOrCompanion@ - @LAPolicyDeviceOwnerAuthentication@ (This one is supported just for convenience. If neither biometric nor watch authentication is available, the evaluation of the policy fails)
--
-- @context@ — @LAContext@ instance to control the authentication.
--
-- ObjC selector: @- initWithContext:@
initWithContext :: (IsLAAuthenticationView laAuthenticationView, IsLAContext context) => laAuthenticationView -> context -> IO (Id LAAuthenticationView)
initWithContext laAuthenticationView  context =
  withObjCPtr context $ \raw_context ->
      sendMsg laAuthenticationView (mkSelector "initWithContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

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
initWithContext_controlSize laAuthenticationView  context controlSize =
  withObjCPtr context $ \raw_context ->
      sendMsg laAuthenticationView (mkSelector "initWithContext:controlSize:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ()), argCULong (coerce controlSize)] >>= ownedObject . castPtr

-- | @LAContext@ instance passed to the initializer.
--
-- ObjC selector: @- context@
context :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> IO (Id LAContext)
context laAuthenticationView  =
    sendMsg laAuthenticationView (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSControlSize@ instance passed to the initializer.
--
-- ObjC selector: @- controlSize@
controlSize :: IsLAAuthenticationView laAuthenticationView => laAuthenticationView -> IO NSControlSize
controlSize laAuthenticationView  =
    fmap (coerce :: CULong -> NSControlSize) $ sendMsg laAuthenticationView (mkSelector "controlSize") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContext:@
initWithContextSelector :: Selector
initWithContextSelector = mkSelector "initWithContext:"

-- | @Selector@ for @initWithContext:controlSize:@
initWithContext_controlSizeSelector :: Selector
initWithContext_controlSizeSelector = mkSelector "initWithContext:controlSize:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @controlSize@
controlSizeSelector :: Selector
controlSizeSelector = mkSelector "controlSize"

