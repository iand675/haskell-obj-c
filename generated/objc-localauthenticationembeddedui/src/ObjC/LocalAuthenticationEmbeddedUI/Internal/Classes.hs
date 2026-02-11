{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.LocalAuthenticationEmbeddedUI.Internal.Classes (
    module ObjC.LocalAuthenticationEmbeddedUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.LocalAuthentication.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.LocalAuthentication.Internal.Classes

-- ---------- LAAuthenticationView ----------

-- | Compact authentication view providing authentication similar to @LAContext@ evaluatePolicy API.
--
-- This view is non-textual, it displays only a compact icon hinting users to use Touch ID or Watch to authenticate. The reason for the authentication must be apparent from the surrounding UI to avoid confusion and security risks.
-- 
-- Phantom type for @LAAuthenticationView@.
data LAAuthenticationView

instance IsObjCObject (Id LAAuthenticationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAAuthenticationView"

class IsNSView a => IsLAAuthenticationView a where
  toLAAuthenticationView :: a -> Id LAAuthenticationView

instance IsLAAuthenticationView (Id LAAuthenticationView) where
  toLAAuthenticationView = unsafeCastId

instance IsNSObject (Id LAAuthenticationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id LAAuthenticationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id LAAuthenticationView) where
  toNSView = unsafeCastId
