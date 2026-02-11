{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.BusinessChat.Internal.Classes (
    module ObjC.BusinessChat.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- BCChatAction ----------

-- | Phantom type for @BCChatAction@.
data BCChatAction

instance IsObjCObject (Id BCChatAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BCChatAction"

class IsNSObject a => IsBCChatAction a where
  toBCChatAction :: a -> Id BCChatAction

instance IsBCChatAction (Id BCChatAction) where
  toBCChatAction = unsafeCastId

instance IsNSObject (Id BCChatAction) where
  toNSObject = unsafeCastId

-- ---------- BCChatButton ----------

-- | Phantom type for @BCChatButton@.
data BCChatButton

instance IsObjCObject (Id BCChatButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BCChatButton"

class IsNSControl a => IsBCChatButton a where
  toBCChatButton :: a -> Id BCChatButton

instance IsBCChatButton (Id BCChatButton) where
  toBCChatButton = unsafeCastId

instance IsNSControl (Id BCChatButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id BCChatButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id BCChatButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id BCChatButton) where
  toNSView = unsafeCastId
