{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.NotificationCenter.Internal.Classes (
    module ObjC.NotificationCenter.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- NCWidgetController ----------

-- | Phantom type for @NCWidgetController@.
data NCWidgetController

instance IsObjCObject (Id NCWidgetController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NCWidgetController"

class IsNSObject a => IsNCWidgetController a where
  toNCWidgetController :: a -> Id NCWidgetController

instance IsNCWidgetController (Id NCWidgetController) where
  toNCWidgetController = unsafeCastId

instance IsNSObject (Id NCWidgetController) where
  toNSObject = unsafeCastId

-- ---------- NCWidgetListViewController ----------

-- | Phantom type for @NCWidgetListViewController@.
data NCWidgetListViewController

instance IsObjCObject (Id NCWidgetListViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NCWidgetListViewController"

class IsNSViewController a => IsNCWidgetListViewController a where
  toNCWidgetListViewController :: a -> Id NCWidgetListViewController

instance IsNCWidgetListViewController (Id NCWidgetListViewController) where
  toNCWidgetListViewController = unsafeCastId

instance IsNSObject (Id NCWidgetListViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NCWidgetListViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NCWidgetListViewController) where
  toNSViewController = unsafeCastId

-- ---------- NCWidgetSearchViewController ----------

-- | Phantom type for @NCWidgetSearchViewController@.
data NCWidgetSearchViewController

instance IsObjCObject (Id NCWidgetSearchViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NCWidgetSearchViewController"

class IsNSViewController a => IsNCWidgetSearchViewController a where
  toNCWidgetSearchViewController :: a -> Id NCWidgetSearchViewController

instance IsNCWidgetSearchViewController (Id NCWidgetSearchViewController) where
  toNCWidgetSearchViewController = unsafeCastId

instance IsNSObject (Id NCWidgetSearchViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id NCWidgetSearchViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id NCWidgetSearchViewController) where
  toNSViewController = unsafeCastId
