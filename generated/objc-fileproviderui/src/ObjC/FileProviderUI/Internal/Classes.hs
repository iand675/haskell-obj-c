{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.FileProviderUI.Internal.Classes (
    module ObjC.FileProviderUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- FPUIActionExtensionContext ----------

-- | An extension context provided to File Provider UI extensions.
-- 
-- Phantom type for @FPUIActionExtensionContext@.
data FPUIActionExtensionContext

instance IsObjCObject (Id FPUIActionExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FPUIActionExtensionContext"

class IsNSExtensionContext a => IsFPUIActionExtensionContext a where
  toFPUIActionExtensionContext :: a -> Id FPUIActionExtensionContext

instance IsFPUIActionExtensionContext (Id FPUIActionExtensionContext) where
  toFPUIActionExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id FPUIActionExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id FPUIActionExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- FPUIActionExtensionViewController ----------

-- | The custom user interface used to perform a selected action.
--
-- Subclass this view controller to provide the user interface for your actions.
--
-- No matter how many actions you define, your File Provider UI extension has only one ``FPUIActionExtensionViewController`` subclass. When the user selects one of your actions, the system instantiates a copy of your subclass, calls its ``FPUIActionExtensionViewController/prepareForActionWithIdentifier:itemIdentifiers:`` method, and presents it to the user.
--
-- Your subclass must do the following:
--
-- - Override the ``FPUIActionExtensionViewController/prepareForActionWithIdentifier:itemIdentifiers:`` method to check the action identifiers and present an appropriate user interface for the selected actions. - Provide some sort of feedback, even if the action doesn't require interaction with the user. For example, present a view that quickly fades out and automatically completes the action. - Call the ``FPUIActionExtensionViewController/extensionContext`` object's ``FPUIActionExtensionContext/cancelRequestWithError:`` or ``FPUIActionExtensionContext/completeRequest`` method when the action is finished to complete the action.
-- 
-- Phantom type for @FPUIActionExtensionViewController@.
data FPUIActionExtensionViewController

instance IsObjCObject (Id FPUIActionExtensionViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FPUIActionExtensionViewController"

class IsNSViewController a => IsFPUIActionExtensionViewController a where
  toFPUIActionExtensionViewController :: a -> Id FPUIActionExtensionViewController

instance IsFPUIActionExtensionViewController (Id FPUIActionExtensionViewController) where
  toFPUIActionExtensionViewController = unsafeCastId

instance IsNSObject (Id FPUIActionExtensionViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id FPUIActionExtensionViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id FPUIActionExtensionViewController) where
  toNSViewController = unsafeCastId
