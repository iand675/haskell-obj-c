{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ContactsUI.Internal.Classes (
    module ObjC.ContactsUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CNContactPicker ----------

-- | CNContactPicker is a popover-based contact picker for choosing a contact or a contact's value, such as a phone number or email address.
-- 
-- Phantom type for @CNContactPicker@.
data CNContactPicker

instance IsObjCObject (Id CNContactPicker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactPicker"

class IsNSObject a => IsCNContactPicker a where
  toCNContactPicker :: a -> Id CNContactPicker

instance IsCNContactPicker (Id CNContactPicker) where
  toCNContactPicker = unsafeCastId

instance IsNSObject (Id CNContactPicker) where
  toNSObject = unsafeCastId

-- ---------- CNContactViewController ----------

-- | A view controller to display and edit a @CNContact.@
-- 
-- Phantom type for @CNContactViewController@.
data CNContactViewController

instance IsObjCObject (Id CNContactViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNContactViewController"

class IsNSViewController a => IsCNContactViewController a where
  toCNContactViewController :: a -> Id CNContactViewController

instance IsCNContactViewController (Id CNContactViewController) where
  toCNContactViewController = unsafeCastId

instance IsNSObject (Id CNContactViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id CNContactViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id CNContactViewController) where
  toNSViewController = unsafeCastId
