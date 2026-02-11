{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.UniformTypeIdentifiers.Internal.Classes (
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- UTType ----------

-- | A class representing a type in a type hierarchy.
--
-- Types may represent files on disk, abstract data types with no on-disk	representation, or even entirely unrelated hierarchical classification	systems such as hardware.
--
-- Older API that does not use @UTType@ typically uses an untyped @NSString@	or @CFStringRef@ to refer to a type by its identifier. To get the	identifier of a type for use with these APIs, use the @identifier@ property	of this class.
--
-- https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/understanding_utis/
-- 
-- Phantom type for @UTType@.
data UTType

instance IsObjCObject (Id UTType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UTType"

class IsNSObject a => IsUTType a where
  toUTType :: a -> Id UTType

instance IsUTType (Id UTType) where
  toUTType = unsafeCastId

instance IsNSObject (Id UTType) where
  toNSObject = unsafeCastId
