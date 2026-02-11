{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MediaLibrary.Internal.Classes (
    module ObjC.MediaLibrary.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MLMediaGroup ----------

-- | Phantom type for @MLMediaGroup@.
data MLMediaGroup

instance IsObjCObject (Id MLMediaGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMediaGroup"

class IsNSObject a => IsMLMediaGroup a where
  toMLMediaGroup :: a -> Id MLMediaGroup

instance IsMLMediaGroup (Id MLMediaGroup) where
  toMLMediaGroup = unsafeCastId

instance IsNSObject (Id MLMediaGroup) where
  toNSObject = unsafeCastId

-- ---------- MLMediaLibrary ----------

-- | Phantom type for @MLMediaLibrary@.
data MLMediaLibrary

instance IsObjCObject (Id MLMediaLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMediaLibrary"

class IsNSObject a => IsMLMediaLibrary a where
  toMLMediaLibrary :: a -> Id MLMediaLibrary

instance IsMLMediaLibrary (Id MLMediaLibrary) where
  toMLMediaLibrary = unsafeCastId

instance IsNSObject (Id MLMediaLibrary) where
  toNSObject = unsafeCastId

-- ---------- MLMediaObject ----------

-- | Phantom type for @MLMediaObject@.
data MLMediaObject

instance IsObjCObject (Id MLMediaObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMediaObject"

class IsNSObject a => IsMLMediaObject a where
  toMLMediaObject :: a -> Id MLMediaObject

instance IsMLMediaObject (Id MLMediaObject) where
  toMLMediaObject = unsafeCastId

instance IsNSObject (Id MLMediaObject) where
  toNSObject = unsafeCastId

-- ---------- MLMediaSource ----------

-- | Phantom type for @MLMediaSource@.
data MLMediaSource

instance IsObjCObject (Id MLMediaSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMediaSource"

class IsNSObject a => IsMLMediaSource a where
  toMLMediaSource :: a -> Id MLMediaSource

instance IsMLMediaSource (Id MLMediaSource) where
  toMLMediaSource = unsafeCastId

instance IsNSObject (Id MLMediaSource) where
  toNSObject = unsafeCastId
