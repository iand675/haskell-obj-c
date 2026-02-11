{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.QuickLookThumbnailing.Internal.Classes (
    module ObjC.QuickLookThumbnailing.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- QLFileThumbnailRequest ----------

-- | This class contains information about the thumbnail that should be provided.
-- 
-- Phantom type for @QLFileThumbnailRequest@.
data QLFileThumbnailRequest

instance IsObjCObject (Id QLFileThumbnailRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLFileThumbnailRequest"

class IsNSObject a => IsQLFileThumbnailRequest a where
  toQLFileThumbnailRequest :: a -> Id QLFileThumbnailRequest

instance IsQLFileThumbnailRequest (Id QLFileThumbnailRequest) where
  toQLFileThumbnailRequest = unsafeCastId

instance IsNSObject (Id QLFileThumbnailRequest) where
  toNSObject = unsafeCastId

-- ---------- QLThumbnailGenerationRequest ----------

-- | Phantom type for @QLThumbnailGenerationRequest@.
data QLThumbnailGenerationRequest

instance IsObjCObject (Id QLThumbnailGenerationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLThumbnailGenerationRequest"

class IsNSObject a => IsQLThumbnailGenerationRequest a where
  toQLThumbnailGenerationRequest :: a -> Id QLThumbnailGenerationRequest

instance IsQLThumbnailGenerationRequest (Id QLThumbnailGenerationRequest) where
  toQLThumbnailGenerationRequest = unsafeCastId

instance IsNSObject (Id QLThumbnailGenerationRequest) where
  toNSObject = unsafeCastId

-- ---------- QLThumbnailGenerator ----------

-- | Phantom type for @QLThumbnailGenerator@.
data QLThumbnailGenerator

instance IsObjCObject (Id QLThumbnailGenerator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLThumbnailGenerator"

class IsNSObject a => IsQLThumbnailGenerator a where
  toQLThumbnailGenerator :: a -> Id QLThumbnailGenerator

instance IsQLThumbnailGenerator (Id QLThumbnailGenerator) where
  toQLThumbnailGenerator = unsafeCastId

instance IsNSObject (Id QLThumbnailGenerator) where
  toNSObject = unsafeCastId

-- ---------- QLThumbnailProvider ----------

-- | Phantom type for @QLThumbnailProvider@.
data QLThumbnailProvider

instance IsObjCObject (Id QLThumbnailProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLThumbnailProvider"

class IsNSObject a => IsQLThumbnailProvider a where
  toQLThumbnailProvider :: a -> Id QLThumbnailProvider

instance IsQLThumbnailProvider (Id QLThumbnailProvider) where
  toQLThumbnailProvider = unsafeCastId

instance IsNSObject (Id QLThumbnailProvider) where
  toNSObject = unsafeCastId

-- ---------- QLThumbnailReply ----------

-- | To provide a thumbnail for a request, you have to return a QLThumbnailReply object.
--
-- To provide a thumbnail, you have two options: 1. Draw the thumbnail, by providing a QLThumbnailReply created with a drawing block. 2. Pass the thumbnail file URL, by providing a QLThumbnailReply created with a file URL.
-- 
-- Phantom type for @QLThumbnailReply@.
data QLThumbnailReply

instance IsObjCObject (Id QLThumbnailReply) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLThumbnailReply"

class IsNSObject a => IsQLThumbnailReply a where
  toQLThumbnailReply :: a -> Id QLThumbnailReply

instance IsQLThumbnailReply (Id QLThumbnailReply) where
  toQLThumbnailReply = unsafeCastId

instance IsNSObject (Id QLThumbnailReply) where
  toNSObject = unsafeCastId

-- ---------- QLThumbnailRepresentation ----------

-- | Phantom type for @QLThumbnailRepresentation@.
data QLThumbnailRepresentation

instance IsObjCObject (Id QLThumbnailRepresentation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLThumbnailRepresentation"

class IsNSObject a => IsQLThumbnailRepresentation a where
  toQLThumbnailRepresentation :: a -> Id QLThumbnailRepresentation

instance IsQLThumbnailRepresentation (Id QLThumbnailRepresentation) where
  toQLThumbnailRepresentation = unsafeCastId

instance IsNSObject (Id QLThumbnailRepresentation) where
  toNSObject = unsafeCastId
