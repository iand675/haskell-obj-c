{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreSpotlight.Internal.Classes (
    module ObjC.CoreSpotlight.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- CSCustomAttributeKey ----------

-- | Phantom type for @CSCustomAttributeKey@.
data CSCustomAttributeKey

instance IsObjCObject (Id CSCustomAttributeKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSCustomAttributeKey"

class IsNSObject a => IsCSCustomAttributeKey a where
  toCSCustomAttributeKey :: a -> Id CSCustomAttributeKey

instance IsCSCustomAttributeKey (Id CSCustomAttributeKey) where
  toCSCustomAttributeKey = unsafeCastId

instance IsNSObject (Id CSCustomAttributeKey) where
  toNSObject = unsafeCastId

-- ---------- CSImportExtension ----------

-- | Phantom type for @CSImportExtension@.
data CSImportExtension

instance IsObjCObject (Id CSImportExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSImportExtension"

class IsNSObject a => IsCSImportExtension a where
  toCSImportExtension :: a -> Id CSImportExtension

instance IsCSImportExtension (Id CSImportExtension) where
  toCSImportExtension = unsafeCastId

instance IsNSObject (Id CSImportExtension) where
  toNSObject = unsafeCastId

-- ---------- CSIndexExtensionRequestHandler ----------

-- | Phantom type for @CSIndexExtensionRequestHandler@.
data CSIndexExtensionRequestHandler

instance IsObjCObject (Id CSIndexExtensionRequestHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSIndexExtensionRequestHandler"

class IsNSObject a => IsCSIndexExtensionRequestHandler a where
  toCSIndexExtensionRequestHandler :: a -> Id CSIndexExtensionRequestHandler

instance IsCSIndexExtensionRequestHandler (Id CSIndexExtensionRequestHandler) where
  toCSIndexExtensionRequestHandler = unsafeCastId

instance IsNSObject (Id CSIndexExtensionRequestHandler) where
  toNSObject = unsafeCastId

-- ---------- CSPerson ----------

-- | Phantom type for @CSPerson@.
data CSPerson

instance IsObjCObject (Id CSPerson) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSPerson"

class IsNSObject a => IsCSPerson a where
  toCSPerson :: a -> Id CSPerson

instance IsCSPerson (Id CSPerson) where
  toCSPerson = unsafeCastId

instance IsNSObject (Id CSPerson) where
  toNSObject = unsafeCastId

-- ---------- CSSearchQuery ----------

-- | Phantom type for @CSSearchQuery@.
data CSSearchQuery

instance IsObjCObject (Id CSSearchQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSearchQuery"

class IsNSObject a => IsCSSearchQuery a where
  toCSSearchQuery :: a -> Id CSSearchQuery

instance IsCSSearchQuery (Id CSSearchQuery) where
  toCSSearchQuery = unsafeCastId

instance IsNSObject (Id CSSearchQuery) where
  toNSObject = unsafeCastId

-- ---------- CSSearchQueryContext ----------

-- | Phantom type for @CSSearchQueryContext@.
data CSSearchQueryContext

instance IsObjCObject (Id CSSearchQueryContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSearchQueryContext"

class IsNSObject a => IsCSSearchQueryContext a where
  toCSSearchQueryContext :: a -> Id CSSearchQueryContext

instance IsCSSearchQueryContext (Id CSSearchQueryContext) where
  toCSSearchQueryContext = unsafeCastId

instance IsNSObject (Id CSSearchQueryContext) where
  toNSObject = unsafeCastId

-- ---------- CSSearchableIndex ----------

-- | Phantom type for @CSSearchableIndex@.
data CSSearchableIndex

instance IsObjCObject (Id CSSearchableIndex) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSearchableIndex"

class IsNSObject a => IsCSSearchableIndex a where
  toCSSearchableIndex :: a -> Id CSSearchableIndex

instance IsCSSearchableIndex (Id CSSearchableIndex) where
  toCSSearchableIndex = unsafeCastId

instance IsNSObject (Id CSSearchableIndex) where
  toNSObject = unsafeCastId

-- ---------- CSSearchableItem ----------

-- | Phantom type for @CSSearchableItem@.
data CSSearchableItem

instance IsObjCObject (Id CSSearchableItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSearchableItem"

class IsNSObject a => IsCSSearchableItem a where
  toCSSearchableItem :: a -> Id CSSearchableItem

instance IsCSSearchableItem (Id CSSearchableItem) where
  toCSSearchableItem = unsafeCastId

instance IsNSObject (Id CSSearchableItem) where
  toNSObject = unsafeCastId

-- ---------- CSSearchableItemAttributeSet ----------

-- | Phantom type for @CSSearchableItemAttributeSet@.
data CSSearchableItemAttributeSet

instance IsObjCObject (Id CSSearchableItemAttributeSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSearchableItemAttributeSet"

class IsNSObject a => IsCSSearchableItemAttributeSet a where
  toCSSearchableItemAttributeSet :: a -> Id CSSearchableItemAttributeSet

instance IsCSSearchableItemAttributeSet (Id CSSearchableItemAttributeSet) where
  toCSSearchableItemAttributeSet = unsafeCastId

instance IsNSObject (Id CSSearchableItemAttributeSet) where
  toNSObject = unsafeCastId

-- ---------- CSSuggestion ----------

-- | Phantom type for @CSSuggestion@.
data CSSuggestion

instance IsObjCObject (Id CSSuggestion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSSuggestion"

class IsNSObject a => IsCSSuggestion a where
  toCSSuggestion :: a -> Id CSSuggestion

instance IsCSSuggestion (Id CSSuggestion) where
  toCSSuggestion = unsafeCastId

instance IsNSObject (Id CSSuggestion) where
  toNSObject = unsafeCastId

-- ---------- CSUserQuery ----------

-- | Phantom type for @CSUserQuery@.
data CSUserQuery

instance IsObjCObject (Id CSUserQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSUserQuery"

class IsCSSearchQuery a => IsCSUserQuery a where
  toCSUserQuery :: a -> Id CSUserQuery

instance IsCSUserQuery (Id CSUserQuery) where
  toCSUserQuery = unsafeCastId

instance IsCSSearchQuery (Id CSUserQuery) where
  toCSSearchQuery = unsafeCastId

instance IsNSObject (Id CSUserQuery) where
  toNSObject = unsafeCastId

-- ---------- CSUserQueryContext ----------

-- | Phantom type for @CSUserQueryContext@.
data CSUserQueryContext

instance IsObjCObject (Id CSUserQueryContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSUserQueryContext"

class IsCSSearchQueryContext a => IsCSUserQueryContext a where
  toCSUserQueryContext :: a -> Id CSUserQueryContext

instance IsCSUserQueryContext (Id CSUserQueryContext) where
  toCSUserQueryContext = unsafeCastId

instance IsCSSearchQueryContext (Id CSUserQueryContext) where
  toCSSearchQueryContext = unsafeCastId

instance IsNSObject (Id CSUserQueryContext) where
  toNSObject = unsafeCastId

-- ---------- CSLocalizedString ----------

-- | Phantom type for @CSLocalizedString@.
data CSLocalizedString

instance IsObjCObject (Id CSLocalizedString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CSLocalizedString"

class IsNSString a => IsCSLocalizedString a where
  toCSLocalizedString :: a -> Id CSLocalizedString

instance IsCSLocalizedString (Id CSLocalizedString) where
  toCSLocalizedString = unsafeCastId

instance IsNSObject (Id CSLocalizedString) where
  toNSObject = unsafeCastId

instance IsNSString (Id CSLocalizedString) where
  toNSString = unsafeCastId
