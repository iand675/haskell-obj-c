{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.OSAKit.Internal.Classes (
    module ObjC.OSAKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- OSALanguage ----------

-- | Phantom type for @OSALanguage@.
data OSALanguage

instance IsObjCObject (Id OSALanguage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OSALanguage"

class IsNSObject a => IsOSALanguage a where
  toOSALanguage :: a -> Id OSALanguage

instance IsOSALanguage (Id OSALanguage) where
  toOSALanguage = unsafeCastId

instance IsNSObject (Id OSALanguage) where
  toNSObject = unsafeCastId

-- ---------- OSALanguageInstance ----------

-- | Phantom type for @OSALanguageInstance@.
data OSALanguageInstance

instance IsObjCObject (Id OSALanguageInstance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OSALanguageInstance"

class IsNSObject a => IsOSALanguageInstance a where
  toOSALanguageInstance :: a -> Id OSALanguageInstance

instance IsOSALanguageInstance (Id OSALanguageInstance) where
  toOSALanguageInstance = unsafeCastId

instance IsNSObject (Id OSALanguageInstance) where
  toNSObject = unsafeCastId

-- ---------- OSAScriptController ----------

-- | Phantom type for @OSAScriptController@.
data OSAScriptController

instance IsObjCObject (Id OSAScriptController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OSAScriptController"

class IsNSController a => IsOSAScriptController a where
  toOSAScriptController :: a -> Id OSAScriptController

instance IsOSAScriptController (Id OSAScriptController) where
  toOSAScriptController = unsafeCastId

instance IsNSController (Id OSAScriptController) where
  toNSController = unsafeCastId

instance IsNSObject (Id OSAScriptController) where
  toNSObject = unsafeCastId

-- ---------- OSAScriptView ----------

-- | Phantom type for @OSAScriptView@.
data OSAScriptView

instance IsObjCObject (Id OSAScriptView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OSAScriptView"

class IsNSTextView a => IsOSAScriptView a where
  toOSAScriptView :: a -> Id OSAScriptView

instance IsOSAScriptView (Id OSAScriptView) where
  toOSAScriptView = unsafeCastId

instance IsNSObject (Id OSAScriptView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id OSAScriptView) where
  toNSResponder = unsafeCastId

instance IsNSText (Id OSAScriptView) where
  toNSText = unsafeCastId

instance IsNSTextView (Id OSAScriptView) where
  toNSTextView = unsafeCastId

instance IsNSView (Id OSAScriptView) where
  toNSView = unsafeCastId
