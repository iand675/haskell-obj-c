{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.QuickLookUI.Internal.Classes (
    module ObjC.QuickLookUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- QLFilePreviewRequest ----------

-- | This class contains information about the preview that should be provided.
-- 
-- Phantom type for @QLFilePreviewRequest@.
data QLFilePreviewRequest

instance IsObjCObject (Id QLFilePreviewRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLFilePreviewRequest"

class IsNSObject a => IsQLFilePreviewRequest a where
  toQLFilePreviewRequest :: a -> Id QLFilePreviewRequest

instance IsQLFilePreviewRequest (Id QLFilePreviewRequest) where
  toQLFilePreviewRequest = unsafeCastId

instance IsNSObject (Id QLFilePreviewRequest) where
  toNSObject = unsafeCastId

-- ---------- QLPreviewProvider ----------

-- | Data-based preview extensions should subclass QLPreviewProvider in their principal object. The subclass should conform to QLPreviewingController.
-- 
-- Phantom type for @QLPreviewProvider@.
data QLPreviewProvider

instance IsObjCObject (Id QLPreviewProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLPreviewProvider"

class IsNSObject a => IsQLPreviewProvider a where
  toQLPreviewProvider :: a -> Id QLPreviewProvider

instance IsQLPreviewProvider (Id QLPreviewProvider) where
  toQLPreviewProvider = unsafeCastId

instance IsNSObject (Id QLPreviewProvider) where
  toNSObject = unsafeCastId

-- ---------- QLPreviewReply ----------

-- | To provide a data-based preview, you have to return a QLPreviewReply object.
-- 
-- Phantom type for @QLPreviewReply@.
data QLPreviewReply

instance IsObjCObject (Id QLPreviewReply) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLPreviewReply"

class IsNSObject a => IsQLPreviewReply a where
  toQLPreviewReply :: a -> Id QLPreviewReply

instance IsQLPreviewReply (Id QLPreviewReply) where
  toQLPreviewReply = unsafeCastId

instance IsNSObject (Id QLPreviewReply) where
  toNSObject = unsafeCastId

-- ---------- QLPreviewReplyAttachment ----------

-- | QLPreviewReplyAttachment is used to provide data for attachment in html data-based previews.
-- 
-- Phantom type for @QLPreviewReplyAttachment@.
data QLPreviewReplyAttachment

instance IsObjCObject (Id QLPreviewReplyAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLPreviewReplyAttachment"

class IsNSObject a => IsQLPreviewReplyAttachment a where
  toQLPreviewReplyAttachment :: a -> Id QLPreviewReplyAttachment

instance IsQLPreviewReplyAttachment (Id QLPreviewReplyAttachment) where
  toQLPreviewReplyAttachment = unsafeCastId

instance IsNSObject (Id QLPreviewReplyAttachment) where
  toNSObject = unsafeCastId

-- ---------- QLPreviewView ----------

-- | A Quick Look preview of an item that you can embed into your view hierarchy.
-- 
-- Phantom type for @QLPreviewView@.
data QLPreviewView

instance IsObjCObject (Id QLPreviewView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLPreviewView"

class IsNSView a => IsQLPreviewView a where
  toQLPreviewView :: a -> Id QLPreviewView

instance IsQLPreviewView (Id QLPreviewView) where
  toQLPreviewView = unsafeCastId

instance IsNSObject (Id QLPreviewView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id QLPreviewView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id QLPreviewView) where
  toNSView = unsafeCastId

-- ---------- QLPreviewPanel ----------

-- | A class that implements the Quick Look preview panel to display a preview of a list of items.
--
-- Every application has a single shared instance of ``QuickLookUI/QLPreviewPanel`` accessible through ``QuickLookUI/QLPreviewPanel/sharedPreviewPanel``. The preview panel follows the responder chain and adapts to the first responder willing to control it. A preview panel controller provides the content through methods defined in the ``QuickLookUI/QLPreviewPanelDataSource`` protocol.
--
-- You can’t subclass ``QuickLookUI/QLPreviewPanel``; you can, however, customize its behavior using a ``QuickLookUI/QLPreviewPanel/delegate``. See the ``QuickLookUI/QLPreviewPanelDelegate`` protocol for the methods to customize a preview panel’s behavior.
-- 
-- Phantom type for @QLPreviewPanel@.
data QLPreviewPanel

instance IsObjCObject (Id QLPreviewPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "QLPreviewPanel"

class IsNSPanel a => IsQLPreviewPanel a where
  toQLPreviewPanel :: a -> Id QLPreviewPanel

instance IsQLPreviewPanel (Id QLPreviewPanel) where
  toQLPreviewPanel = unsafeCastId

instance IsNSObject (Id QLPreviewPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id QLPreviewPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id QLPreviewPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id QLPreviewPanel) where
  toNSWindow = unsafeCastId
