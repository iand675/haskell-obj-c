{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SharedWithYou.Internal.Classes (
    module ObjC.SharedWithYou.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SharedWithYouCore.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SharedWithYouCore.Internal.Classes

-- ---------- SWHighlight ----------

-- | SWHighlight
--
-- A model object representing a universal link shared by any number of contacts, in any number of conversations. The identities of the contacts are not exposed to the application.
--
-- The system tracks universal links shared with the current user, and decides which links to elevate for consumption in an app. When the system deems a link to be useful, it surfaces that link to the hosting app in the form of an @SWHighlight@ object. Only universal links can be surfaced in this way.
-- 
-- Phantom type for @SWHighlight@.
data SWHighlight

instance IsObjCObject (Id SWHighlight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlight"

class IsNSObject a => IsSWHighlight a where
  toSWHighlight :: a -> Id SWHighlight

instance IsSWHighlight (Id SWHighlight) where
  toSWHighlight = unsafeCastId

instance IsNSObject (Id SWHighlight) where
  toNSObject = unsafeCastId

-- ---------- SWHighlightCenter ----------

-- | SWHighlightCenter
--
-- Provides the application with a priority-ordered list of universal links which have been shared with the current user.
--
-- The system decides which links should be surfaced. The app is responsible for updating its UI to reflect the latest provided list.
-- 
-- Phantom type for @SWHighlightCenter@.
data SWHighlightCenter

instance IsObjCObject (Id SWHighlightCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlightCenter"

class IsNSObject a => IsSWHighlightCenter a where
  toSWHighlightCenter :: a -> Id SWHighlightCenter

instance IsSWHighlightCenter (Id SWHighlightCenter) where
  toSWHighlightCenter = unsafeCastId

instance IsNSObject (Id SWHighlightCenter) where
  toNSObject = unsafeCastId

-- ---------- SWHighlightChangeEvent ----------

-- | SWHighlightChangeEvent
--
-- A model object representing activity that has happened on some content.
-- 
-- Phantom type for @SWHighlightChangeEvent@.
data SWHighlightChangeEvent

instance IsObjCObject (Id SWHighlightChangeEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlightChangeEvent"

class IsNSObject a => IsSWHighlightChangeEvent a where
  toSWHighlightChangeEvent :: a -> Id SWHighlightChangeEvent

instance IsSWHighlightChangeEvent (Id SWHighlightChangeEvent) where
  toSWHighlightChangeEvent = unsafeCastId

instance IsNSObject (Id SWHighlightChangeEvent) where
  toNSObject = unsafeCastId

-- ---------- SWHighlightMembershipEvent ----------

-- | SWHighlightMembershipEvent
--
-- A model object representing a membership event that has happened on some content.
-- 
-- Phantom type for @SWHighlightMembershipEvent@.
data SWHighlightMembershipEvent

instance IsObjCObject (Id SWHighlightMembershipEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlightMembershipEvent"

class IsNSObject a => IsSWHighlightMembershipEvent a where
  toSWHighlightMembershipEvent :: a -> Id SWHighlightMembershipEvent

instance IsSWHighlightMembershipEvent (Id SWHighlightMembershipEvent) where
  toSWHighlightMembershipEvent = unsafeCastId

instance IsNSObject (Id SWHighlightMembershipEvent) where
  toNSObject = unsafeCastId

-- ---------- SWHighlightMentionEvent ----------

-- | _SWHighlightMentionEvent
--
-- A model object representing a mention event that has happened on some content.
-- 
-- Phantom type for @SWHighlightMentionEvent@.
data SWHighlightMentionEvent

instance IsObjCObject (Id SWHighlightMentionEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlightMentionEvent"

class IsNSObject a => IsSWHighlightMentionEvent a where
  toSWHighlightMentionEvent :: a -> Id SWHighlightMentionEvent

instance IsSWHighlightMentionEvent (Id SWHighlightMentionEvent) where
  toSWHighlightMentionEvent = unsafeCastId

instance IsNSObject (Id SWHighlightMentionEvent) where
  toNSObject = unsafeCastId

-- ---------- SWHighlightPersistenceEvent ----------

-- | SWHighlightPersistenceEvent
--
-- A model object representing a persistence event that has happened on some content.
-- 
-- Phantom type for @SWHighlightPersistenceEvent@.
data SWHighlightPersistenceEvent

instance IsObjCObject (Id SWHighlightPersistenceEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWHighlightPersistenceEvent"

class IsNSObject a => IsSWHighlightPersistenceEvent a where
  toSWHighlightPersistenceEvent :: a -> Id SWHighlightPersistenceEvent

instance IsSWHighlightPersistenceEvent (Id SWHighlightPersistenceEvent) where
  toSWHighlightPersistenceEvent = unsafeCastId

instance IsNSObject (Id SWHighlightPersistenceEvent) where
  toNSObject = unsafeCastId

-- ---------- SWRemoveParticipantAlert ----------

-- | Phantom type for @SWRemoveParticipantAlert@.
data SWRemoveParticipantAlert

instance IsObjCObject (Id SWRemoveParticipantAlert) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWRemoveParticipantAlert"

class IsNSObject a => IsSWRemoveParticipantAlert a where
  toSWRemoveParticipantAlert :: a -> Id SWRemoveParticipantAlert

instance IsSWRemoveParticipantAlert (Id SWRemoveParticipantAlert) where
  toSWRemoveParticipantAlert = unsafeCastId

instance IsNSObject (Id SWRemoveParticipantAlert) where
  toNSObject = unsafeCastId

-- ---------- SWCollaborationHighlight ----------

-- | SWCollaborationHighlight
--
-- A SWHighlight object that represents an active collaboration
-- 
-- Phantom type for @SWCollaborationHighlight@.
data SWCollaborationHighlight

instance IsObjCObject (Id SWCollaborationHighlight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationHighlight"

class IsSWHighlight a => IsSWCollaborationHighlight a where
  toSWCollaborationHighlight :: a -> Id SWCollaborationHighlight

instance IsSWCollaborationHighlight (Id SWCollaborationHighlight) where
  toSWCollaborationHighlight = unsafeCastId

instance IsNSObject (Id SWCollaborationHighlight) where
  toNSObject = unsafeCastId

instance IsSWHighlight (Id SWCollaborationHighlight) where
  toSWHighlight = unsafeCastId

-- ---------- SWAttributionView ----------

-- | Phantom type for @SWAttributionView@.
data SWAttributionView

instance IsObjCObject (Id SWAttributionView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWAttributionView"

class IsNSView a => IsSWAttributionView a where
  toSWAttributionView :: a -> Id SWAttributionView

instance IsSWAttributionView (Id SWAttributionView) where
  toSWAttributionView = unsafeCastId

instance IsNSObject (Id SWAttributionView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SWAttributionView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SWAttributionView) where
  toNSView = unsafeCastId

-- ---------- SWCollaborationView ----------

-- | Phantom type for @SWCollaborationView@.
data SWCollaborationView

instance IsObjCObject (Id SWCollaborationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SWCollaborationView"

class IsNSView a => IsSWCollaborationView a where
  toSWCollaborationView :: a -> Id SWCollaborationView

instance IsSWCollaborationView (Id SWCollaborationView) where
  toSWCollaborationView = unsafeCastId

instance IsNSObject (Id SWCollaborationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SWCollaborationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SWCollaborationView) where
  toNSView = unsafeCastId
