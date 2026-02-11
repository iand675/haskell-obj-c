{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Automator.Internal.Classes (
    module ObjC.Automator.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.OSAKit.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.OSAKit.Internal.Classes

-- ---------- AMAction ----------

-- | Phantom type for @AMAction@.
data AMAction

instance IsObjCObject (Id AMAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMAction"

class IsNSObject a => IsAMAction a where
  toAMAction :: a -> Id AMAction

instance IsAMAction (Id AMAction) where
  toAMAction = unsafeCastId

instance IsNSObject (Id AMAction) where
  toNSObject = unsafeCastId

-- ---------- AMWorkflow ----------

-- | Phantom type for @AMWorkflow@.
data AMWorkflow

instance IsObjCObject (Id AMWorkflow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMWorkflow"

class IsNSObject a => IsAMWorkflow a where
  toAMWorkflow :: a -> Id AMWorkflow

instance IsAMWorkflow (Id AMWorkflow) where
  toAMWorkflow = unsafeCastId

instance IsNSObject (Id AMWorkflow) where
  toNSObject = unsafeCastId

-- ---------- AMWorkspace ----------

-- | Phantom type for @AMWorkspace@.
data AMWorkspace

instance IsObjCObject (Id AMWorkspace) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMWorkspace"

class IsNSObject a => IsAMWorkspace a where
  toAMWorkspace :: a -> Id AMWorkspace

instance IsAMWorkspace (Id AMWorkspace) where
  toAMWorkspace = unsafeCastId

instance IsNSObject (Id AMWorkspace) where
  toNSObject = unsafeCastId

-- ---------- OSAScript ----------

-- | Phantom type for @OSAScript@.
data OSAScript

instance IsObjCObject (Id OSAScript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "OSAScript"

class IsNSObject a => IsOSAScript a where
  toOSAScript :: a -> Id OSAScript

instance IsOSAScript (Id OSAScript) where
  toOSAScript = unsafeCastId

instance IsNSObject (Id OSAScript) where
  toNSObject = unsafeCastId

-- ---------- AMBundleAction ----------

-- | Phantom type for @AMBundleAction@.
data AMBundleAction

instance IsObjCObject (Id AMBundleAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMBundleAction"

class IsAMAction a => IsAMBundleAction a where
  toAMBundleAction :: a -> Id AMBundleAction

instance IsAMBundleAction (Id AMBundleAction) where
  toAMBundleAction = unsafeCastId

instance IsAMAction (Id AMBundleAction) where
  toAMAction = unsafeCastId

instance IsNSObject (Id AMBundleAction) where
  toNSObject = unsafeCastId

-- ---------- AMWorkflowController ----------

-- | Phantom type for @AMWorkflowController@.
data AMWorkflowController

instance IsObjCObject (Id AMWorkflowController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMWorkflowController"

class IsNSController a => IsAMWorkflowController a where
  toAMWorkflowController :: a -> Id AMWorkflowController

instance IsAMWorkflowController (Id AMWorkflowController) where
  toAMWorkflowController = unsafeCastId

instance IsNSController (Id AMWorkflowController) where
  toNSController = unsafeCastId

instance IsNSObject (Id AMWorkflowController) where
  toNSObject = unsafeCastId

-- ---------- AMAppleScriptAction ----------

-- | Phantom type for @AMAppleScriptAction@.
data AMAppleScriptAction

instance IsObjCObject (Id AMAppleScriptAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMAppleScriptAction"

class IsAMBundleAction a => IsAMAppleScriptAction a where
  toAMAppleScriptAction :: a -> Id AMAppleScriptAction

instance IsAMAppleScriptAction (Id AMAppleScriptAction) where
  toAMAppleScriptAction = unsafeCastId

instance IsAMAction (Id AMAppleScriptAction) where
  toAMAction = unsafeCastId

instance IsAMBundleAction (Id AMAppleScriptAction) where
  toAMBundleAction = unsafeCastId

instance IsNSObject (Id AMAppleScriptAction) where
  toNSObject = unsafeCastId

-- ---------- AMShellScriptAction ----------

-- | Phantom type for @AMShellScriptAction@.
data AMShellScriptAction

instance IsObjCObject (Id AMShellScriptAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMShellScriptAction"

class IsAMBundleAction a => IsAMShellScriptAction a where
  toAMShellScriptAction :: a -> Id AMShellScriptAction

instance IsAMShellScriptAction (Id AMShellScriptAction) where
  toAMShellScriptAction = unsafeCastId

instance IsAMAction (Id AMShellScriptAction) where
  toAMAction = unsafeCastId

instance IsAMBundleAction (Id AMShellScriptAction) where
  toAMBundleAction = unsafeCastId

instance IsNSObject (Id AMShellScriptAction) where
  toNSObject = unsafeCastId

-- ---------- AMWorkflowView ----------

-- | Phantom type for @AMWorkflowView@.
data AMWorkflowView

instance IsObjCObject (Id AMWorkflowView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AMWorkflowView"

class IsNSView a => IsAMWorkflowView a where
  toAMWorkflowView :: a -> Id AMWorkflowView

instance IsAMWorkflowView (Id AMWorkflowView) where
  toAMWorkflowView = unsafeCastId

instance IsNSObject (Id AMWorkflowView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AMWorkflowView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AMWorkflowView) where
  toNSView = unsafeCastId
