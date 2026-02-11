{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CallKit.Internal.Classes (
    module ObjC.CallKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CXAction ----------

-- | Phantom type for @CXAction@.
data CXAction

instance IsObjCObject (Id CXAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXAction"

class IsNSObject a => IsCXAction a where
  toCXAction :: a -> Id CXAction

instance IsCXAction (Id CXAction) where
  toCXAction = unsafeCastId

instance IsNSObject (Id CXAction) where
  toNSObject = unsafeCastId

-- ---------- CXCall ----------

-- | Phantom type for @CXCall@.
data CXCall

instance IsObjCObject (Id CXCall) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCall"

class IsNSObject a => IsCXCall a where
  toCXCall :: a -> Id CXCall

instance IsCXCall (Id CXCall) where
  toCXCall = unsafeCastId

instance IsNSObject (Id CXCall) where
  toNSObject = unsafeCastId

-- ---------- CXCallController ----------

-- | Phantom type for @CXCallController@.
data CXCallController

instance IsObjCObject (Id CXCallController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallController"

class IsNSObject a => IsCXCallController a where
  toCXCallController :: a -> Id CXCallController

instance IsCXCallController (Id CXCallController) where
  toCXCallController = unsafeCastId

instance IsNSObject (Id CXCallController) where
  toNSObject = unsafeCastId

-- ---------- CXCallDirectoryManager ----------

-- | Phantom type for @CXCallDirectoryManager@.
data CXCallDirectoryManager

instance IsObjCObject (Id CXCallDirectoryManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallDirectoryManager"

class IsNSObject a => IsCXCallDirectoryManager a where
  toCXCallDirectoryManager :: a -> Id CXCallDirectoryManager

instance IsCXCallDirectoryManager (Id CXCallDirectoryManager) where
  toCXCallDirectoryManager = unsafeCastId

instance IsNSObject (Id CXCallDirectoryManager) where
  toNSObject = unsafeCastId

-- ---------- CXCallDirectoryProvider ----------

-- | Phantom type for @CXCallDirectoryProvider@.
data CXCallDirectoryProvider

instance IsObjCObject (Id CXCallDirectoryProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallDirectoryProvider"

class IsNSObject a => IsCXCallDirectoryProvider a where
  toCXCallDirectoryProvider :: a -> Id CXCallDirectoryProvider

instance IsCXCallDirectoryProvider (Id CXCallDirectoryProvider) where
  toCXCallDirectoryProvider = unsafeCastId

instance IsNSObject (Id CXCallDirectoryProvider) where
  toNSObject = unsafeCastId

-- ---------- CXCallObserver ----------

-- | Phantom type for @CXCallObserver@.
data CXCallObserver

instance IsObjCObject (Id CXCallObserver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallObserver"

class IsNSObject a => IsCXCallObserver a where
  toCXCallObserver :: a -> Id CXCallObserver

instance IsCXCallObserver (Id CXCallObserver) where
  toCXCallObserver = unsafeCastId

instance IsNSObject (Id CXCallObserver) where
  toNSObject = unsafeCastId

-- ---------- CXCallUpdate ----------

-- | Phantom type for @CXCallUpdate@.
data CXCallUpdate

instance IsObjCObject (Id CXCallUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallUpdate"

class IsNSObject a => IsCXCallUpdate a where
  toCXCallUpdate :: a -> Id CXCallUpdate

instance IsCXCallUpdate (Id CXCallUpdate) where
  toCXCallUpdate = unsafeCastId

instance IsNSObject (Id CXCallUpdate) where
  toNSObject = unsafeCastId

-- ---------- CXHandle ----------

-- | Phantom type for @CXHandle@.
data CXHandle

instance IsObjCObject (Id CXHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXHandle"

class IsNSObject a => IsCXHandle a where
  toCXHandle :: a -> Id CXHandle

instance IsCXHandle (Id CXHandle) where
  toCXHandle = unsafeCastId

instance IsNSObject (Id CXHandle) where
  toNSObject = unsafeCastId

-- ---------- CXProvider ----------

-- | Phantom type for @CXProvider@.
data CXProvider

instance IsObjCObject (Id CXProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXProvider"

class IsNSObject a => IsCXProvider a where
  toCXProvider :: a -> Id CXProvider

instance IsCXProvider (Id CXProvider) where
  toCXProvider = unsafeCastId

instance IsNSObject (Id CXProvider) where
  toNSObject = unsafeCastId

-- ---------- CXProviderConfiguration ----------

-- | Phantom type for @CXProviderConfiguration@.
data CXProviderConfiguration

instance IsObjCObject (Id CXProviderConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXProviderConfiguration"

class IsNSObject a => IsCXProviderConfiguration a where
  toCXProviderConfiguration :: a -> Id CXProviderConfiguration

instance IsCXProviderConfiguration (Id CXProviderConfiguration) where
  toCXProviderConfiguration = unsafeCastId

instance IsNSObject (Id CXProviderConfiguration) where
  toNSObject = unsafeCastId

-- ---------- CXTransaction ----------

-- | Phantom type for @CXTransaction@.
data CXTransaction

instance IsObjCObject (Id CXTransaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXTransaction"

class IsNSObject a => IsCXTransaction a where
  toCXTransaction :: a -> Id CXTransaction

instance IsCXTransaction (Id CXTransaction) where
  toCXTransaction = unsafeCastId

instance IsNSObject (Id CXTransaction) where
  toNSObject = unsafeCastId

-- ---------- CXCallAction ----------

-- | Phantom type for @CXCallAction@.
data CXCallAction

instance IsObjCObject (Id CXCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallAction"

class IsCXAction a => IsCXCallAction a where
  toCXCallAction :: a -> Id CXCallAction

instance IsCXCallAction (Id CXCallAction) where
  toCXCallAction = unsafeCastId

instance IsCXAction (Id CXCallAction) where
  toCXAction = unsafeCastId

instance IsNSObject (Id CXCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXCallDirectoryExtensionContext ----------

-- | Phantom type for @CXCallDirectoryExtensionContext@.
data CXCallDirectoryExtensionContext

instance IsObjCObject (Id CXCallDirectoryExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXCallDirectoryExtensionContext"

class IsNSExtensionContext a => IsCXCallDirectoryExtensionContext a where
  toCXCallDirectoryExtensionContext :: a -> Id CXCallDirectoryExtensionContext

instance IsCXCallDirectoryExtensionContext (Id CXCallDirectoryExtensionContext) where
  toCXCallDirectoryExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id CXCallDirectoryExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id CXCallDirectoryExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- CXAnswerCallAction ----------

-- | Phantom type for @CXAnswerCallAction@.
data CXAnswerCallAction

instance IsObjCObject (Id CXAnswerCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXAnswerCallAction"

class IsCXCallAction a => IsCXAnswerCallAction a where
  toCXAnswerCallAction :: a -> Id CXAnswerCallAction

instance IsCXAnswerCallAction (Id CXAnswerCallAction) where
  toCXAnswerCallAction = unsafeCastId

instance IsCXAction (Id CXAnswerCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXAnswerCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXAnswerCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXEndCallAction ----------

-- | Phantom type for @CXEndCallAction@.
data CXEndCallAction

instance IsObjCObject (Id CXEndCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXEndCallAction"

class IsCXCallAction a => IsCXEndCallAction a where
  toCXEndCallAction :: a -> Id CXEndCallAction

instance IsCXEndCallAction (Id CXEndCallAction) where
  toCXEndCallAction = unsafeCastId

instance IsCXAction (Id CXEndCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXEndCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXEndCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXPlayDTMFCallAction ----------

-- | Phantom type for @CXPlayDTMFCallAction@.
data CXPlayDTMFCallAction

instance IsObjCObject (Id CXPlayDTMFCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXPlayDTMFCallAction"

class IsCXCallAction a => IsCXPlayDTMFCallAction a where
  toCXPlayDTMFCallAction :: a -> Id CXPlayDTMFCallAction

instance IsCXPlayDTMFCallAction (Id CXPlayDTMFCallAction) where
  toCXPlayDTMFCallAction = unsafeCastId

instance IsCXAction (Id CXPlayDTMFCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXPlayDTMFCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXPlayDTMFCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXSetGroupCallAction ----------

-- | Phantom type for @CXSetGroupCallAction@.
data CXSetGroupCallAction

instance IsObjCObject (Id CXSetGroupCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXSetGroupCallAction"

class IsCXCallAction a => IsCXSetGroupCallAction a where
  toCXSetGroupCallAction :: a -> Id CXSetGroupCallAction

instance IsCXSetGroupCallAction (Id CXSetGroupCallAction) where
  toCXSetGroupCallAction = unsafeCastId

instance IsCXAction (Id CXSetGroupCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXSetGroupCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXSetGroupCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXSetHeldCallAction ----------

-- | Phantom type for @CXSetHeldCallAction@.
data CXSetHeldCallAction

instance IsObjCObject (Id CXSetHeldCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXSetHeldCallAction"

class IsCXCallAction a => IsCXSetHeldCallAction a where
  toCXSetHeldCallAction :: a -> Id CXSetHeldCallAction

instance IsCXSetHeldCallAction (Id CXSetHeldCallAction) where
  toCXSetHeldCallAction = unsafeCastId

instance IsCXAction (Id CXSetHeldCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXSetHeldCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXSetHeldCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXSetMutedCallAction ----------

-- | Phantom type for @CXSetMutedCallAction@.
data CXSetMutedCallAction

instance IsObjCObject (Id CXSetMutedCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXSetMutedCallAction"

class IsCXCallAction a => IsCXSetMutedCallAction a where
  toCXSetMutedCallAction :: a -> Id CXSetMutedCallAction

instance IsCXSetMutedCallAction (Id CXSetMutedCallAction) where
  toCXSetMutedCallAction = unsafeCastId

instance IsCXAction (Id CXSetMutedCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXSetMutedCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXSetMutedCallAction) where
  toNSObject = unsafeCastId

-- ---------- CXStartCallAction ----------

-- | Phantom type for @CXStartCallAction@.
data CXStartCallAction

instance IsObjCObject (Id CXStartCallAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CXStartCallAction"

class IsCXCallAction a => IsCXStartCallAction a where
  toCXStartCallAction :: a -> Id CXStartCallAction

instance IsCXStartCallAction (Id CXStartCallAction) where
  toCXStartCallAction = unsafeCastId

instance IsCXAction (Id CXStartCallAction) where
  toCXAction = unsafeCastId

instance IsCXCallAction (Id CXStartCallAction) where
  toCXCallAction = unsafeCastId

instance IsNSObject (Id CXStartCallAction) where
  toNSObject = unsafeCastId
