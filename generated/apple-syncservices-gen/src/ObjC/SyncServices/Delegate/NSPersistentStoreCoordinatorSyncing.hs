{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPersistentStoreCoordinatorSyncing@.
--
-- Usage:
--
-- @
-- delegate <- newNSPersistentStoreCoordinatorSyncing defaultNSPersistentStoreCoordinatorSyncingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SyncServices.Delegate.NSPersistentStoreCoordinatorSyncing
  ( NSPersistentStoreCoordinatorSyncingOverrides(..)
  , defaultNSPersistentStoreCoordinatorSyncingOverrides
  , newNSPersistentStoreCoordinatorSyncing
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol NSPersistentStoreCoordinatorSyncing@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPersistentStoreCoordinatorSyncingOverrides = NSPersistentStoreCoordinatorSyncingOverrides
  { _managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator :: !(Maybe (RawId -> IO RawId))
  , _managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator :: !(Maybe (RawId -> IO RawId))
  , _persistentStoreCoordinatorShouldStartSyncing :: !(Maybe (RawId -> IO Bool))
  , _persistentStoreCoordinator_willPushChangesInSyncSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_didPushChangesInSyncSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_willPullChangesInSyncSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_didPullChangesInSyncSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_didFinishSyncSession :: !(Maybe (RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_didCancelSyncSession_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _persistentStoreCoordinator_didCommitChanges_inSyncSession :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPersistentStoreCoordinatorSyncingOverrides :: NSPersistentStoreCoordinatorSyncingOverrides
defaultNSPersistentStoreCoordinatorSyncingOverrides = NSPersistentStoreCoordinatorSyncingOverrides
  { _managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator = Nothing
  , _managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator = Nothing
  , _persistentStoreCoordinatorShouldStartSyncing = Nothing
  , _persistentStoreCoordinator_willPushChangesInSyncSession = Nothing
  , _persistentStoreCoordinator_didPushChangesInSyncSession = Nothing
  , _persistentStoreCoordinator_willPullChangesInSyncSession = Nothing
  , _persistentStoreCoordinator_didPullChangesInSyncSession = Nothing
  , _persistentStoreCoordinator_didFinishSyncSession = Nothing
  , _persistentStoreCoordinator_didCancelSyncSession_error = Nothing
  , _persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession = Nothing
  , _persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession = Nothing
  , _persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession = Nothing
  , _persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession = Nothing
  , _persistentStoreCoordinator_didCommitChanges_inSyncSession = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPersistentStoreCoordinatorSyncingDelegateClass #-}
nsPersistentStoreCoordinatorSyncingDelegateClass :: Class
nsPersistentStoreCoordinatorSyncingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPersistentStoreCoordinatorSyncing" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator = unSelector (mkSelector "managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator:")
      sel_managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator = unSelector (mkSelector "managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator:")
      sel_persistentStoreCoordinatorShouldStartSyncing = unSelector (mkSelector "persistentStoreCoordinatorShouldStartSyncing:")
      sel_persistentStoreCoordinator_willPushChangesInSyncSession = unSelector (mkSelector "persistentStoreCoordinator:willPushChangesInSyncSession:")
      sel_persistentStoreCoordinator_didPushChangesInSyncSession = unSelector (mkSelector "persistentStoreCoordinator:didPushChangesInSyncSession:")
      sel_persistentStoreCoordinator_willPullChangesInSyncSession = unSelector (mkSelector "persistentStoreCoordinator:willPullChangesInSyncSession:")
      sel_persistentStoreCoordinator_didPullChangesInSyncSession = unSelector (mkSelector "persistentStoreCoordinator:didPullChangesInSyncSession:")
      sel_persistentStoreCoordinator_didFinishSyncSession = unSelector (mkSelector "persistentStoreCoordinator:didFinishSyncSession:")
      sel_persistentStoreCoordinator_didCancelSyncSession_error = unSelector (mkSelector "persistentStoreCoordinator:didCancelSyncSession:error:")
      sel_persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession = unSelector (mkSelector "persistentStoreCoordinator:willPushRecord:forManagedObject:inSyncSession:")
      sel_persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession = unSelector (mkSelector "persistentStoreCoordinator:willDeleteRecordWithIdentifier:inSyncSession:")
      sel_persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession = unSelector (mkSelector "persistentStoreCoordinator:willApplyChange:toManagedObject:inSyncSession:")
      sel_persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession = unSelector (mkSelector "persistentStoreCoordinator:didApplyChange:toManagedObject:inSyncSession:")
      sel_persistentStoreCoordinator_didCommitChanges_inSyncSession = unSelector (mkSelector "persistentStoreCoordinator:didCommitChanges:inSyncSession:")
  -- managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator:" "@@:@" stub_0

  -- managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator:" "@@:@" stub_1

  -- persistentStoreCoordinatorShouldStartSyncing:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinatorShouldStartSyncing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "persistentStoreCoordinatorShouldStartSyncing:" "B@:@" stub_2

  -- persistentStoreCoordinator:willPushChangesInSyncSession:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_willPushChangesInSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "persistentStoreCoordinator:willPushChangesInSyncSession:" "v@:@@" stub_3

  -- persistentStoreCoordinator:didPushChangesInSyncSession:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didPushChangesInSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "persistentStoreCoordinator:didPushChangesInSyncSession:" "v@:@@" stub_4

  -- persistentStoreCoordinator:willPullChangesInSyncSession:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_willPullChangesInSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "persistentStoreCoordinator:willPullChangesInSyncSession:" "v@:@@" stub_5

  -- persistentStoreCoordinator:didPullChangesInSyncSession:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didPullChangesInSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "persistentStoreCoordinator:didPullChangesInSyncSession:" "v@:@@" stub_6

  -- persistentStoreCoordinator:didFinishSyncSession:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didFinishSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "persistentStoreCoordinator:didFinishSyncSession:" "v@:@@" stub_7

  -- persistentStoreCoordinator:didCancelSyncSession:error:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didCancelSyncSession_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "persistentStoreCoordinator:didCancelSyncSession:error:" "v@:@@@" stub_8

  -- persistentStoreCoordinator:willPushRecord:forManagedObject:inSyncSession:
  stub_9 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "persistentStoreCoordinator:willPushRecord:forManagedObject:inSyncSession:" "@@:@@@@" stub_9

  -- persistentStoreCoordinator:willDeleteRecordWithIdentifier:inSyncSession:
  stub_10 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "persistentStoreCoordinator:willDeleteRecordWithIdentifier:inSyncSession:" "B@:@@@" stub_10

  -- persistentStoreCoordinator:willApplyChange:toManagedObject:inSyncSession:
  stub_11 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "persistentStoreCoordinator:willApplyChange:toManagedObject:inSyncSession:" "@@:@@@@" stub_11

  -- persistentStoreCoordinator:didApplyChange:toManagedObject:inSyncSession:
  stub_12 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "persistentStoreCoordinator:didApplyChange:toManagedObject:inSyncSession:" "v@:@@@@" stub_12

  -- persistentStoreCoordinator:didCommitChanges:inSyncSession:
  stub_13 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    case _persistentStoreCoordinator_didCommitChanges_inSyncSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "persistentStoreCoordinator:didCommitChanges:inSyncSession:" "v@:@@@" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPersistentStoreCoordinatorSyncingOverrides
    if queriedSel == sel_managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator then pure (maybe 0 (const 1) (_managedObjectContextsToMonitorWhenSyncingPersistentStoreCoordinator rec_))
    else if queriedSel == sel_managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator then pure (maybe 0 (const 1) (_managedObjectContextsToReloadAfterSyncingPersistentStoreCoordinator rec_))
    else if queriedSel == sel_persistentStoreCoordinatorShouldStartSyncing then pure (maybe 0 (const 1) (_persistentStoreCoordinatorShouldStartSyncing rec_))
    else if queriedSel == sel_persistentStoreCoordinator_willPushChangesInSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_willPushChangesInSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didPushChangesInSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didPushChangesInSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_willPullChangesInSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_willPullChangesInSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didPullChangesInSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didPullChangesInSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didFinishSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didFinishSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didCancelSyncSession_error then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didCancelSyncSession_error rec_))
    else if queriedSel == sel_persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_willPushRecord_forManagedObject_inSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_willDeleteRecordWithIdentifier_inSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_willApplyChange_toManagedObject_inSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didApplyChange_toManagedObject_inSyncSession rec_))
    else if queriedSel == sel_persistentStoreCoordinator_didCommitChanges_inSyncSession then pure (maybe 0 (const 1) (_persistentStoreCoordinator_didCommitChanges_inSyncSession rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newNSPersistentStoreCoordinatorSyncing :: NSPersistentStoreCoordinatorSyncingOverrides -> IO RawId
newNSPersistentStoreCoordinatorSyncing overrides = do
  inst <- class_createInstance nsPersistentStoreCoordinatorSyncingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
