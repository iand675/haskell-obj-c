{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ISyncSessionDriverDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newISyncSessionDriverDataSource defaultISyncSessionDriverDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SyncServices.Delegate.ISyncSessionDriverDataSource
  ( ISyncSessionDriverDataSourceOverrides(..)
  , defaultISyncSessionDriverDataSourceOverrides
  , newISyncSessionDriverDataSource
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

-- | Overrides record for @\@protocol ISyncSessionDriverDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ISyncSessionDriverDataSourceOverrides = ISyncSessionDriverDataSourceOverrides
  { _clientIdentifier :: !(Maybe (IO RawId))
  , _clientDescriptionURL :: !(Maybe (IO RawId))
  , _schemaBundleURLs :: !(Maybe (IO RawId))
  , _recordsForEntityName_moreComing_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _deleteAllRecordsForEntityName_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _entityNamesToSync :: !(Maybe (IO RawId))
  , _entityNamesToPull :: !(Maybe (IO RawId))
  , _sessionBeginTimeout :: !(Maybe (IO Double))
  , _sessionPullChangesTimeout :: !(Maybe (IO Double))
  , _lastAnchorForEntityName :: !(Maybe (RawId -> IO RawId))
  , _nextAnchorForEntityName :: !(Maybe (RawId -> IO RawId))
  , _changedRecordsForEntityName_moreComing_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _changesForEntityName_moreComing_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _identifiersForRecordsToDeleteForEntityName_moreComing_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultISyncSessionDriverDataSourceOverrides :: ISyncSessionDriverDataSourceOverrides
defaultISyncSessionDriverDataSourceOverrides = ISyncSessionDriverDataSourceOverrides
  { _clientIdentifier = Nothing
  , _clientDescriptionURL = Nothing
  , _schemaBundleURLs = Nothing
  , _recordsForEntityName_moreComing_error = Nothing
  , _deleteAllRecordsForEntityName_error = Nothing
  , _entityNamesToSync = Nothing
  , _entityNamesToPull = Nothing
  , _sessionBeginTimeout = Nothing
  , _sessionPullChangesTimeout = Nothing
  , _lastAnchorForEntityName = Nothing
  , _nextAnchorForEntityName = Nothing
  , _changedRecordsForEntityName_moreComing_error = Nothing
  , _changesForEntityName_moreComing_error = Nothing
  , _identifiersForRecordsToDeleteForEntityName_moreComing_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE iSyncSessionDriverDataSourceDelegateClass #-}
iSyncSessionDriverDataSourceDelegateClass :: Class
iSyncSessionDriverDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsISyncSessionDriverDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_clientIdentifier = unSelector (mkSelector "clientIdentifier")
      sel_clientDescriptionURL = unSelector (mkSelector "clientDescriptionURL")
      sel_schemaBundleURLs = unSelector (mkSelector "schemaBundleURLs")
      sel_recordsForEntityName_moreComing_error = unSelector (mkSelector "recordsForEntityName:moreComing:error:")
      sel_deleteAllRecordsForEntityName_error = unSelector (mkSelector "deleteAllRecordsForEntityName:error:")
      sel_entityNamesToSync = unSelector (mkSelector "entityNamesToSync")
      sel_entityNamesToPull = unSelector (mkSelector "entityNamesToPull")
      sel_sessionBeginTimeout = unSelector (mkSelector "sessionBeginTimeout")
      sel_sessionPullChangesTimeout = unSelector (mkSelector "sessionPullChangesTimeout")
      sel_lastAnchorForEntityName = unSelector (mkSelector "lastAnchorForEntityName:")
      sel_nextAnchorForEntityName = unSelector (mkSelector "nextAnchorForEntityName:")
      sel_changedRecordsForEntityName_moreComing_error = unSelector (mkSelector "changedRecordsForEntityName:moreComing:error:")
      sel_changesForEntityName_moreComing_error = unSelector (mkSelector "changesForEntityName:moreComing:error:")
      sel_identifiersForRecordsToDeleteForEntityName_moreComing_error = unSelector (mkSelector "identifiersForRecordsToDeleteForEntityName:moreComing:error:")
  -- clientIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _clientIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "clientIdentifier" "@@:" stub_0

  -- clientDescriptionURL
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _clientDescriptionURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "clientDescriptionURL" "@@:" stub_1

  -- schemaBundleURLs
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _schemaBundleURLs rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "schemaBundleURLs" "@@:" stub_2

  -- recordsForEntityName:moreComing:error:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _recordsForEntityName_moreComing_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "recordsForEntityName:moreComing:error:" "@@:@@@" stub_3

  -- deleteAllRecordsForEntityName:error:
  stub_4 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _deleteAllRecordsForEntityName_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "deleteAllRecordsForEntityName:error:" "B@:@@" stub_4

  -- entityNamesToSync
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _entityNamesToSync rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "entityNamesToSync" "@@:" stub_5

  -- entityNamesToPull
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _entityNamesToPull rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "entityNamesToPull" "@@:" stub_6

  -- sessionBeginTimeout
  stub_7 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _sessionBeginTimeout rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "sessionBeginTimeout" "d@:" stub_7

  -- sessionPullChangesTimeout
  stub_8 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _sessionPullChangesTimeout rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "sessionPullChangesTimeout" "d@:" stub_8

  -- lastAnchorForEntityName:
  stub_9 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _lastAnchorForEntityName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "lastAnchorForEntityName:" "@@:@" stub_9

  -- nextAnchorForEntityName:
  stub_10 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _nextAnchorForEntityName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "nextAnchorForEntityName:" "@@:@" stub_10

  -- changedRecordsForEntityName:moreComing:error:
  stub_11 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _changedRecordsForEntityName_moreComing_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "changedRecordsForEntityName:moreComing:error:" "@@:@@@" stub_11

  -- changesForEntityName:moreComing:error:
  stub_12 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _changesForEntityName_moreComing_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "changesForEntityName:moreComing:error:" "@@:@@@" stub_12

  -- identifiersForRecordsToDeleteForEntityName:moreComing:error:
  stub_13 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    case _identifiersForRecordsToDeleteForEntityName_moreComing_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "identifiersForRecordsToDeleteForEntityName:moreComing:error:" "@@:@@@" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncSessionDriverDataSourceOverrides
    if queriedSel == sel_clientIdentifier then pure (maybe 0 (const 1) (_clientIdentifier rec_))
    else if queriedSel == sel_clientDescriptionURL then pure (maybe 0 (const 1) (_clientDescriptionURL rec_))
    else if queriedSel == sel_schemaBundleURLs then pure (maybe 0 (const 1) (_schemaBundleURLs rec_))
    else if queriedSel == sel_recordsForEntityName_moreComing_error then pure (maybe 0 (const 1) (_recordsForEntityName_moreComing_error rec_))
    else if queriedSel == sel_deleteAllRecordsForEntityName_error then pure (maybe 0 (const 1) (_deleteAllRecordsForEntityName_error rec_))
    else if queriedSel == sel_entityNamesToSync then pure (maybe 0 (const 1) (_entityNamesToSync rec_))
    else if queriedSel == sel_entityNamesToPull then pure (maybe 0 (const 1) (_entityNamesToPull rec_))
    else if queriedSel == sel_sessionBeginTimeout then pure (maybe 0 (const 1) (_sessionBeginTimeout rec_))
    else if queriedSel == sel_sessionPullChangesTimeout then pure (maybe 0 (const 1) (_sessionPullChangesTimeout rec_))
    else if queriedSel == sel_lastAnchorForEntityName then pure (maybe 0 (const 1) (_lastAnchorForEntityName rec_))
    else if queriedSel == sel_nextAnchorForEntityName then pure (maybe 0 (const 1) (_nextAnchorForEntityName rec_))
    else if queriedSel == sel_changedRecordsForEntityName_moreComing_error then pure (maybe 0 (const 1) (_changedRecordsForEntityName_moreComing_error rec_))
    else if queriedSel == sel_changesForEntityName_moreComing_error then pure (maybe 0 (const 1) (_changesForEntityName_moreComing_error rec_))
    else if queriedSel == sel_identifiersForRecordsToDeleteForEntityName_moreComing_error then pure (maybe 0 (const 1) (_identifiersForRecordsToDeleteForEntityName_moreComing_error rec_))
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
newISyncSessionDriverDataSource :: ISyncSessionDriverDataSourceOverrides -> IO RawId
newISyncSessionDriverDataSource overrides = do
  inst <- class_createInstance iSyncSessionDriverDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
