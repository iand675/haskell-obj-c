{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileManagerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileManagerDelegate defaultNSFileManagerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSFileManagerDelegate
  ( NSFileManagerDelegateOverrides(..)
  , defaultNSFileManagerDelegateOverrides
  , newNSFileManagerDelegate
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

-- | Overrides record for @\@protocol NSFileManagerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileManagerDelegateOverrides = NSFileManagerDelegateOverrides
  { _fileManager_shouldCopyItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldCopyItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_copyingItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_copyingItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldMoveItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldMoveItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_movingItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_movingItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldLinkItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldLinkItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_linkingItemAtPath_toPath :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_linkingItemAtURL_toURL :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldRemoveItemAtPath :: !(Maybe (RawId -> RawId -> IO Bool))
  , _fileManager_shouldRemoveItemAtURL :: !(Maybe (RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_removingItemAtPath :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _fileManager_shouldProceedAfterError_removingItemAtURL :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileManagerDelegateOverrides :: NSFileManagerDelegateOverrides
defaultNSFileManagerDelegateOverrides = NSFileManagerDelegateOverrides
  { _fileManager_shouldCopyItemAtPath_toPath = Nothing
  , _fileManager_shouldCopyItemAtURL_toURL = Nothing
  , _fileManager_shouldProceedAfterError_copyingItemAtPath_toPath = Nothing
  , _fileManager_shouldProceedAfterError_copyingItemAtURL_toURL = Nothing
  , _fileManager_shouldMoveItemAtPath_toPath = Nothing
  , _fileManager_shouldMoveItemAtURL_toURL = Nothing
  , _fileManager_shouldProceedAfterError_movingItemAtPath_toPath = Nothing
  , _fileManager_shouldProceedAfterError_movingItemAtURL_toURL = Nothing
  , _fileManager_shouldLinkItemAtPath_toPath = Nothing
  , _fileManager_shouldLinkItemAtURL_toURL = Nothing
  , _fileManager_shouldProceedAfterError_linkingItemAtPath_toPath = Nothing
  , _fileManager_shouldProceedAfterError_linkingItemAtURL_toURL = Nothing
  , _fileManager_shouldRemoveItemAtPath = Nothing
  , _fileManager_shouldRemoveItemAtURL = Nothing
  , _fileManager_shouldProceedAfterError_removingItemAtPath = Nothing
  , _fileManager_shouldProceedAfterError_removingItemAtURL = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileManagerDelegateDelegateClass #-}
nsFileManagerDelegateDelegateClass :: Class
nsFileManagerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileManagerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_fileManager_shouldCopyItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldCopyItemAtPath:toPath:")
      sel_fileManager_shouldCopyItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldCopyItemAtURL:toURL:")
      sel_fileManager_shouldProceedAfterError_copyingItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldProceedAfterError:copyingItemAtPath:toPath:")
      sel_fileManager_shouldProceedAfterError_copyingItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldProceedAfterError:copyingItemAtURL:toURL:")
      sel_fileManager_shouldMoveItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldMoveItemAtPath:toPath:")
      sel_fileManager_shouldMoveItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldMoveItemAtURL:toURL:")
      sel_fileManager_shouldProceedAfterError_movingItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldProceedAfterError:movingItemAtPath:toPath:")
      sel_fileManager_shouldProceedAfterError_movingItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldProceedAfterError:movingItemAtURL:toURL:")
      sel_fileManager_shouldLinkItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldLinkItemAtPath:toPath:")
      sel_fileManager_shouldLinkItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldLinkItemAtURL:toURL:")
      sel_fileManager_shouldProceedAfterError_linkingItemAtPath_toPath = unSelector (mkSelector "fileManager:shouldProceedAfterError:linkingItemAtPath:toPath:")
      sel_fileManager_shouldProceedAfterError_linkingItemAtURL_toURL = unSelector (mkSelector "fileManager:shouldProceedAfterError:linkingItemAtURL:toURL:")
      sel_fileManager_shouldRemoveItemAtPath = unSelector (mkSelector "fileManager:shouldRemoveItemAtPath:")
      sel_fileManager_shouldRemoveItemAtURL = unSelector (mkSelector "fileManager:shouldRemoveItemAtURL:")
      sel_fileManager_shouldProceedAfterError_removingItemAtPath = unSelector (mkSelector "fileManager:shouldProceedAfterError:removingItemAtPath:")
      sel_fileManager_shouldProceedAfterError_removingItemAtURL = unSelector (mkSelector "fileManager:shouldProceedAfterError:removingItemAtURL:")
  -- fileManager:shouldCopyItemAtPath:toPath:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldCopyItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldCopyItemAtPath:toPath:" "B@:@@@" stub_0

  -- fileManager:shouldCopyItemAtURL:toURL:
  stub_1 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldCopyItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldCopyItemAtURL:toURL:" "B@:@@@" stub_1

  -- fileManager:shouldProceedAfterError:copyingItemAtPath:toPath:
  stub_2 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_copyingItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:copyingItemAtPath:toPath:" "B@:@@@@" stub_2

  -- fileManager:shouldProceedAfterError:copyingItemAtURL:toURL:
  stub_3 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_copyingItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:copyingItemAtURL:toURL:" "B@:@@@@" stub_3

  -- fileManager:shouldMoveItemAtPath:toPath:
  stub_4 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldMoveItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldMoveItemAtPath:toPath:" "B@:@@@" stub_4

  -- fileManager:shouldMoveItemAtURL:toURL:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldMoveItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldMoveItemAtURL:toURL:" "B@:@@@" stub_5

  -- fileManager:shouldProceedAfterError:movingItemAtPath:toPath:
  stub_6 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_movingItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:movingItemAtPath:toPath:" "B@:@@@@" stub_6

  -- fileManager:shouldProceedAfterError:movingItemAtURL:toURL:
  stub_7 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_movingItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:movingItemAtURL:toURL:" "B@:@@@@" stub_7

  -- fileManager:shouldLinkItemAtPath:toPath:
  stub_8 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldLinkItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldLinkItemAtPath:toPath:" "B@:@@@" stub_8

  -- fileManager:shouldLinkItemAtURL:toURL:
  stub_9 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldLinkItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldLinkItemAtURL:toURL:" "B@:@@@" stub_9

  -- fileManager:shouldProceedAfterError:linkingItemAtPath:toPath:
  stub_10 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_linkingItemAtPath_toPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:linkingItemAtPath:toPath:" "B@:@@@@" stub_10

  -- fileManager:shouldProceedAfterError:linkingItemAtURL:toURL:
  stub_11 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_linkingItemAtURL_toURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:linkingItemAtURL:toURL:" "B@:@@@@" stub_11

  -- fileManager:shouldRemoveItemAtPath:
  stub_12 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldRemoveItemAtPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldRemoveItemAtPath:" "B@:@@" stub_12

  -- fileManager:shouldRemoveItemAtURL:
  stub_13 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldRemoveItemAtURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldRemoveItemAtURL:" "B@:@@" stub_13

  -- fileManager:shouldProceedAfterError:removingItemAtPath:
  stub_14 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_removingItemAtPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:removingItemAtPath:" "B@:@@@" stub_14

  -- fileManager:shouldProceedAfterError:removingItemAtURL:
  stub_15 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    case _fileManager_shouldProceedAfterError_removingItemAtURL rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "fileManager:shouldProceedAfterError:removingItemAtURL:" "B@:@@@" stub_15

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileManagerDelegateOverrides
    if queriedSel == sel_fileManager_shouldCopyItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldCopyItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldCopyItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldCopyItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_copyingItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_copyingItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_copyingItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_copyingItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldMoveItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldMoveItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldMoveItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldMoveItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_movingItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_movingItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_movingItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_movingItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldLinkItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldLinkItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldLinkItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldLinkItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_linkingItemAtPath_toPath then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_linkingItemAtPath_toPath rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_linkingItemAtURL_toURL then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_linkingItemAtURL_toURL rec_))
    else if queriedSel == sel_fileManager_shouldRemoveItemAtPath then pure (maybe 0 (const 1) (_fileManager_shouldRemoveItemAtPath rec_))
    else if queriedSel == sel_fileManager_shouldRemoveItemAtURL then pure (maybe 0 (const 1) (_fileManager_shouldRemoveItemAtURL rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_removingItemAtPath then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_removingItemAtPath rec_))
    else if queriedSel == sel_fileManager_shouldProceedAfterError_removingItemAtURL then pure (maybe 0 (const 1) (_fileManager_shouldProceedAfterError_removingItemAtURL rec_))
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
newNSFileManagerDelegate :: NSFileManagerDelegateOverrides -> IO RawId
newNSFileManagerDelegate overrides = do
  inst <- class_createInstance nsFileManagerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
