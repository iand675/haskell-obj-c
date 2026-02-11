{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTRStorage@.
--
-- Usage:
--
-- @
-- delegate <- newMTRStorage defaultMTRStorageOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Matter.Delegate.MTRStorage
  ( MTRStorageOverrides(..)
  , defaultMTRStorageOverrides
  , newMTRStorage
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

-- | Overrides record for @\@protocol MTRStorage@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTRStorageOverrides = MTRStorageOverrides
  { _storageDataForKey :: !(Maybe (RawId -> IO RawId))
  , _setStorageData_forKey :: !(Maybe (RawId -> RawId -> IO Bool))
  , _removeStorageDataForKey :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMTRStorageOverrides :: MTRStorageOverrides
defaultMTRStorageOverrides = MTRStorageOverrides
  { _storageDataForKey = Nothing
  , _setStorageData_forKey = Nothing
  , _removeStorageDataForKey = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtrStorageDelegateClass #-}
mtrStorageDelegateClass :: Class
mtrStorageDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTRStorage" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_storageDataForKey = unSelector (mkSelector "storageDataForKey:")
      sel_setStorageData_forKey = unSelector (mkSelector "setStorageData:forKey:")
      sel_removeStorageDataForKey = unSelector (mkSelector "removeStorageDataForKey:")
  -- storageDataForKey:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRStorageOverrides
    case _storageDataForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "storageDataForKey:" "@@:@" stub_0

  -- setStorageData:forKey:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRStorageOverrides
    case _setStorageData_forKey rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setStorageData:forKey:" "B@:@@" stub_1

  -- removeStorageDataForKey:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRStorageOverrides
    case _removeStorageDataForKey rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "removeStorageDataForKey:" "B@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTRStorageOverrides
    if queriedSel == sel_storageDataForKey then pure (maybe 0 (const 1) (_storageDataForKey rec_))
    else if queriedSel == sel_setStorageData_forKey then pure (maybe 0 (const 1) (_setStorageData_forKey rec_))
    else if queriedSel == sel_removeStorageDataForKey then pure (maybe 0 (const 1) (_removeStorageDataForKey rec_))
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
newMTRStorage :: MTRStorageOverrides -> IO RawId
newMTRStorage overrides = do
  inst <- class_createInstance mtrStorageDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
