{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CKRecordKeyValueSetting@.
--
-- Usage:
--
-- @
-- delegate <- newCKRecordKeyValueSetting defaultCKRecordKeyValueSettingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CloudKit.Delegate.CKRecordKeyValueSetting
  ( CKRecordKeyValueSettingOverrides(..)
  , defaultCKRecordKeyValueSettingOverrides
  , newCKRecordKeyValueSetting
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

-- | Overrides record for @\@protocol CKRecordKeyValueSetting@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CKRecordKeyValueSettingOverrides = CKRecordKeyValueSettingOverrides
  { _objectForKey :: !(Maybe (RawId -> IO RawId))
  , _setObject_forKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _objectForKeyedSubscript :: !(Maybe (RawId -> IO RawId))
  , _setObject_forKeyedSubscript :: !(Maybe (RawId -> RawId -> IO ()))
  , _allKeys :: !(Maybe (IO RawId))
  , _changedKeys :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultCKRecordKeyValueSettingOverrides :: CKRecordKeyValueSettingOverrides
defaultCKRecordKeyValueSettingOverrides = CKRecordKeyValueSettingOverrides
  { _objectForKey = Nothing
  , _setObject_forKey = Nothing
  , _objectForKeyedSubscript = Nothing
  , _setObject_forKeyedSubscript = Nothing
  , _allKeys = Nothing
  , _changedKeys = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ckRecordKeyValueSettingDelegateClass #-}
ckRecordKeyValueSettingDelegateClass :: Class
ckRecordKeyValueSettingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCKRecordKeyValueSetting" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_objectForKey = unSelector (mkSelector "objectForKey:")
      sel_setObject_forKey = unSelector (mkSelector "setObject:forKey:")
      sel_objectForKeyedSubscript = unSelector (mkSelector "objectForKeyedSubscript:")
      sel_setObject_forKeyedSubscript = unSelector (mkSelector "setObject:forKeyedSubscript:")
      sel_allKeys = unSelector (mkSelector "allKeys")
      sel_changedKeys = unSelector (mkSelector "changedKeys")
  -- objectForKey:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _objectForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objectForKey:" "@@:@" stub_0

  -- setObject:forKey:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _setObject_forKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "setObject:forKey:" "v@:@@" stub_1

  -- objectForKeyedSubscript:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _objectForKeyedSubscript rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objectForKeyedSubscript:" "@@:@" stub_2

  -- setObject:forKeyedSubscript:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _setObject_forKeyedSubscript rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "setObject:forKeyedSubscript:" "v@:@@" stub_3

  -- allKeys
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _allKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "allKeys" "@@:" stub_4

  -- changedKeys
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    case _changedKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "changedKeys" "@@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CKRecordKeyValueSettingOverrides
    if queriedSel == sel_objectForKey then pure (maybe 0 (const 1) (_objectForKey rec_))
    else if queriedSel == sel_setObject_forKey then pure (maybe 0 (const 1) (_setObject_forKey rec_))
    else if queriedSel == sel_objectForKeyedSubscript then pure (maybe 0 (const 1) (_objectForKeyedSubscript rec_))
    else if queriedSel == sel_setObject_forKeyedSubscript then pure (maybe 0 (const 1) (_setObject_forKeyedSubscript rec_))
    else if queriedSel == sel_allKeys then pure (maybe 0 (const 1) (_allKeys rec_))
    else if queriedSel == sel_changedKeys then pure (maybe 0 (const 1) (_changedKeys rec_))
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
newCKRecordKeyValueSetting :: CKRecordKeyValueSettingOverrides -> IO RawId
newCKRecordKeyValueSetting overrides = do
  inst <- class_createInstance ckRecordKeyValueSettingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
