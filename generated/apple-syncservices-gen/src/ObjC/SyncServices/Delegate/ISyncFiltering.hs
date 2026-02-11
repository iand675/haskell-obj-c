{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol ISyncFiltering@.
--
-- Usage:
--
-- @
-- delegate <- newISyncFiltering defaultISyncFilteringOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SyncServices.Delegate.ISyncFiltering
  ( ISyncFilteringOverrides(..)
  , defaultISyncFilteringOverrides
  , newISyncFiltering
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

-- | Overrides record for @\@protocol ISyncFiltering@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data ISyncFilteringOverrides = ISyncFilteringOverrides
  { _isEqual :: !(Maybe (RawId -> IO Bool))
  , _supportedEntityNames :: !(Maybe (IO RawId))
  , _shouldApplyRecord_withRecordIdentifier :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultISyncFilteringOverrides :: ISyncFilteringOverrides
defaultISyncFilteringOverrides = ISyncFilteringOverrides
  { _isEqual = Nothing
  , _supportedEntityNames = Nothing
  , _shouldApplyRecord_withRecordIdentifier = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE iSyncFilteringDelegateClass #-}
iSyncFilteringDelegateClass :: Class
iSyncFilteringDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsISyncFiltering" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_isEqual = unSelector (mkSelector "isEqual:")
      sel_supportedEntityNames = unSelector (mkSelector "supportedEntityNames")
      sel_shouldApplyRecord_withRecordIdentifier = unSelector (mkSelector "shouldApplyRecord:withRecordIdentifier:")
  -- isEqual:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncFilteringOverrides
    case _isEqual rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "isEqual:" "B@:@" stub_0

  -- supportedEntityNames
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncFilteringOverrides
    case _supportedEntityNames rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedEntityNames" "@@:" stub_1

  -- shouldApplyRecord:withRecordIdentifier:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncFilteringOverrides
    case _shouldApplyRecord_withRecordIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldApplyRecord:withRecordIdentifier:" "B@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO ISyncFilteringOverrides
    if queriedSel == sel_isEqual then pure (maybe 0 (const 1) (_isEqual rec_))
    else if queriedSel == sel_supportedEntityNames then pure (maybe 0 (const 1) (_supportedEntityNames rec_))
    else if queriedSel == sel_shouldApplyRecord_withRecordIdentifier then pure (maybe 0 (const 1) (_shouldApplyRecord_withRecordIdentifier rec_))
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
newISyncFiltering :: ISyncFilteringOverrides -> IO RawId
newISyncFiltering overrides = do
  inst <- class_createInstance iSyncFilteringDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
