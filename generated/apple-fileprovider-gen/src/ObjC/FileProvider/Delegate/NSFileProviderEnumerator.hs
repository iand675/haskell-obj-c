{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderEnumerator@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderEnumerator defaultNSFileProviderEnumeratorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderEnumerator
  ( NSFileProviderEnumeratorOverrides(..)
  , defaultNSFileProviderEnumeratorOverrides
  , newNSFileProviderEnumerator
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

-- | Overrides record for @\@protocol NSFileProviderEnumerator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderEnumeratorOverrides = NSFileProviderEnumeratorOverrides
  { _invalidate :: !(Maybe (IO ()))
  , _enumerateItemsForObserver_startingAtPage :: !(Maybe (RawId -> RawId -> IO ()))
  , _enumerateChangesForObserver_fromSyncAnchor :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderEnumeratorOverrides :: NSFileProviderEnumeratorOverrides
defaultNSFileProviderEnumeratorOverrides = NSFileProviderEnumeratorOverrides
  { _invalidate = Nothing
  , _enumerateItemsForObserver_startingAtPage = Nothing
  , _enumerateChangesForObserver_fromSyncAnchor = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileProviderEnumeratorDelegateClass #-}
nsFileProviderEnumeratorDelegateClass :: Class
nsFileProviderEnumeratorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderEnumerator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidate = unSelector (mkSelector "invalidate")
      sel_enumerateItemsForObserver_startingAtPage = unSelector (mkSelector "enumerateItemsForObserver:startingAtPage:")
      sel_enumerateChangesForObserver_fromSyncAnchor = unSelector (mkSelector "enumerateChangesForObserver:fromSyncAnchor:")
  -- invalidate
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumeratorOverrides
    case _invalidate rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "invalidate" "v@:" stub_0

  -- enumerateItemsForObserver:startingAtPage:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumeratorOverrides
    case _enumerateItemsForObserver_startingAtPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "enumerateItemsForObserver:startingAtPage:" "v@:@@" stub_1

  -- enumerateChangesForObserver:fromSyncAnchor:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumeratorOverrides
    case _enumerateChangesForObserver_fromSyncAnchor rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "enumerateChangesForObserver:fromSyncAnchor:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderEnumeratorOverrides
    if queriedSel == sel_invalidate then pure (maybe 0 (const 1) (_invalidate rec_))
    else if queriedSel == sel_enumerateItemsForObserver_startingAtPage then pure (maybe 0 (const 1) (_enumerateItemsForObserver_startingAtPage rec_))
    else if queriedSel == sel_enumerateChangesForObserver_fromSyncAnchor then pure (maybe 0 (const 1) (_enumerateChangesForObserver_fromSyncAnchor rec_))
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
newNSFileProviderEnumerator :: NSFileProviderEnumeratorOverrides -> IO RawId
newNSFileProviderEnumerator overrides = do
  inst <- class_createInstance nsFileProviderEnumeratorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
