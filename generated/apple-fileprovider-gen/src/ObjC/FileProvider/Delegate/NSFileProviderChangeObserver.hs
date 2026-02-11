{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderChangeObserver@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderChangeObserver defaultNSFileProviderChangeObserverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderChangeObserver
  ( NSFileProviderChangeObserverOverrides(..)
  , defaultNSFileProviderChangeObserverOverrides
  , newNSFileProviderChangeObserver
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

-- | Overrides record for @\@protocol NSFileProviderChangeObserver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderChangeObserverOverrides = NSFileProviderChangeObserverOverrides
  { _didUpdateItems :: !(Maybe (RawId -> IO ()))
  , _didDeleteItemsWithIdentifiers :: !(Maybe (RawId -> IO ()))
  , _finishEnumeratingChangesUpToSyncAnchor_moreComing :: !(Maybe (RawId -> Bool -> IO ()))
  , _finishEnumeratingWithError :: !(Maybe (RawId -> IO ()))
  , _suggestedBatchSize :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderChangeObserverOverrides :: NSFileProviderChangeObserverOverrides
defaultNSFileProviderChangeObserverOverrides = NSFileProviderChangeObserverOverrides
  { _didUpdateItems = Nothing
  , _didDeleteItemsWithIdentifiers = Nothing
  , _finishEnumeratingChangesUpToSyncAnchor_moreComing = Nothing
  , _finishEnumeratingWithError = Nothing
  , _suggestedBatchSize = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileProviderChangeObserverDelegateClass #-}
nsFileProviderChangeObserverDelegateClass :: Class
nsFileProviderChangeObserverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderChangeObserver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_didUpdateItems = unSelector (mkSelector "didUpdateItems:")
      sel_didDeleteItemsWithIdentifiers = unSelector (mkSelector "didDeleteItemsWithIdentifiers:")
      sel_finishEnumeratingChangesUpToSyncAnchor_moreComing = unSelector (mkSelector "finishEnumeratingChangesUpToSyncAnchor:moreComing:")
      sel_finishEnumeratingWithError = unSelector (mkSelector "finishEnumeratingWithError:")
      sel_suggestedBatchSize = unSelector (mkSelector "suggestedBatchSize")
  -- didUpdateItems:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    case _didUpdateItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didUpdateItems:" "v@:@" stub_0

  -- didDeleteItemsWithIdentifiers:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    case _didDeleteItemsWithIdentifiers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didDeleteItemsWithIdentifiers:" "v@:@" stub_1

  -- finishEnumeratingChangesUpToSyncAnchor:moreComing:
  stub_2 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    case _finishEnumeratingChangesUpToSyncAnchor_moreComing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "finishEnumeratingChangesUpToSyncAnchor:moreComing:" "v@:@B" stub_2

  -- finishEnumeratingWithError:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    case _finishEnumeratingWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "finishEnumeratingWithError:" "v@:@" stub_3

  -- suggestedBatchSize
  stub_4 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    case _suggestedBatchSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "suggestedBatchSize" "q@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderChangeObserverOverrides
    if queriedSel == sel_didUpdateItems then pure (maybe 0 (const 1) (_didUpdateItems rec_))
    else if queriedSel == sel_didDeleteItemsWithIdentifiers then pure (maybe 0 (const 1) (_didDeleteItemsWithIdentifiers rec_))
    else if queriedSel == sel_finishEnumeratingChangesUpToSyncAnchor_moreComing then pure (maybe 0 (const 1) (_finishEnumeratingChangesUpToSyncAnchor_moreComing rec_))
    else if queriedSel == sel_finishEnumeratingWithError then pure (maybe 0 (const 1) (_finishEnumeratingWithError rec_))
    else if queriedSel == sel_suggestedBatchSize then pure (maybe 0 (const 1) (_suggestedBatchSize rec_))
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
newNSFileProviderChangeObserver :: NSFileProviderChangeObserverOverrides -> IO RawId
newNSFileProviderChangeObserver overrides = do
  inst <- class_createInstance nsFileProviderChangeObserverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
