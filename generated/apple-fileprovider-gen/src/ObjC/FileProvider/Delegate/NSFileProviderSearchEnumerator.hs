{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderSearchEnumerator@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderSearchEnumerator defaultNSFileProviderSearchEnumeratorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderSearchEnumerator
  ( NSFileProviderSearchEnumeratorOverrides(..)
  , defaultNSFileProviderSearchEnumeratorOverrides
  , newNSFileProviderSearchEnumerator
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

-- | Overrides record for @\@protocol NSFileProviderSearchEnumerator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderSearchEnumeratorOverrides = NSFileProviderSearchEnumeratorOverrides
  { _invalidate :: !(Maybe (IO ()))
  , _enumerateSearchResultsForObserver_startingAtPage :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderSearchEnumeratorOverrides :: NSFileProviderSearchEnumeratorOverrides
defaultNSFileProviderSearchEnumeratorOverrides = NSFileProviderSearchEnumeratorOverrides
  { _invalidate = Nothing
  , _enumerateSearchResultsForObserver_startingAtPage = Nothing
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
{-# NOINLINE nsFileProviderSearchEnumeratorDelegateClass #-}
nsFileProviderSearchEnumeratorDelegateClass :: Class
nsFileProviderSearchEnumeratorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderSearchEnumerator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidate = unSelector (mkSelector "invalidate")
      sel_enumerateSearchResultsForObserver_startingAtPage = unSelector (mkSelector "enumerateSearchResultsForObserver:startingAtPage:")
  -- invalidate
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchEnumeratorOverrides
    case _invalidate rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "invalidate" "v@:" stub_0

  -- enumerateSearchResultsForObserver:startingAtPage:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchEnumeratorOverrides
    case _enumerateSearchResultsForObserver_startingAtPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "enumerateSearchResultsForObserver:startingAtPage:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchEnumeratorOverrides
    if queriedSel == sel_invalidate then pure (maybe 0 (const 1) (_invalidate rec_))
    else if queriedSel == sel_enumerateSearchResultsForObserver_startingAtPage then pure (maybe 0 (const 1) (_enumerateSearchResultsForObserver_startingAtPage rec_))
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
newNSFileProviderSearchEnumerator :: NSFileProviderSearchEnumeratorOverrides -> IO RawId
newNSFileProviderSearchEnumerator overrides = do
  inst <- class_createInstance nsFileProviderSearchEnumeratorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
