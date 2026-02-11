{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewPrefetching@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewPrefetching defaultNSCollectionViewPrefetchingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewPrefetching
  ( NSCollectionViewPrefetchingOverrides(..)
  , defaultNSCollectionViewPrefetchingOverrides
  , newNSCollectionViewPrefetching
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

-- | Overrides record for @\@protocol NSCollectionViewPrefetching@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewPrefetchingOverrides = NSCollectionViewPrefetchingOverrides
  { _collectionView_prefetchItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO ()))
  , _collectionView_cancelPrefetchingForItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewPrefetchingOverrides :: NSCollectionViewPrefetchingOverrides
defaultNSCollectionViewPrefetchingOverrides = NSCollectionViewPrefetchingOverrides
  { _collectionView_prefetchItemsAtIndexPaths = Nothing
  , _collectionView_cancelPrefetchingForItemsAtIndexPaths = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewPrefetchingDelegateClass #-}
nsCollectionViewPrefetchingDelegateClass :: Class
nsCollectionViewPrefetchingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewPrefetching" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collectionView_prefetchItemsAtIndexPaths = unSelector (mkSelector "collectionView:prefetchItemsAtIndexPaths:")
      sel_collectionView_cancelPrefetchingForItemsAtIndexPaths = unSelector (mkSelector "collectionView:cancelPrefetchingForItemsAtIndexPaths:")
  -- collectionView:prefetchItemsAtIndexPaths:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewPrefetchingOverrides
    case _collectionView_prefetchItemsAtIndexPaths rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collectionView:prefetchItemsAtIndexPaths:" "v@:@@" stub_0

  -- collectionView:cancelPrefetchingForItemsAtIndexPaths:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewPrefetchingOverrides
    case _collectionView_cancelPrefetchingForItemsAtIndexPaths rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collectionView:cancelPrefetchingForItemsAtIndexPaths:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewPrefetchingOverrides
    if queriedSel == sel_collectionView_prefetchItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_prefetchItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_cancelPrefetchingForItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_cancelPrefetchingForItemsAtIndexPaths rec_))
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
newNSCollectionViewPrefetching :: NSCollectionViewPrefetchingOverrides -> IO RawId
newNSCollectionViewPrefetching overrides = do
  inst <- class_createInstance nsCollectionViewPrefetchingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
