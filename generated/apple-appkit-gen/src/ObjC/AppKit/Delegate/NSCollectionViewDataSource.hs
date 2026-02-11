{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewDataSource defaultNSCollectionViewDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewDataSource
  ( NSCollectionViewDataSourceOverrides(..)
  , defaultNSCollectionViewDataSourceOverrides
  , newNSCollectionViewDataSource
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

-- | Overrides record for @\@protocol NSCollectionViewDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewDataSourceOverrides = NSCollectionViewDataSourceOverrides
  { _collectionView_numberOfItemsInSection :: !(Maybe (RawId -> Int -> IO Int))
  , _collectionView_itemForRepresentedObjectAtIndexPath :: !(Maybe (RawId -> RawId -> IO RawId))
  , _numberOfSectionsInCollectionView :: !(Maybe (RawId -> IO Int))
  , _collectionView_viewForSupplementaryElementOfKind_atIndexPath :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewDataSourceOverrides :: NSCollectionViewDataSourceOverrides
defaultNSCollectionViewDataSourceOverrides = NSCollectionViewDataSourceOverrides
  { _collectionView_numberOfItemsInSection = Nothing
  , _collectionView_itemForRepresentedObjectAtIndexPath = Nothing
  , _numberOfSectionsInCollectionView = Nothing
  , _collectionView_viewForSupplementaryElementOfKind_atIndexPath = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewDataSourceDelegateClass #-}
nsCollectionViewDataSourceDelegateClass :: Class
nsCollectionViewDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collectionView_numberOfItemsInSection = unSelector (mkSelector "collectionView:numberOfItemsInSection:")
      sel_collectionView_itemForRepresentedObjectAtIndexPath = unSelector (mkSelector "collectionView:itemForRepresentedObjectAtIndexPath:")
      sel_numberOfSectionsInCollectionView = unSelector (mkSelector "numberOfSectionsInCollectionView:")
      sel_collectionView_viewForSupplementaryElementOfKind_atIndexPath = unSelector (mkSelector "collectionView:viewForSupplementaryElementOfKind:atIndexPath:")
  -- collectionView:numberOfItemsInSection:
  stub_0 <- wrap_at_q_q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDataSourceOverrides
    case _collectionView_numberOfItemsInSection rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (fromIntegral r)
  addObjCMethod cls "collectionView:numberOfItemsInSection:" "q@:@q" stub_0

  -- collectionView:itemForRepresentedObjectAtIndexPath:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDataSourceOverrides
    case _collectionView_itemForRepresentedObjectAtIndexPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:itemForRepresentedObjectAtIndexPath:" "@@:@@" stub_1

  -- numberOfSectionsInCollectionView:
  stub_2 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDataSourceOverrides
    case _numberOfSectionsInCollectionView rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfSectionsInCollectionView:" "q@:@" stub_2

  -- collectionView:viewForSupplementaryElementOfKind:atIndexPath:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDataSourceOverrides
    case _collectionView_viewForSupplementaryElementOfKind_atIndexPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:viewForSupplementaryElementOfKind:atIndexPath:" "@@:@@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDataSourceOverrides
    if queriedSel == sel_collectionView_numberOfItemsInSection then pure (maybe 0 (const 1) (_collectionView_numberOfItemsInSection rec_))
    else if queriedSel == sel_collectionView_itemForRepresentedObjectAtIndexPath then pure (maybe 0 (const 1) (_collectionView_itemForRepresentedObjectAtIndexPath rec_))
    else if queriedSel == sel_numberOfSectionsInCollectionView then pure (maybe 0 (const 1) (_numberOfSectionsInCollectionView rec_))
    else if queriedSel == sel_collectionView_viewForSupplementaryElementOfKind_atIndexPath then pure (maybe 0 (const 1) (_collectionView_viewForSupplementaryElementOfKind_atIndexPath rec_))
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
newNSCollectionViewDataSource :: NSCollectionViewDataSourceOverrides -> IO RawId
newNSCollectionViewDataSource overrides = do
  inst <- class_createInstance nsCollectionViewDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
