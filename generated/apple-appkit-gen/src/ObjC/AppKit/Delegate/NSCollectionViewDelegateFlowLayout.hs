{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewDelegateFlowLayout@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewDelegateFlowLayout defaultNSCollectionViewDelegateFlowLayoutOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewDelegateFlowLayout
  ( NSCollectionViewDelegateFlowLayoutOverrides(..)
  , defaultNSCollectionViewDelegateFlowLayoutOverrides
  , newNSCollectionViewDelegateFlowLayout
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

-- | Overrides record for @\@protocol NSCollectionViewDelegateFlowLayout@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewDelegateFlowLayoutOverrides = NSCollectionViewDelegateFlowLayoutOverrides
  { _collectionView_layout_minimumLineSpacingForSectionAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO Double))
  , _collectionView_layout_minimumInteritemSpacingForSectionAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO Double))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewDelegateFlowLayoutOverrides :: NSCollectionViewDelegateFlowLayoutOverrides
defaultNSCollectionViewDelegateFlowLayoutOverrides = NSCollectionViewDelegateFlowLayoutOverrides
  { _collectionView_layout_minimumLineSpacingForSectionAtIndex = Nothing
  , _collectionView_layout_minimumInteritemSpacingForSectionAtIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CDouble))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewDelegateFlowLayoutDelegateClass #-}
nsCollectionViewDelegateFlowLayoutDelegateClass :: Class
nsCollectionViewDelegateFlowLayoutDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewDelegateFlowLayout" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collectionView_layout_minimumLineSpacingForSectionAtIndex = unSelector (mkSelector "collectionView:layout:minimumLineSpacingForSectionAtIndex:")
      sel_collectionView_layout_minimumInteritemSpacingForSectionAtIndex = unSelector (mkSelector "collectionView:layout:minimumInteritemSpacingForSectionAtIndex:")
  -- collectionView:layout:minimumLineSpacingForSectionAtIndex:
  stub_0 <- wrap_at_at_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateFlowLayoutOverrides
    case _collectionView_layout_minimumLineSpacingForSectionAtIndex rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "collectionView:layout:minimumLineSpacingForSectionAtIndex:" "d@:@@q" stub_0

  -- collectionView:layout:minimumInteritemSpacingForSectionAtIndex:
  stub_1 <- wrap_at_at_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateFlowLayoutOverrides
    case _collectionView_layout_minimumInteritemSpacingForSectionAtIndex rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "collectionView:layout:minimumInteritemSpacingForSectionAtIndex:" "d@:@@q" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateFlowLayoutOverrides
    if queriedSel == sel_collectionView_layout_minimumLineSpacingForSectionAtIndex then pure (maybe 0 (const 1) (_collectionView_layout_minimumLineSpacingForSectionAtIndex rec_))
    else if queriedSel == sel_collectionView_layout_minimumInteritemSpacingForSectionAtIndex then pure (maybe 0 (const 1) (_collectionView_layout_minimumInteritemSpacingForSectionAtIndex rec_))
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
newNSCollectionViewDelegateFlowLayout :: NSCollectionViewDelegateFlowLayoutOverrides -> IO RawId
newNSCollectionViewDelegateFlowLayout overrides = do
  inst <- class_createInstance nsCollectionViewDelegateFlowLayoutDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
