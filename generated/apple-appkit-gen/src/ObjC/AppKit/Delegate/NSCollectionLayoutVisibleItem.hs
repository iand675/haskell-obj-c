{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionLayoutVisibleItem@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionLayoutVisibleItem defaultNSCollectionLayoutVisibleItemOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionLayoutVisibleItem
  ( NSCollectionLayoutVisibleItemOverrides(..)
  , defaultNSCollectionLayoutVisibleItemOverrides
  , newNSCollectionLayoutVisibleItem
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

-- | Overrides record for @\@protocol NSCollectionLayoutVisibleItem@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionLayoutVisibleItemOverrides = NSCollectionLayoutVisibleItemOverrides
  { _alpha :: !(Maybe (IO Double))
  , _setAlpha :: !(Maybe (Double -> IO ()))
  , _zIndex :: !(Maybe (IO Int))
  , _setZIndex :: !(Maybe (Int -> IO ()))
  , _hidden :: !(Maybe (IO Bool))
  , _setHidden :: !(Maybe (Bool -> IO ()))
  , _name :: !(Maybe (IO RawId))
  , _indexPath :: !(Maybe (IO RawId))
  , _representedElementKind :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionLayoutVisibleItemOverrides :: NSCollectionLayoutVisibleItemOverrides
defaultNSCollectionLayoutVisibleItemOverrides = NSCollectionLayoutVisibleItemOverrides
  { _alpha = Nothing
  , _setAlpha = Nothing
  , _zIndex = Nothing
  , _setZIndex = Nothing
  , _hidden = Nothing
  , _setHidden = Nothing
  , _name = Nothing
  , _indexPath = Nothing
  , _representedElementKind = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionLayoutVisibleItemDelegateClass #-}
nsCollectionLayoutVisibleItemDelegateClass :: Class
nsCollectionLayoutVisibleItemDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionLayoutVisibleItem" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_alpha = unSelector (mkSelector "alpha")
      sel_setAlpha = unSelector (mkSelector "setAlpha:")
      sel_zIndex = unSelector (mkSelector "zIndex")
      sel_setZIndex = unSelector (mkSelector "setZIndex:")
      sel_hidden = unSelector (mkSelector "hidden")
      sel_setHidden = unSelector (mkSelector "setHidden:")
      sel_name = unSelector (mkSelector "name")
      sel_indexPath = unSelector (mkSelector "indexPath")
      sel_representedElementKind = unSelector (mkSelector "representedElementKind")
  -- alpha
  stub_0 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _alpha rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "alpha" "d@:" stub_0

  -- setAlpha:
  stub_1 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _setAlpha rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setAlpha:" "v@:d" stub_1

  -- zIndex
  stub_2 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _zIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "zIndex" "q@:" stub_2

  -- setZIndex:
  stub_3 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _setZIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setZIndex:" "v@:q" stub_3

  -- hidden
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _hidden rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hidden" "B@:" stub_4

  -- setHidden:
  stub_5 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _setHidden rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setHidden:" "v@:B" stub_5

  -- name
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_6

  -- indexPath
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _indexPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "indexPath" "@@:" stub_7

  -- representedElementKind
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    case _representedElementKind rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "representedElementKind" "@@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutVisibleItemOverrides
    if queriedSel == sel_alpha then pure (maybe 0 (const 1) (_alpha rec_))
    else if queriedSel == sel_setAlpha then pure (maybe 0 (const 1) (_setAlpha rec_))
    else if queriedSel == sel_zIndex then pure (maybe 0 (const 1) (_zIndex rec_))
    else if queriedSel == sel_setZIndex then pure (maybe 0 (const 1) (_setZIndex rec_))
    else if queriedSel == sel_hidden then pure (maybe 0 (const 1) (_hidden rec_))
    else if queriedSel == sel_setHidden then pure (maybe 0 (const 1) (_setHidden rec_))
    else if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_indexPath then pure (maybe 0 (const 1) (_indexPath rec_))
    else if queriedSel == sel_representedElementKind then pure (maybe 0 (const 1) (_representedElementKind rec_))
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
newNSCollectionLayoutVisibleItem :: NSCollectionLayoutVisibleItemOverrides -> IO RawId
newNSCollectionLayoutVisibleItem overrides = do
  inst <- class_createInstance nsCollectionLayoutVisibleItemDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
