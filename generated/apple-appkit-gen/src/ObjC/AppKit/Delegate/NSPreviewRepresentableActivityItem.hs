{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPreviewRepresentableActivityItem@.
--
-- Usage:
--
-- @
-- delegate <- newNSPreviewRepresentableActivityItem defaultNSPreviewRepresentableActivityItemOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPreviewRepresentableActivityItem
  ( NSPreviewRepresentableActivityItemOverrides(..)
  , defaultNSPreviewRepresentableActivityItemOverrides
  , newNSPreviewRepresentableActivityItem
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

-- | Overrides record for @\@protocol NSPreviewRepresentableActivityItem@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPreviewRepresentableActivityItemOverrides = NSPreviewRepresentableActivityItemOverrides
  { _item :: !(Maybe (IO RawId))
  , _title :: !(Maybe (IO RawId))
  , _imageProvider :: !(Maybe (IO RawId))
  , _iconProvider :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPreviewRepresentableActivityItemOverrides :: NSPreviewRepresentableActivityItemOverrides
defaultNSPreviewRepresentableActivityItemOverrides = NSPreviewRepresentableActivityItemOverrides
  { _item = Nothing
  , _title = Nothing
  , _imageProvider = Nothing
  , _iconProvider = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPreviewRepresentableActivityItemDelegateClass #-}
nsPreviewRepresentableActivityItemDelegateClass :: Class
nsPreviewRepresentableActivityItemDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPreviewRepresentableActivityItem" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_item = unSelector (mkSelector "item")
      sel_title = unSelector (mkSelector "title")
      sel_imageProvider = unSelector (mkSelector "imageProvider")
      sel_iconProvider = unSelector (mkSelector "iconProvider")
  -- item
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPreviewRepresentableActivityItemOverrides
    case _item rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "item" "@@:" stub_0

  -- title
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPreviewRepresentableActivityItemOverrides
    case _title rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "title" "@@:" stub_1

  -- imageProvider
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPreviewRepresentableActivityItemOverrides
    case _imageProvider rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "imageProvider" "@@:" stub_2

  -- iconProvider
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPreviewRepresentableActivityItemOverrides
    case _iconProvider rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "iconProvider" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPreviewRepresentableActivityItemOverrides
    if queriedSel == sel_item then pure (maybe 0 (const 1) (_item rec_))
    else if queriedSel == sel_title then pure (maybe 0 (const 1) (_title rec_))
    else if queriedSel == sel_imageProvider then pure (maybe 0 (const 1) (_imageProvider rec_))
    else if queriedSel == sel_iconProvider then pure (maybe 0 (const 1) (_iconProvider rec_))
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
newNSPreviewRepresentableActivityItem :: NSPreviewRepresentableActivityItemOverrides -> IO RawId
newNSPreviewRepresentableActivityItem overrides = do
  inst <- class_createInstance nsPreviewRepresentableActivityItemDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
