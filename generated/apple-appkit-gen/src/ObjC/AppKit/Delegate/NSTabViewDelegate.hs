{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTabViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTabViewDelegate defaultNSTabViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTabViewDelegate
  ( NSTabViewDelegateOverrides(..)
  , defaultNSTabViewDelegateOverrides
  , newNSTabViewDelegate
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

-- | Overrides record for @\@protocol NSTabViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTabViewDelegateOverrides = NSTabViewDelegateOverrides
  { _tabView_shouldSelectTabViewItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _tabView_willSelectTabViewItem :: !(Maybe (RawId -> RawId -> IO ()))
  , _tabView_didSelectTabViewItem :: !(Maybe (RawId -> RawId -> IO ()))
  , _tabViewDidChangeNumberOfTabViewItems :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTabViewDelegateOverrides :: NSTabViewDelegateOverrides
defaultNSTabViewDelegateOverrides = NSTabViewDelegateOverrides
  { _tabView_shouldSelectTabViewItem = Nothing
  , _tabView_willSelectTabViewItem = Nothing
  , _tabView_didSelectTabViewItem = Nothing
  , _tabViewDidChangeNumberOfTabViewItems = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTabViewDelegateDelegateClass #-}
nsTabViewDelegateDelegateClass :: Class
nsTabViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTabViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tabView_shouldSelectTabViewItem = unSelector (mkSelector "tabView:shouldSelectTabViewItem:")
      sel_tabView_willSelectTabViewItem = unSelector (mkSelector "tabView:willSelectTabViewItem:")
      sel_tabView_didSelectTabViewItem = unSelector (mkSelector "tabView:didSelectTabViewItem:")
      sel_tabViewDidChangeNumberOfTabViewItems = unSelector (mkSelector "tabViewDidChangeNumberOfTabViewItems:")
  -- tabView:shouldSelectTabViewItem:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTabViewDelegateOverrides
    case _tabView_shouldSelectTabViewItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tabView:shouldSelectTabViewItem:" "B@:@@" stub_0

  -- tabView:willSelectTabViewItem:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTabViewDelegateOverrides
    case _tabView_willSelectTabViewItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tabView:willSelectTabViewItem:" "v@:@@" stub_1

  -- tabView:didSelectTabViewItem:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTabViewDelegateOverrides
    case _tabView_didSelectTabViewItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tabView:didSelectTabViewItem:" "v@:@@" stub_2

  -- tabViewDidChangeNumberOfTabViewItems:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTabViewDelegateOverrides
    case _tabViewDidChangeNumberOfTabViewItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "tabViewDidChangeNumberOfTabViewItems:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTabViewDelegateOverrides
    if queriedSel == sel_tabView_shouldSelectTabViewItem then pure (maybe 0 (const 1) (_tabView_shouldSelectTabViewItem rec_))
    else if queriedSel == sel_tabView_willSelectTabViewItem then pure (maybe 0 (const 1) (_tabView_willSelectTabViewItem rec_))
    else if queriedSel == sel_tabView_didSelectTabViewItem then pure (maybe 0 (const 1) (_tabView_didSelectTabViewItem rec_))
    else if queriedSel == sel_tabViewDidChangeNumberOfTabViewItems then pure (maybe 0 (const 1) (_tabViewDidChangeNumberOfTabViewItems rec_))
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
newNSTabViewDelegate :: NSTabViewDelegateOverrides -> IO RawId
newNSTabViewDelegate overrides = do
  inst <- class_createInstance nsTabViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
