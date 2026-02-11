{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NCWidgetListViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNCWidgetListViewDelegate defaultNCWidgetListViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NotificationCenter.Delegate.NCWidgetListViewDelegate
  ( NCWidgetListViewDelegateOverrides(..)
  , defaultNCWidgetListViewDelegateOverrides
  , newNCWidgetListViewDelegate
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

-- | Overrides record for @\@protocol NCWidgetListViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NCWidgetListViewDelegateOverrides = NCWidgetListViewDelegateOverrides
  { _widgetList_viewControllerForRow :: !(Maybe (RawId -> Int -> IO RawId))
  , _widgetListPerformAddAction :: !(Maybe (RawId -> IO ()))
  , _widgetList_shouldReorderRow :: !(Maybe (RawId -> Int -> IO Bool))
  , _widgetList_didReorderRow_toRow :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _widgetList_shouldRemoveRow :: !(Maybe (RawId -> Int -> IO Bool))
  , _widgetList_didRemoveRow :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNCWidgetListViewDelegateOverrides :: NCWidgetListViewDelegateOverrides
defaultNCWidgetListViewDelegateOverrides = NCWidgetListViewDelegateOverrides
  { _widgetList_viewControllerForRow = Nothing
  , _widgetListPerformAddAction = Nothing
  , _widgetList_shouldReorderRow = Nothing
  , _widgetList_didReorderRow_toRow = Nothing
  , _widgetList_shouldRemoveRow = Nothing
  , _widgetList_didRemoveRow = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ncWidgetListViewDelegateDelegateClass #-}
ncWidgetListViewDelegateDelegateClass :: Class
ncWidgetListViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNCWidgetListViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_widgetList_viewControllerForRow = unSelector (mkSelector "widgetList:viewControllerForRow:")
      sel_widgetListPerformAddAction = unSelector (mkSelector "widgetListPerformAddAction:")
      sel_widgetList_shouldReorderRow = unSelector (mkSelector "widgetList:shouldReorderRow:")
      sel_widgetList_didReorderRow_toRow = unSelector (mkSelector "widgetList:didReorderRow:toRow:")
      sel_widgetList_shouldRemoveRow = unSelector (mkSelector "widgetList:shouldRemoveRow:")
      sel_widgetList_didRemoveRow = unSelector (mkSelector "widgetList:didRemoveRow:")
  -- widgetList:viewControllerForRow:
  stub_0 <- wrap_at_Q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetList_viewControllerForRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "widgetList:viewControllerForRow:" "@@:@Q" stub_0

  -- widgetListPerformAddAction:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetListPerformAddAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "widgetListPerformAddAction:" "v@:@" stub_1

  -- widgetList:shouldReorderRow:
  stub_2 <- wrap_at_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetList_shouldReorderRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "widgetList:shouldReorderRow:" "B@:@Q" stub_2

  -- widgetList:didReorderRow:toRow:
  stub_3 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetList_didReorderRow_toRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "widgetList:didReorderRow:toRow:" "v@:@QQ" stub_3

  -- widgetList:shouldRemoveRow:
  stub_4 <- wrap_at_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetList_shouldRemoveRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "widgetList:shouldRemoveRow:" "B@:@Q" stub_4

  -- widgetList:didRemoveRow:
  stub_5 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    case _widgetList_didRemoveRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "widgetList:didRemoveRow:" "v@:@Q" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetListViewDelegateOverrides
    if queriedSel == sel_widgetList_viewControllerForRow then pure (maybe 0 (const 1) (_widgetList_viewControllerForRow rec_))
    else if queriedSel == sel_widgetListPerformAddAction then pure (maybe 0 (const 1) (_widgetListPerformAddAction rec_))
    else if queriedSel == sel_widgetList_shouldReorderRow then pure (maybe 0 (const 1) (_widgetList_shouldReorderRow rec_))
    else if queriedSel == sel_widgetList_didReorderRow_toRow then pure (maybe 0 (const 1) (_widgetList_didReorderRow_toRow rec_))
    else if queriedSel == sel_widgetList_shouldRemoveRow then pure (maybe 0 (const 1) (_widgetList_shouldRemoveRow rec_))
    else if queriedSel == sel_widgetList_didRemoveRow then pure (maybe 0 (const 1) (_widgetList_didRemoveRow rec_))
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
newNCWidgetListViewDelegate :: NCWidgetListViewDelegateOverrides -> IO RawId
newNCWidgetListViewDelegate overrides = do
  inst <- class_createInstance ncWidgetListViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
