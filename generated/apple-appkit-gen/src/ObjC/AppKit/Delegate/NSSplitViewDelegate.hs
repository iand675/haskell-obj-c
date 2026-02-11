{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSplitViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSplitViewDelegate defaultNSSplitViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSplitViewDelegate
  ( NSSplitViewDelegateOverrides(..)
  , defaultNSSplitViewDelegateOverrides
  , newNSSplitViewDelegate
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

-- | Overrides record for @\@protocol NSSplitViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSplitViewDelegateOverrides = NSSplitViewDelegateOverrides
  { _splitView_canCollapseSubview :: !(Maybe (RawId -> RawId -> IO Bool))
  , _splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _splitView_constrainMinCoordinate_ofSubviewAt :: !(Maybe (RawId -> Double -> Int -> IO Double))
  , _splitView_constrainMaxCoordinate_ofSubviewAt :: !(Maybe (RawId -> Double -> Int -> IO Double))
  , _splitView_constrainSplitPosition_ofSubviewAt :: !(Maybe (RawId -> Double -> Int -> IO Double))
  , _splitView_shouldAdjustSizeOfSubview :: !(Maybe (RawId -> RawId -> IO Bool))
  , _splitView_shouldHideDividerAtIndex :: !(Maybe (RawId -> Int -> IO Bool))
  , _splitViewWillResizeSubviews :: !(Maybe (RawId -> IO ()))
  , _splitViewDidResizeSubviews :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSplitViewDelegateOverrides :: NSSplitViewDelegateOverrides
defaultNSSplitViewDelegateOverrides = NSSplitViewDelegateOverrides
  { _splitView_canCollapseSubview = Nothing
  , _splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex = Nothing
  , _splitView_constrainMinCoordinate_ofSubviewAt = Nothing
  , _splitView_constrainMaxCoordinate_ofSubviewAt = Nothing
  , _splitView_constrainSplitPosition_ofSubviewAt = Nothing
  , _splitView_shouldAdjustSizeOfSubview = Nothing
  , _splitView_shouldHideDividerAtIndex = Nothing
  , _splitViewWillResizeSubviews = Nothing
  , _splitViewDidResizeSubviews = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_d_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> CLong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSplitViewDelegateDelegateClass #-}
nsSplitViewDelegateDelegateClass :: Class
nsSplitViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSplitViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_splitView_canCollapseSubview = unSelector (mkSelector "splitView:canCollapseSubview:")
      sel_splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex = unSelector (mkSelector "splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:")
      sel_splitView_constrainMinCoordinate_ofSubviewAt = unSelector (mkSelector "splitView:constrainMinCoordinate:ofSubviewAt:")
      sel_splitView_constrainMaxCoordinate_ofSubviewAt = unSelector (mkSelector "splitView:constrainMaxCoordinate:ofSubviewAt:")
      sel_splitView_constrainSplitPosition_ofSubviewAt = unSelector (mkSelector "splitView:constrainSplitPosition:ofSubviewAt:")
      sel_splitView_shouldAdjustSizeOfSubview = unSelector (mkSelector "splitView:shouldAdjustSizeOfSubview:")
      sel_splitView_shouldHideDividerAtIndex = unSelector (mkSelector "splitView:shouldHideDividerAtIndex:")
      sel_splitViewWillResizeSubviews = unSelector (mkSelector "splitViewWillResizeSubviews:")
      sel_splitViewDidResizeSubviews = unSelector (mkSelector "splitViewDidResizeSubviews:")
  -- splitView:canCollapseSubview:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_canCollapseSubview rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "splitView:canCollapseSubview:" "B@:@@" stub_0

  -- splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:
  stub_1 <- wrap_at_at_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:" "B@:@@q" stub_1

  -- splitView:constrainMinCoordinate:ofSubviewAt:
  stub_2 <- wrap_at_d_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_constrainMinCoordinate_ofSubviewAt rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "splitView:constrainMinCoordinate:ofSubviewAt:" "d@:@dq" stub_2

  -- splitView:constrainMaxCoordinate:ofSubviewAt:
  stub_3 <- wrap_at_d_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_constrainMaxCoordinate_ofSubviewAt rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "splitView:constrainMaxCoordinate:ofSubviewAt:" "d@:@dq" stub_3

  -- splitView:constrainSplitPosition:ofSubviewAt:
  stub_4 <- wrap_at_d_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_constrainSplitPosition_ofSubviewAt rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "splitView:constrainSplitPosition:ofSubviewAt:" "d@:@dq" stub_4

  -- splitView:shouldAdjustSizeOfSubview:
  stub_5 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_shouldAdjustSizeOfSubview rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "splitView:shouldAdjustSizeOfSubview:" "B@:@@" stub_5

  -- splitView:shouldHideDividerAtIndex:
  stub_6 <- wrap_at_q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitView_shouldHideDividerAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "splitView:shouldHideDividerAtIndex:" "B@:@q" stub_6

  -- splitViewWillResizeSubviews:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitViewWillResizeSubviews rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "splitViewWillResizeSubviews:" "v@:@" stub_7

  -- splitViewDidResizeSubviews:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    case _splitViewDidResizeSubviews rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "splitViewDidResizeSubviews:" "v@:@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSplitViewDelegateOverrides
    if queriedSel == sel_splitView_canCollapseSubview then pure (maybe 0 (const 1) (_splitView_canCollapseSubview rec_))
    else if queriedSel == sel_splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex then pure (maybe 0 (const 1) (_splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex rec_))
    else if queriedSel == sel_splitView_constrainMinCoordinate_ofSubviewAt then pure (maybe 0 (const 1) (_splitView_constrainMinCoordinate_ofSubviewAt rec_))
    else if queriedSel == sel_splitView_constrainMaxCoordinate_ofSubviewAt then pure (maybe 0 (const 1) (_splitView_constrainMaxCoordinate_ofSubviewAt rec_))
    else if queriedSel == sel_splitView_constrainSplitPosition_ofSubviewAt then pure (maybe 0 (const 1) (_splitView_constrainSplitPosition_ofSubviewAt rec_))
    else if queriedSel == sel_splitView_shouldAdjustSizeOfSubview then pure (maybe 0 (const 1) (_splitView_shouldAdjustSizeOfSubview rec_))
    else if queriedSel == sel_splitView_shouldHideDividerAtIndex then pure (maybe 0 (const 1) (_splitView_shouldHideDividerAtIndex rec_))
    else if queriedSel == sel_splitViewWillResizeSubviews then pure (maybe 0 (const 1) (_splitViewWillResizeSubviews rec_))
    else if queriedSel == sel_splitViewDidResizeSubviews then pure (maybe 0 (const 1) (_splitViewDidResizeSubviews rec_))
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
newNSSplitViewDelegate :: NSSplitViewDelegateOverrides -> IO RawId
newNSSplitViewDelegate overrides = do
  inst <- class_createInstance nsSplitViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
