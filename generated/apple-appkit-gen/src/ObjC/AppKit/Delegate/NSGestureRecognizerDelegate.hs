{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSGestureRecognizerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSGestureRecognizerDelegate defaultNSGestureRecognizerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSGestureRecognizerDelegate
  ( NSGestureRecognizerDelegateOverrides(..)
  , defaultNSGestureRecognizerDelegateOverrides
  , newNSGestureRecognizerDelegate
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

-- | Overrides record for @\@protocol NSGestureRecognizerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSGestureRecognizerDelegateOverrides = NSGestureRecognizerDelegateOverrides
  { _gestureRecognizer_shouldAttemptToRecognizeWithEvent :: !(Maybe (RawId -> RawId -> IO Bool))
  , _gestureRecognizerShouldBegin :: !(Maybe (RawId -> IO Bool))
  , _gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer :: !(Maybe (RawId -> RawId -> IO Bool))
  , _gestureRecognizer_shouldRequireFailureOfGestureRecognizer :: !(Maybe (RawId -> RawId -> IO Bool))
  , _gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer :: !(Maybe (RawId -> RawId -> IO Bool))
  , _gestureRecognizer_shouldReceiveTouch :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSGestureRecognizerDelegateOverrides :: NSGestureRecognizerDelegateOverrides
defaultNSGestureRecognizerDelegateOverrides = NSGestureRecognizerDelegateOverrides
  { _gestureRecognizer_shouldAttemptToRecognizeWithEvent = Nothing
  , _gestureRecognizerShouldBegin = Nothing
  , _gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer = Nothing
  , _gestureRecognizer_shouldRequireFailureOfGestureRecognizer = Nothing
  , _gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer = Nothing
  , _gestureRecognizer_shouldReceiveTouch = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsGestureRecognizerDelegateDelegateClass #-}
nsGestureRecognizerDelegateDelegateClass :: Class
nsGestureRecognizerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSGestureRecognizerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_gestureRecognizer_shouldAttemptToRecognizeWithEvent = unSelector (mkSelector "gestureRecognizer:shouldAttemptToRecognizeWithEvent:")
      sel_gestureRecognizerShouldBegin = unSelector (mkSelector "gestureRecognizerShouldBegin:")
      sel_gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer = unSelector (mkSelector "gestureRecognizer:shouldRecognizeSimultaneouslyWithGestureRecognizer:")
      sel_gestureRecognizer_shouldRequireFailureOfGestureRecognizer = unSelector (mkSelector "gestureRecognizer:shouldRequireFailureOfGestureRecognizer:")
      sel_gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer = unSelector (mkSelector "gestureRecognizer:shouldBeRequiredToFailByGestureRecognizer:")
      sel_gestureRecognizer_shouldReceiveTouch = unSelector (mkSelector "gestureRecognizer:shouldReceiveTouch:")
  -- gestureRecognizer:shouldAttemptToRecognizeWithEvent:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizer_shouldAttemptToRecognizeWithEvent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizer:shouldAttemptToRecognizeWithEvent:" "B@:@@" stub_0

  -- gestureRecognizerShouldBegin:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizerShouldBegin rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizerShouldBegin:" "B@:@" stub_1

  -- gestureRecognizer:shouldRecognizeSimultaneouslyWithGestureRecognizer:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizer:shouldRecognizeSimultaneouslyWithGestureRecognizer:" "B@:@@" stub_2

  -- gestureRecognizer:shouldRequireFailureOfGestureRecognizer:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizer_shouldRequireFailureOfGestureRecognizer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizer:shouldRequireFailureOfGestureRecognizer:" "B@:@@" stub_3

  -- gestureRecognizer:shouldBeRequiredToFailByGestureRecognizer:
  stub_4 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizer:shouldBeRequiredToFailByGestureRecognizer:" "B@:@@" stub_4

  -- gestureRecognizer:shouldReceiveTouch:
  stub_5 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    case _gestureRecognizer_shouldReceiveTouch rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "gestureRecognizer:shouldReceiveTouch:" "B@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGestureRecognizerDelegateOverrides
    if queriedSel == sel_gestureRecognizer_shouldAttemptToRecognizeWithEvent then pure (maybe 0 (const 1) (_gestureRecognizer_shouldAttemptToRecognizeWithEvent rec_))
    else if queriedSel == sel_gestureRecognizerShouldBegin then pure (maybe 0 (const 1) (_gestureRecognizerShouldBegin rec_))
    else if queriedSel == sel_gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer then pure (maybe 0 (const 1) (_gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer rec_))
    else if queriedSel == sel_gestureRecognizer_shouldRequireFailureOfGestureRecognizer then pure (maybe 0 (const 1) (_gestureRecognizer_shouldRequireFailureOfGestureRecognizer rec_))
    else if queriedSel == sel_gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer then pure (maybe 0 (const 1) (_gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer rec_))
    else if queriedSel == sel_gestureRecognizer_shouldReceiveTouch then pure (maybe 0 (const 1) (_gestureRecognizer_shouldReceiveTouch rec_))
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
newNSGestureRecognizerDelegate :: NSGestureRecognizerDelegateOverrides -> IO RawId
newNSGestureRecognizerDelegate overrides = do
  inst <- class_createInstance nsGestureRecognizerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
