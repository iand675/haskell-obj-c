{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAnimationDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSAnimationDelegate defaultNSAnimationDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAnimationDelegate
  ( NSAnimationDelegateOverrides(..)
  , defaultNSAnimationDelegateOverrides
  , newNSAnimationDelegate
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

-- | Overrides record for @\@protocol NSAnimationDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAnimationDelegateOverrides = NSAnimationDelegateOverrides
  { _animationShouldStart :: !(Maybe (RawId -> IO Bool))
  , _animationDidStop :: !(Maybe (RawId -> IO ()))
  , _animationDidEnd :: !(Maybe (RawId -> IO ()))
  , _animation_valueForProgress :: !(Maybe (RawId -> Float -> IO Float))
  , _animation_didReachProgressMark :: !(Maybe (RawId -> Float -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAnimationDelegateOverrides :: NSAnimationDelegateOverrides
defaultNSAnimationDelegateOverrides = NSAnimationDelegateOverrides
  { _animationShouldStart = Nothing
  , _animationDidStop = Nothing
  , _animationDidEnd = Nothing
  , _animation_valueForProgress = Nothing
  , _animation_didReachProgressMark = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_at_f_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CFloat -> IO CFloat))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAnimationDelegateDelegateClass #-}
nsAnimationDelegateDelegateClass :: Class
nsAnimationDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAnimationDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_animationShouldStart = unSelector (mkSelector "animationShouldStart:")
      sel_animationDidStop = unSelector (mkSelector "animationDidStop:")
      sel_animationDidEnd = unSelector (mkSelector "animationDidEnd:")
      sel_animation_valueForProgress = unSelector (mkSelector "animation:valueForProgress:")
      sel_animation_didReachProgressMark = unSelector (mkSelector "animation:didReachProgressMark:")
  -- animationShouldStart:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    case _animationShouldStart rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "animationShouldStart:" "B@:@" stub_0

  -- animationDidStop:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    case _animationDidStop rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "animationDidStop:" "v@:@" stub_1

  -- animationDidEnd:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    case _animationDidEnd rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "animationDidEnd:" "v@:@" stub_2

  -- animation:valueForProgress:
  stub_3 <- wrap_at_f_f $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    case _animation_valueForProgress rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1)
        pure (realToFrac r)
  addObjCMethod cls "animation:valueForProgress:" "f@:@f" stub_3

  -- animation:didReachProgressMark:
  stub_4 <- wrap_at_f_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    case _animation_didReachProgressMark rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "animation:didReachProgressMark:" "v@:@f" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimationDelegateOverrides
    if queriedSel == sel_animationShouldStart then pure (maybe 0 (const 1) (_animationShouldStart rec_))
    else if queriedSel == sel_animationDidStop then pure (maybe 0 (const 1) (_animationDidStop rec_))
    else if queriedSel == sel_animationDidEnd then pure (maybe 0 (const 1) (_animationDidEnd rec_))
    else if queriedSel == sel_animation_valueForProgress then pure (maybe 0 (const 1) (_animation_valueForProgress rec_))
    else if queriedSel == sel_animation_didReachProgressMark then pure (maybe 0 (const 1) (_animation_didReachProgressMark rec_))
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
newNSAnimationDelegate :: NSAnimationDelegateOverrides -> IO RawId
newNSAnimationDelegate overrides = do
  inst <- class_createInstance nsAnimationDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
