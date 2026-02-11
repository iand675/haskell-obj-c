{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSViewControllerPresentationAnimator@.
--
-- Usage:
--
-- @
-- delegate <- newNSViewControllerPresentationAnimator defaultNSViewControllerPresentationAnimatorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSViewControllerPresentationAnimator
  ( NSViewControllerPresentationAnimatorOverrides(..)
  , defaultNSViewControllerPresentationAnimatorOverrides
  , newNSViewControllerPresentationAnimator
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

-- | Overrides record for @\@protocol NSViewControllerPresentationAnimator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSViewControllerPresentationAnimatorOverrides = NSViewControllerPresentationAnimatorOverrides
  { _animatePresentationOfViewController_fromViewController :: !(Maybe (RawId -> RawId -> IO ()))
  , _animateDismissalOfViewController_fromViewController :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSViewControllerPresentationAnimatorOverrides :: NSViewControllerPresentationAnimatorOverrides
defaultNSViewControllerPresentationAnimatorOverrides = NSViewControllerPresentationAnimatorOverrides
  { _animatePresentationOfViewController_fromViewController = Nothing
  , _animateDismissalOfViewController_fromViewController = Nothing
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
{-# NOINLINE nsViewControllerPresentationAnimatorDelegateClass #-}
nsViewControllerPresentationAnimatorDelegateClass :: Class
nsViewControllerPresentationAnimatorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSViewControllerPresentationAnimator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_animatePresentationOfViewController_fromViewController = unSelector (mkSelector "animatePresentationOfViewController:fromViewController:")
      sel_animateDismissalOfViewController_fromViewController = unSelector (mkSelector "animateDismissalOfViewController:fromViewController:")
  -- animatePresentationOfViewController:fromViewController:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSViewControllerPresentationAnimatorOverrides
    case _animatePresentationOfViewController_fromViewController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "animatePresentationOfViewController:fromViewController:" "v@:@@" stub_0

  -- animateDismissalOfViewController:fromViewController:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSViewControllerPresentationAnimatorOverrides
    case _animateDismissalOfViewController_fromViewController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "animateDismissalOfViewController:fromViewController:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSViewControllerPresentationAnimatorOverrides
    if queriedSel == sel_animatePresentationOfViewController_fromViewController then pure (maybe 0 (const 1) (_animatePresentationOfViewController_fromViewController rec_))
    else if queriedSel == sel_animateDismissalOfViewController_fromViewController then pure (maybe 0 (const 1) (_animateDismissalOfViewController_fromViewController rec_))
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
newNSViewControllerPresentationAnimator :: NSViewControllerPresentationAnimatorOverrides -> IO RawId
newNSViewControllerPresentationAnimator overrides = do
  inst <- class_createInstance nsViewControllerPresentationAnimatorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
