{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPageControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSPageControllerDelegate defaultNSPageControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPageControllerDelegate
  ( NSPageControllerDelegateOverrides(..)
  , defaultNSPageControllerDelegateOverrides
  , newNSPageControllerDelegate
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

-- | Overrides record for @\@protocol NSPageControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPageControllerDelegateOverrides = NSPageControllerDelegateOverrides
  { _pageController_identifierForObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _pageController_viewControllerForIdentifier :: !(Maybe (RawId -> RawId -> IO RawId))
  , _pageController_prepareViewController_withObject :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _pageController_didTransitionToObject :: !(Maybe (RawId -> RawId -> IO ()))
  , _pageControllerWillStartLiveTransition :: !(Maybe (RawId -> IO ()))
  , _pageControllerDidEndLiveTransition :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPageControllerDelegateOverrides :: NSPageControllerDelegateOverrides
defaultNSPageControllerDelegateOverrides = NSPageControllerDelegateOverrides
  { _pageController_identifierForObject = Nothing
  , _pageController_viewControllerForIdentifier = Nothing
  , _pageController_prepareViewController_withObject = Nothing
  , _pageController_didTransitionToObject = Nothing
  , _pageControllerWillStartLiveTransition = Nothing
  , _pageControllerDidEndLiveTransition = Nothing
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
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPageControllerDelegateDelegateClass #-}
nsPageControllerDelegateDelegateClass :: Class
nsPageControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPageControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pageController_identifierForObject = unSelector (mkSelector "pageController:identifierForObject:")
      sel_pageController_viewControllerForIdentifier = unSelector (mkSelector "pageController:viewControllerForIdentifier:")
      sel_pageController_prepareViewController_withObject = unSelector (mkSelector "pageController:prepareViewController:withObject:")
      sel_pageController_didTransitionToObject = unSelector (mkSelector "pageController:didTransitionToObject:")
      sel_pageControllerWillStartLiveTransition = unSelector (mkSelector "pageControllerWillStartLiveTransition:")
      sel_pageControllerDidEndLiveTransition = unSelector (mkSelector "pageControllerDidEndLiveTransition:")
  -- pageController:identifierForObject:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageController_identifierForObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pageController:identifierForObject:" "@@:@@" stub_0

  -- pageController:viewControllerForIdentifier:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageController_viewControllerForIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pageController:viewControllerForIdentifier:" "@@:@@" stub_1

  -- pageController:prepareViewController:withObject:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageController_prepareViewController_withObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pageController:prepareViewController:withObject:" "v@:@@@" stub_2

  -- pageController:didTransitionToObject:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageController_didTransitionToObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pageController:didTransitionToObject:" "v@:@@" stub_3

  -- pageControllerWillStartLiveTransition:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageControllerWillStartLiveTransition rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageControllerWillStartLiveTransition:" "v@:@" stub_4

  -- pageControllerDidEndLiveTransition:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    case _pageControllerDidEndLiveTransition rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pageControllerDidEndLiveTransition:" "v@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPageControllerDelegateOverrides
    if queriedSel == sel_pageController_identifierForObject then pure (maybe 0 (const 1) (_pageController_identifierForObject rec_))
    else if queriedSel == sel_pageController_viewControllerForIdentifier then pure (maybe 0 (const 1) (_pageController_viewControllerForIdentifier rec_))
    else if queriedSel == sel_pageController_prepareViewController_withObject then pure (maybe 0 (const 1) (_pageController_prepareViewController_withObject rec_))
    else if queriedSel == sel_pageController_didTransitionToObject then pure (maybe 0 (const 1) (_pageController_didTransitionToObject rec_))
    else if queriedSel == sel_pageControllerWillStartLiveTransition then pure (maybe 0 (const 1) (_pageControllerWillStartLiveTransition rec_))
    else if queriedSel == sel_pageControllerDidEndLiveTransition then pure (maybe 0 (const 1) (_pageControllerDidEndLiveTransition rec_))
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
newNSPageControllerDelegate :: NSPageControllerDelegateOverrides -> IO RawId
newNSPageControllerDelegate overrides = do
  inst <- class_createInstance nsPageControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
