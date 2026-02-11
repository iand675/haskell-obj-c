{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKOverlayDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSKOverlayDelegate defaultSKOverlayDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.StoreKit.Delegate.SKOverlayDelegate
  ( SKOverlayDelegateOverrides(..)
  , defaultSKOverlayDelegateOverrides
  , newSKOverlayDelegate
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

-- | Overrides record for @\@protocol SKOverlayDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKOverlayDelegateOverrides = SKOverlayDelegateOverrides
  { _storeOverlay_didFailToLoadWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _storeOverlay_willStartPresentation :: !(Maybe (RawId -> RawId -> IO ()))
  , _storeOverlay_didFinishPresentation :: !(Maybe (RawId -> RawId -> IO ()))
  , _storeOverlay_willStartDismissal :: !(Maybe (RawId -> RawId -> IO ()))
  , _storeOverlay_didFinishDismissal :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSKOverlayDelegateOverrides :: SKOverlayDelegateOverrides
defaultSKOverlayDelegateOverrides = SKOverlayDelegateOverrides
  { _storeOverlay_didFailToLoadWithError = Nothing
  , _storeOverlay_willStartPresentation = Nothing
  , _storeOverlay_didFinishPresentation = Nothing
  , _storeOverlay_willStartDismissal = Nothing
  , _storeOverlay_didFinishDismissal = Nothing
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
{-# NOINLINE skOverlayDelegateDelegateClass #-}
skOverlayDelegateDelegateClass :: Class
skOverlayDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKOverlayDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_storeOverlay_didFailToLoadWithError = unSelector (mkSelector "storeOverlay:didFailToLoadWithError:")
      sel_storeOverlay_willStartPresentation = unSelector (mkSelector "storeOverlay:willStartPresentation:")
      sel_storeOverlay_didFinishPresentation = unSelector (mkSelector "storeOverlay:didFinishPresentation:")
      sel_storeOverlay_willStartDismissal = unSelector (mkSelector "storeOverlay:willStartDismissal:")
      sel_storeOverlay_didFinishDismissal = unSelector (mkSelector "storeOverlay:didFinishDismissal:")
  -- storeOverlay:didFailToLoadWithError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    case _storeOverlay_didFailToLoadWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "storeOverlay:didFailToLoadWithError:" "v@:@@" stub_0

  -- storeOverlay:willStartPresentation:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    case _storeOverlay_willStartPresentation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "storeOverlay:willStartPresentation:" "v@:@@" stub_1

  -- storeOverlay:didFinishPresentation:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    case _storeOverlay_didFinishPresentation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "storeOverlay:didFinishPresentation:" "v@:@@" stub_2

  -- storeOverlay:willStartDismissal:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    case _storeOverlay_willStartDismissal rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "storeOverlay:willStartDismissal:" "v@:@@" stub_3

  -- storeOverlay:didFinishDismissal:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    case _storeOverlay_didFinishDismissal rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "storeOverlay:didFinishDismissal:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKOverlayDelegateOverrides
    if queriedSel == sel_storeOverlay_didFailToLoadWithError then pure (maybe 0 (const 1) (_storeOverlay_didFailToLoadWithError rec_))
    else if queriedSel == sel_storeOverlay_willStartPresentation then pure (maybe 0 (const 1) (_storeOverlay_willStartPresentation rec_))
    else if queriedSel == sel_storeOverlay_didFinishPresentation then pure (maybe 0 (const 1) (_storeOverlay_didFinishPresentation rec_))
    else if queriedSel == sel_storeOverlay_willStartDismissal then pure (maybe 0 (const 1) (_storeOverlay_willStartDismissal rec_))
    else if queriedSel == sel_storeOverlay_didFinishDismissal then pure (maybe 0 (const 1) (_storeOverlay_didFinishDismissal rec_))
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
newSKOverlayDelegate :: SKOverlayDelegateOverrides -> IO RawId
newSKOverlayDelegate overrides = do
  inst <- class_createInstance skOverlayDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
