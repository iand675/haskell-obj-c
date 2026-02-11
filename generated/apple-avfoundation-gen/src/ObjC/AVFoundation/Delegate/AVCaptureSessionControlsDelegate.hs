{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCaptureSessionControlsDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVCaptureSessionControlsDelegate defaultAVCaptureSessionControlsDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVCaptureSessionControlsDelegate
  ( AVCaptureSessionControlsDelegateOverrides(..)
  , defaultAVCaptureSessionControlsDelegateOverrides
  , newAVCaptureSessionControlsDelegate
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

-- | Overrides record for @\@protocol AVCaptureSessionControlsDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCaptureSessionControlsDelegateOverrides = AVCaptureSessionControlsDelegateOverrides
  { _sessionControlsDidBecomeActive :: !(Maybe (RawId -> IO ()))
  , _sessionControlsWillEnterFullscreenAppearance :: !(Maybe (RawId -> IO ()))
  , _sessionControlsWillExitFullscreenAppearance :: !(Maybe (RawId -> IO ()))
  , _sessionControlsDidBecomeInactive :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCaptureSessionControlsDelegateOverrides :: AVCaptureSessionControlsDelegateOverrides
defaultAVCaptureSessionControlsDelegateOverrides = AVCaptureSessionControlsDelegateOverrides
  { _sessionControlsDidBecomeActive = Nothing
  , _sessionControlsWillEnterFullscreenAppearance = Nothing
  , _sessionControlsWillExitFullscreenAppearance = Nothing
  , _sessionControlsDidBecomeInactive = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avCaptureSessionControlsDelegateDelegateClass #-}
avCaptureSessionControlsDelegateDelegateClass :: Class
avCaptureSessionControlsDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCaptureSessionControlsDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sessionControlsDidBecomeActive = unSelector (mkSelector "sessionControlsDidBecomeActive:")
      sel_sessionControlsWillEnterFullscreenAppearance = unSelector (mkSelector "sessionControlsWillEnterFullscreenAppearance:")
      sel_sessionControlsWillExitFullscreenAppearance = unSelector (mkSelector "sessionControlsWillExitFullscreenAppearance:")
      sel_sessionControlsDidBecomeInactive = unSelector (mkSelector "sessionControlsDidBecomeInactive:")
  -- sessionControlsDidBecomeActive:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionControlsDelegateOverrides
    case _sessionControlsDidBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionControlsDidBecomeActive:" "v@:@" stub_0

  -- sessionControlsWillEnterFullscreenAppearance:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionControlsDelegateOverrides
    case _sessionControlsWillEnterFullscreenAppearance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionControlsWillEnterFullscreenAppearance:" "v@:@" stub_1

  -- sessionControlsWillExitFullscreenAppearance:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionControlsDelegateOverrides
    case _sessionControlsWillExitFullscreenAppearance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionControlsWillExitFullscreenAppearance:" "v@:@" stub_2

  -- sessionControlsDidBecomeInactive:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionControlsDelegateOverrides
    case _sessionControlsDidBecomeInactive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionControlsDidBecomeInactive:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionControlsDelegateOverrides
    if queriedSel == sel_sessionControlsDidBecomeActive then pure (maybe 0 (const 1) (_sessionControlsDidBecomeActive rec_))
    else if queriedSel == sel_sessionControlsWillEnterFullscreenAppearance then pure (maybe 0 (const 1) (_sessionControlsWillEnterFullscreenAppearance rec_))
    else if queriedSel == sel_sessionControlsWillExitFullscreenAppearance then pure (maybe 0 (const 1) (_sessionControlsWillExitFullscreenAppearance rec_))
    else if queriedSel == sel_sessionControlsDidBecomeInactive then pure (maybe 0 (const 1) (_sessionControlsDidBecomeInactive rec_))
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
newAVCaptureSessionControlsDelegate :: AVCaptureSessionControlsDelegateOverrides -> IO RawId
newAVCaptureSessionControlsDelegate overrides = do
  inst <- class_createInstance avCaptureSessionControlsDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
