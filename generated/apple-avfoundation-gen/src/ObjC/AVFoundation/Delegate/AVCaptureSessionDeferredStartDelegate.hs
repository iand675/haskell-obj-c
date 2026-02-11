{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCaptureSessionDeferredStartDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVCaptureSessionDeferredStartDelegate defaultAVCaptureSessionDeferredStartDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVCaptureSessionDeferredStartDelegate
  ( AVCaptureSessionDeferredStartDelegateOverrides(..)
  , defaultAVCaptureSessionDeferredStartDelegateOverrides
  , newAVCaptureSessionDeferredStartDelegate
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

-- | Overrides record for @\@protocol AVCaptureSessionDeferredStartDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCaptureSessionDeferredStartDelegateOverrides = AVCaptureSessionDeferredStartDelegateOverrides
  { _sessionWillRunDeferredStart :: !(Maybe (RawId -> IO ()))
  , _sessionDidRunDeferredStart :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCaptureSessionDeferredStartDelegateOverrides :: AVCaptureSessionDeferredStartDelegateOverrides
defaultAVCaptureSessionDeferredStartDelegateOverrides = AVCaptureSessionDeferredStartDelegateOverrides
  { _sessionWillRunDeferredStart = Nothing
  , _sessionDidRunDeferredStart = Nothing
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
{-# NOINLINE avCaptureSessionDeferredStartDelegateDelegateClass #-}
avCaptureSessionDeferredStartDelegateDelegateClass :: Class
avCaptureSessionDeferredStartDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCaptureSessionDeferredStartDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sessionWillRunDeferredStart = unSelector (mkSelector "sessionWillRunDeferredStart:")
      sel_sessionDidRunDeferredStart = unSelector (mkSelector "sessionDidRunDeferredStart:")
  -- sessionWillRunDeferredStart:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionDeferredStartDelegateOverrides
    case _sessionWillRunDeferredStart rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionWillRunDeferredStart:" "v@:@" stub_0

  -- sessionDidRunDeferredStart:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionDeferredStartDelegateOverrides
    case _sessionDidRunDeferredStart rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "sessionDidRunDeferredStart:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureSessionDeferredStartDelegateOverrides
    if queriedSel == sel_sessionWillRunDeferredStart then pure (maybe 0 (const 1) (_sessionWillRunDeferredStart rec_))
    else if queriedSel == sel_sessionDidRunDeferredStart then pure (maybe 0 (const 1) (_sessionDidRunDeferredStart rec_))
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
newAVCaptureSessionDeferredStartDelegate :: AVCaptureSessionDeferredStartDelegateOverrides -> IO RawId
newAVCaptureSessionDeferredStartDelegate overrides = do
  inst <- class_createInstance avCaptureSessionDeferredStartDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
