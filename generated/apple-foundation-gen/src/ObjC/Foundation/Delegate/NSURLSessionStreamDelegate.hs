{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLSessionStreamDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLSessionStreamDelegate defaultNSURLSessionStreamDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLSessionStreamDelegate
  ( NSURLSessionStreamDelegateOverrides(..)
  , defaultNSURLSessionStreamDelegateOverrides
  , newNSURLSessionStreamDelegate
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

-- | Overrides record for @\@protocol NSURLSessionStreamDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLSessionStreamDelegateOverrides = NSURLSessionStreamDelegateOverrides
  { _urlSession_readClosedForStreamTask :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlSession_writeClosedForStreamTask :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlSession_betterRouteDiscoveredForStreamTask :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlSession_streamTask_didBecomeInputStream_outputStream :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLSessionStreamDelegateOverrides :: NSURLSessionStreamDelegateOverrides
defaultNSURLSessionStreamDelegateOverrides = NSURLSessionStreamDelegateOverrides
  { _urlSession_readClosedForStreamTask = Nothing
  , _urlSession_writeClosedForStreamTask = Nothing
  , _urlSession_betterRouteDiscoveredForStreamTask = Nothing
  , _urlSession_streamTask_didBecomeInputStream_outputStream = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlSessionStreamDelegateDelegateClass #-}
nsurlSessionStreamDelegateDelegateClass :: Class
nsurlSessionStreamDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLSessionStreamDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlSession_readClosedForStreamTask = unSelector (mkSelector "URLSession:readClosedForStreamTask:")
      sel_urlSession_writeClosedForStreamTask = unSelector (mkSelector "URLSession:writeClosedForStreamTask:")
      sel_urlSession_betterRouteDiscoveredForStreamTask = unSelector (mkSelector "URLSession:betterRouteDiscoveredForStreamTask:")
      sel_urlSession_streamTask_didBecomeInputStream_outputStream = unSelector (mkSelector "URLSession:streamTask:didBecomeInputStream:outputStream:")
  -- URLSession:readClosedForStreamTask:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionStreamDelegateOverrides
    case _urlSession_readClosedForStreamTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLSession:readClosedForStreamTask:" "v@:@@" stub_0

  -- URLSession:writeClosedForStreamTask:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionStreamDelegateOverrides
    case _urlSession_writeClosedForStreamTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLSession:writeClosedForStreamTask:" "v@:@@" stub_1

  -- URLSession:betterRouteDiscoveredForStreamTask:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionStreamDelegateOverrides
    case _urlSession_betterRouteDiscoveredForStreamTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLSession:betterRouteDiscoveredForStreamTask:" "v@:@@" stub_2

  -- URLSession:streamTask:didBecomeInputStream:outputStream:
  stub_3 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionStreamDelegateOverrides
    case _urlSession_streamTask_didBecomeInputStream_outputStream rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "URLSession:streamTask:didBecomeInputStream:outputStream:" "v@:@@@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionStreamDelegateOverrides
    if queriedSel == sel_urlSession_readClosedForStreamTask then pure (maybe 0 (const 1) (_urlSession_readClosedForStreamTask rec_))
    else if queriedSel == sel_urlSession_writeClosedForStreamTask then pure (maybe 0 (const 1) (_urlSession_writeClosedForStreamTask rec_))
    else if queriedSel == sel_urlSession_betterRouteDiscoveredForStreamTask then pure (maybe 0 (const 1) (_urlSession_betterRouteDiscoveredForStreamTask rec_))
    else if queriedSel == sel_urlSession_streamTask_didBecomeInputStream_outputStream then pure (maybe 0 (const 1) (_urlSession_streamTask_didBecomeInputStream_outputStream rec_))
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
newNSURLSessionStreamDelegate :: NSURLSessionStreamDelegateOverrides -> IO RawId
newNSURLSessionStreamDelegate overrides = do
  inst <- class_createInstance nsurlSessionStreamDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
