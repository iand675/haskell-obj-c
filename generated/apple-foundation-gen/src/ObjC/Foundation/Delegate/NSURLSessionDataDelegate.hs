{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLSessionDataDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLSessionDataDelegate defaultNSURLSessionDataDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLSessionDataDelegate
  ( NSURLSessionDataDelegateOverrides(..)
  , defaultNSURLSessionDataDelegateOverrides
  , newNSURLSessionDataDelegate
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

-- | Overrides record for @\@protocol NSURLSessionDataDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLSessionDataDelegateOverrides = NSURLSessionDataDelegateOverrides
  { _urlSession_dataTask_didBecomeDownloadTask :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_dataTask_didBecomeStreamTask :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_dataTask_didReceiveData :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLSessionDataDelegateOverrides :: NSURLSessionDataDelegateOverrides
defaultNSURLSessionDataDelegateOverrides = NSURLSessionDataDelegateOverrides
  { _urlSession_dataTask_didBecomeDownloadTask = Nothing
  , _urlSession_dataTask_didBecomeStreamTask = Nothing
  , _urlSession_dataTask_didReceiveData = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlSessionDataDelegateDelegateClass #-}
nsurlSessionDataDelegateDelegateClass :: Class
nsurlSessionDataDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLSessionDataDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlSession_dataTask_didBecomeDownloadTask = unSelector (mkSelector "URLSession:dataTask:didBecomeDownloadTask:")
      sel_urlSession_dataTask_didBecomeStreamTask = unSelector (mkSelector "URLSession:dataTask:didBecomeStreamTask:")
      sel_urlSession_dataTask_didReceiveData = unSelector (mkSelector "URLSession:dataTask:didReceiveData:")
  -- URLSession:dataTask:didBecomeDownloadTask:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDataDelegateOverrides
    case _urlSession_dataTask_didBecomeDownloadTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:dataTask:didBecomeDownloadTask:" "v@:@@@" stub_0

  -- URLSession:dataTask:didBecomeStreamTask:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDataDelegateOverrides
    case _urlSession_dataTask_didBecomeStreamTask rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:dataTask:didBecomeStreamTask:" "v@:@@@" stub_1

  -- URLSession:dataTask:didReceiveData:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDataDelegateOverrides
    case _urlSession_dataTask_didReceiveData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:dataTask:didReceiveData:" "v@:@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLSessionDataDelegateOverrides
    if queriedSel == sel_urlSession_dataTask_didBecomeDownloadTask then pure (maybe 0 (const 1) (_urlSession_dataTask_didBecomeDownloadTask rec_))
    else if queriedSel == sel_urlSession_dataTask_didBecomeStreamTask then pure (maybe 0 (const 1) (_urlSession_dataTask_didBecomeStreamTask rec_))
    else if queriedSel == sel_urlSession_dataTask_didReceiveData then pure (maybe 0 (const 1) (_urlSession_dataTask_didReceiveData rec_))
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
newNSURLSessionDataDelegate :: NSURLSessionDataDelegateOverrides -> IO RawId
newNSURLSessionDataDelegate overrides = do
  inst <- class_createInstance nsurlSessionDataDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
