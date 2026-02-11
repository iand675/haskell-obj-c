{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLProtocolClient@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLProtocolClient defaultNSURLProtocolClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLProtocolClient
  ( NSURLProtocolClientOverrides(..)
  , defaultNSURLProtocolClientOverrides
  , newNSURLProtocolClient
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

-- | Overrides record for @\@protocol NSURLProtocolClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLProtocolClientOverrides = NSURLProtocolClientOverrides
  { _urlProtocol_wasRedirectedToRequest_redirectResponse :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlProtocol_cachedResponseIsValid :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlProtocol_didLoadData :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlProtocolDidFinishLoading :: !(Maybe (RawId -> IO ()))
  , _urlProtocol_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlProtocol_didReceiveAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlProtocol_didCancelAuthenticationChallenge :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLProtocolClientOverrides :: NSURLProtocolClientOverrides
defaultNSURLProtocolClientOverrides = NSURLProtocolClientOverrides
  { _urlProtocol_wasRedirectedToRequest_redirectResponse = Nothing
  , _urlProtocol_cachedResponseIsValid = Nothing
  , _urlProtocol_didLoadData = Nothing
  , _urlProtocolDidFinishLoading = Nothing
  , _urlProtocol_didFailWithError = Nothing
  , _urlProtocol_didReceiveAuthenticationChallenge = Nothing
  , _urlProtocol_didCancelAuthenticationChallenge = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlProtocolClientDelegateClass #-}
nsurlProtocolClientDelegateClass :: Class
nsurlProtocolClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLProtocolClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlProtocol_wasRedirectedToRequest_redirectResponse = unSelector (mkSelector "URLProtocol:wasRedirectedToRequest:redirectResponse:")
      sel_urlProtocol_cachedResponseIsValid = unSelector (mkSelector "URLProtocol:cachedResponseIsValid:")
      sel_urlProtocol_didLoadData = unSelector (mkSelector "URLProtocol:didLoadData:")
      sel_urlProtocolDidFinishLoading = unSelector (mkSelector "URLProtocolDidFinishLoading:")
      sel_urlProtocol_didFailWithError = unSelector (mkSelector "URLProtocol:didFailWithError:")
      sel_urlProtocol_didReceiveAuthenticationChallenge = unSelector (mkSelector "URLProtocol:didReceiveAuthenticationChallenge:")
      sel_urlProtocol_didCancelAuthenticationChallenge = unSelector (mkSelector "URLProtocol:didCancelAuthenticationChallenge:")
  -- URLProtocol:wasRedirectedToRequest:redirectResponse:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_wasRedirectedToRequest_redirectResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLProtocol:wasRedirectedToRequest:redirectResponse:" "v@:@@@" stub_0

  -- URLProtocol:cachedResponseIsValid:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_cachedResponseIsValid rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLProtocol:cachedResponseIsValid:" "v@:@@" stub_1

  -- URLProtocol:didLoadData:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_didLoadData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLProtocol:didLoadData:" "v@:@@" stub_2

  -- URLProtocolDidFinishLoading:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocolDidFinishLoading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "URLProtocolDidFinishLoading:" "v@:@" stub_3

  -- URLProtocol:didFailWithError:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLProtocol:didFailWithError:" "v@:@@" stub_4

  -- URLProtocol:didReceiveAuthenticationChallenge:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_didReceiveAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLProtocol:didReceiveAuthenticationChallenge:" "v@:@@" stub_5

  -- URLProtocol:didCancelAuthenticationChallenge:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    case _urlProtocol_didCancelAuthenticationChallenge rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLProtocol:didCancelAuthenticationChallenge:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLProtocolClientOverrides
    if queriedSel == sel_urlProtocol_wasRedirectedToRequest_redirectResponse then pure (maybe 0 (const 1) (_urlProtocol_wasRedirectedToRequest_redirectResponse rec_))
    else if queriedSel == sel_urlProtocol_cachedResponseIsValid then pure (maybe 0 (const 1) (_urlProtocol_cachedResponseIsValid rec_))
    else if queriedSel == sel_urlProtocol_didLoadData then pure (maybe 0 (const 1) (_urlProtocol_didLoadData rec_))
    else if queriedSel == sel_urlProtocolDidFinishLoading then pure (maybe 0 (const 1) (_urlProtocolDidFinishLoading rec_))
    else if queriedSel == sel_urlProtocol_didFailWithError then pure (maybe 0 (const 1) (_urlProtocol_didFailWithError rec_))
    else if queriedSel == sel_urlProtocol_didReceiveAuthenticationChallenge then pure (maybe 0 (const 1) (_urlProtocol_didReceiveAuthenticationChallenge rec_))
    else if queriedSel == sel_urlProtocol_didCancelAuthenticationChallenge then pure (maybe 0 (const 1) (_urlProtocol_didCancelAuthenticationChallenge rec_))
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
newNSURLProtocolClient :: NSURLProtocolClientOverrides -> IO RawId
newNSURLProtocolClient overrides = do
  inst <- class_createInstance nsurlProtocolClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
