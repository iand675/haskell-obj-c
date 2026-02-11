{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSURLHandleClient@.
--
-- Usage:
--
-- @
-- delegate <- newNSURLHandleClient defaultNSURLHandleClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSURLHandleClient
  ( NSURLHandleClientOverrides(..)
  , defaultNSURLHandleClientOverrides
  , newNSURLHandleClient
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

-- | Overrides record for @\@protocol NSURLHandleClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSURLHandleClientOverrides = NSURLHandleClientOverrides
  { _urlHandle_resourceDataDidBecomeAvailable :: !(Maybe (RawId -> RawId -> IO ()))
  , _urlHandleResourceDidBeginLoading :: !(Maybe (RawId -> IO ()))
  , _urlHandleResourceDidFinishLoading :: !(Maybe (RawId -> IO ()))
  , _urlHandleResourceDidCancelLoading :: !(Maybe (RawId -> IO ()))
  , _urlHandle_resourceDidFailLoadingWithReason :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSURLHandleClientOverrides :: NSURLHandleClientOverrides
defaultNSURLHandleClientOverrides = NSURLHandleClientOverrides
  { _urlHandle_resourceDataDidBecomeAvailable = Nothing
  , _urlHandleResourceDidBeginLoading = Nothing
  , _urlHandleResourceDidFinishLoading = Nothing
  , _urlHandleResourceDidCancelLoading = Nothing
  , _urlHandle_resourceDidFailLoadingWithReason = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsurlHandleClientDelegateClass #-}
nsurlHandleClientDelegateClass :: Class
nsurlHandleClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSURLHandleClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlHandle_resourceDataDidBecomeAvailable = unSelector (mkSelector "URLHandle:resourceDataDidBecomeAvailable:")
      sel_urlHandleResourceDidBeginLoading = unSelector (mkSelector "URLHandleResourceDidBeginLoading:")
      sel_urlHandleResourceDidFinishLoading = unSelector (mkSelector "URLHandleResourceDidFinishLoading:")
      sel_urlHandleResourceDidCancelLoading = unSelector (mkSelector "URLHandleResourceDidCancelLoading:")
      sel_urlHandle_resourceDidFailLoadingWithReason = unSelector (mkSelector "URLHandle:resourceDidFailLoadingWithReason:")
  -- URLHandle:resourceDataDidBecomeAvailable:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    case _urlHandle_resourceDataDidBecomeAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLHandle:resourceDataDidBecomeAvailable:" "v@:@@" stub_0

  -- URLHandleResourceDidBeginLoading:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    case _urlHandleResourceDidBeginLoading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "URLHandleResourceDidBeginLoading:" "v@:@" stub_1

  -- URLHandleResourceDidFinishLoading:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    case _urlHandleResourceDidFinishLoading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "URLHandleResourceDidFinishLoading:" "v@:@" stub_2

  -- URLHandleResourceDidCancelLoading:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    case _urlHandleResourceDidCancelLoading rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "URLHandleResourceDidCancelLoading:" "v@:@" stub_3

  -- URLHandle:resourceDidFailLoadingWithReason:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    case _urlHandle_resourceDidFailLoadingWithReason rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "URLHandle:resourceDidFailLoadingWithReason:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSURLHandleClientOverrides
    if queriedSel == sel_urlHandle_resourceDataDidBecomeAvailable then pure (maybe 0 (const 1) (_urlHandle_resourceDataDidBecomeAvailable rec_))
    else if queriedSel == sel_urlHandleResourceDidBeginLoading then pure (maybe 0 (const 1) (_urlHandleResourceDidBeginLoading rec_))
    else if queriedSel == sel_urlHandleResourceDidFinishLoading then pure (maybe 0 (const 1) (_urlHandleResourceDidFinishLoading rec_))
    else if queriedSel == sel_urlHandleResourceDidCancelLoading then pure (maybe 0 (const 1) (_urlHandleResourceDidCancelLoading rec_))
    else if queriedSel == sel_urlHandle_resourceDidFailLoadingWithReason then pure (maybe 0 (const 1) (_urlHandle_resourceDidFailLoadingWithReason rec_))
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
newNSURLHandleClient :: NSURLHandleClientOverrides -> IO RawId
newNSURLHandleClient overrides = do
  inst <- class_createInstance nsurlHandleClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
