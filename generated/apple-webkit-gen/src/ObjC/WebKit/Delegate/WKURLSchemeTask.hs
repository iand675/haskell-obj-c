{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WKURLSchemeTask@.
--
-- Usage:
--
-- @
-- delegate <- newWKURLSchemeTask defaultWKURLSchemeTaskOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WKURLSchemeTask
  ( WKURLSchemeTaskOverrides(..)
  , defaultWKURLSchemeTaskOverrides
  , newWKURLSchemeTask
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

-- | Overrides record for @\@protocol WKURLSchemeTask@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WKURLSchemeTaskOverrides = WKURLSchemeTaskOverrides
  { _didReceiveResponse :: !(Maybe (RawId -> IO ()))
  , _didReceiveData :: !(Maybe (RawId -> IO ()))
  , _didFinish :: !(Maybe (IO ()))
  , _didFailWithError :: !(Maybe (RawId -> IO ()))
  , _request :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultWKURLSchemeTaskOverrides :: WKURLSchemeTaskOverrides
defaultWKURLSchemeTaskOverrides = WKURLSchemeTaskOverrides
  { _didReceiveResponse = Nothing
  , _didReceiveData = Nothing
  , _didFinish = Nothing
  , _didFailWithError = Nothing
  , _request = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE wkurlSchemeTaskDelegateClass #-}
wkurlSchemeTaskDelegateClass :: Class
wkurlSchemeTaskDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWKURLSchemeTask" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_didReceiveResponse = unSelector (mkSelector "didReceiveResponse:")
      sel_didReceiveData = unSelector (mkSelector "didReceiveData:")
      sel_didFinish = unSelector (mkSelector "didFinish")
      sel_didFailWithError = unSelector (mkSelector "didFailWithError:")
      sel_request = unSelector (mkSelector "request")
  -- didReceiveResponse:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    case _didReceiveResponse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didReceiveResponse:" "v@:@" stub_0

  -- didReceiveData:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    case _didReceiveData rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didReceiveData:" "v@:@" stub_1

  -- didFinish
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    case _didFinish rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "didFinish" "v@:" stub_2

  -- didFailWithError:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    case _didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didFailWithError:" "v@:@" stub_3

  -- request
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    case _request rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "request" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WKURLSchemeTaskOverrides
    if queriedSel == sel_didReceiveResponse then pure (maybe 0 (const 1) (_didReceiveResponse rec_))
    else if queriedSel == sel_didReceiveData then pure (maybe 0 (const 1) (_didReceiveData rec_))
    else if queriedSel == sel_didFinish then pure (maybe 0 (const 1) (_didFinish rec_))
    else if queriedSel == sel_didFailWithError then pure (maybe 0 (const 1) (_didFailWithError rec_))
    else if queriedSel == sel_request then pure (maybe 0 (const 1) (_request rec_))
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
newWKURLSchemeTask :: WKURLSchemeTaskOverrides -> IO RawId
newWKURLSchemeTask overrides = do
  inst <- class_createInstance wkurlSchemeTaskDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
