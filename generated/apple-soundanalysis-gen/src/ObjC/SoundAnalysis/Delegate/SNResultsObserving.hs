{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SNResultsObserving@.
--
-- Usage:
--
-- @
-- delegate <- newSNResultsObserving defaultSNResultsObservingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SoundAnalysis.Delegate.SNResultsObserving
  ( SNResultsObservingOverrides(..)
  , defaultSNResultsObservingOverrides
  , newSNResultsObserving
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

-- | Overrides record for @\@protocol SNResultsObserving@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SNResultsObservingOverrides = SNResultsObservingOverrides
  { _request_didProduceResult :: !(Maybe (RawId -> RawId -> IO ()))
  , _request_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _requestDidComplete :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSNResultsObservingOverrides :: SNResultsObservingOverrides
defaultSNResultsObservingOverrides = SNResultsObservingOverrides
  { _request_didProduceResult = Nothing
  , _request_didFailWithError = Nothing
  , _requestDidComplete = Nothing
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
{-# NOINLINE snResultsObservingDelegateClass #-}
snResultsObservingDelegateClass :: Class
snResultsObservingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSNResultsObserving" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_request_didProduceResult = unSelector (mkSelector "request:didProduceResult:")
      sel_request_didFailWithError = unSelector (mkSelector "request:didFailWithError:")
      sel_requestDidComplete = unSelector (mkSelector "requestDidComplete:")
  -- request:didProduceResult:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SNResultsObservingOverrides
    case _request_didProduceResult rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "request:didProduceResult:" "v@:@@" stub_0

  -- request:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SNResultsObservingOverrides
    case _request_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "request:didFailWithError:" "v@:@@" stub_1

  -- requestDidComplete:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SNResultsObservingOverrides
    case _requestDidComplete rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "requestDidComplete:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SNResultsObservingOverrides
    if queriedSel == sel_request_didProduceResult then pure (maybe 0 (const 1) (_request_didProduceResult rec_))
    else if queriedSel == sel_request_didFailWithError then pure (maybe 0 (const 1) (_request_didFailWithError rec_))
    else if queriedSel == sel_requestDidComplete then pure (maybe 0 (const 1) (_requestDidComplete rec_))
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
newSNResultsObserving :: SNResultsObservingOverrides -> IO RawId
newSNResultsObserving overrides = do
  inst <- class_createInstance snResultsObservingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
